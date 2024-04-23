use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
  parse::Parse, parse_quote, punctuated::Punctuated, Attribute, DeriveInput,
  Field, Ident, Token, Type,
};

enum FieldKind {
  Normal,
  Optional,
  Repeated,
}

struct BuilderField {
  field: Field,
  input_ty: Type,
  method_name: Ident,
  kind: FieldKind,
}

impl TryFrom<Field> for BuilderField {
  type Error = syn::Error;
  fn try_from(field: Field) -> Result<Self, Self::Error> {
    builder_attrs_validate(&field.attrs, &["each"])?;

    let mut kind = FieldKind::Normal;
    let mut input_ty = field.ty.clone();
    let mut method_name = field.ident.clone().ok_or_else(|| {
      syn::Error::new_spanned(&field, "named fields are required")
    })?;

    let repeated = Self::parse_repeated_type(&field)?;
    let optional = Self::parse_option_type(&field.ty);

    match (optional, repeated) {
      (None, None) => {}
      (Some(ty), None) => {
        kind = FieldKind::Optional;
        input_ty = ty;
      }
      (None, Some((ty, name))) => {
        kind = FieldKind::Repeated;
        input_ty = ty;
        method_name = name;
      }
      (Some(_), Some(_)) => {
        return Err(syn::Error::new_spanned(
          &field,
          "fields cannot be both optional and repeated",
        ))
      }
    };

    Ok(Self {
      field,
      input_ty,
      method_name,
      kind,
    })
  }
}

impl BuilderField {
  // if ty is an optional type Option<T>, return Some(T). Otherwise return None.
  fn parse_option_type(ty: &Type) -> Option<Type> {
    parse_generic_type(ty, "Option")
  }

  fn parse_repeated_type(
    field: &Field,
  ) -> Result<Option<(Type, Ident)>, syn::Error> {
    let each_names: Vec<syn::LitStr> = builder_attr_get(&field.attrs, "each")?;
    if each_names.len() > 1 {
      return Err(syn::Error::new_spanned(
        each_names.first().unwrap(),
        "multiple 'each' attributes are not allowed",
      ));
    }

    let Some(each_name) = each_names.first() else {
      return Ok(None);
    };
    let method_name = Ident::new(&each_name.value(), each_name.span());
    let Some(ty) = parse_generic_type(&field.ty, "Vec") else {
      return Err(syn::Error::new_spanned(&field.ty, "expected Vec<T> type"));
    };

    Ok(Some((ty, method_name)))
  }

  fn field_name(&self) -> syn::Ident {
    // unwrap is safe because we only deal with named fields
    self.field.ident.clone().unwrap()
  }

  fn builder_method_name(&self) -> syn::Ident {
    self.method_name.clone()
  }

  fn builder_field_ty(&self) -> Type {
    let input_ty = &self.input_ty;
    match self.kind {
      FieldKind::Normal => parse_quote! { std::option::Option<#input_ty> },
      FieldKind::Optional => parse_quote! { std::option::Option<#input_ty> },
      FieldKind::Repeated => parse_quote! { std::vec::Vec<#input_ty> },
    }
  }

  fn builder_method(&self) -> TokenStream {
    let method_name = self.builder_method_name();
    let field_name = self.field_name();
    let ty = self.input_ty.clone();
    let assignment = match self.kind {
      FieldKind::Normal => {
        quote! {self.#field_name = std::option::Option::Some(value);}
      }
      FieldKind::Optional => {
        quote! {self.#field_name = std::option::Option::Some(value);}
      }
      FieldKind::Repeated => quote! {self.#field_name.push(value);},
    };

    quote! {
      pub fn #method_name(&mut self, value: #ty) -> &mut Self {
        #assignment
        self
      }
    }
  }

  fn validate(&self) -> TokenStream {
    let field_name = self.field_name();

    match self.kind {
      FieldKind::Normal => {
        let err_msg = format!("missing \"{}\" field", field_name);
        quote! {
          let std::option::Option::Some(#field_name) = self.#field_name.take() else {
            return Err(std::boxed::Box::<dyn std::error::Error>::from(#err_msg));
          };
        }
      }
      FieldKind::Optional => {
        quote! {let #field_name = self.#field_name.take();}
      }
      FieldKind::Repeated => {
        quote! {let #field_name = std::mem::take(&mut self.#field_name); }
      }
    }
  }

  fn field_initializer(&self) -> TokenStream {
    let field_name = self.field_name();
    // Default::default() works for both Option and Vec.
    quote! {
      #field_name: std::default::Default::default()
    }
  }
}

struct BuilderInput {
  name: syn::Ident,
  generics: syn::Generics,
  fields: Vec<BuilderField>,
}

impl TryFrom<DeriveInput> for BuilderInput {
  type Error = syn::Error;

  fn try_from(value: DeriveInput) -> Result<Self, Self::Error> {
    let DeriveInput {
      ident,
      generics,
      data,
      ..
    } = value;

    let struct_data = match data {
      syn::Data::Struct(data) => data,
      syn::Data::Enum(data) => {
        return Err(syn::parse::Error::new_spanned(
          data.enum_token,
          "Enums are not supported",
        ));
      }
      syn::Data::Union(data) => {
        return Err(syn::parse::Error::new_spanned(
          data.union_token,
          "Unions are not supported",
        ));
      }
    };

    let fields = match struct_data.fields {
      syn::Fields::Named(fields) => fields
        .named
        .into_iter()
        .map(BuilderField::try_from)
        .collect::<Result<Vec<_>, _>>()?,
      _ => {
        return Err(syn::parse::Error::new_spanned(
          struct_data.fields,
          "Only named fields are supported",
        ))
      }
    };

    Ok(Self {
      name: ident,
      fields,
      generics,
    })
  }
}

impl BuilderInput {
  fn constructor(&self) -> TokenStream {
    let builder_name = self.type_name();
    let generics = &self.generics;
    let name = &self.name;
    let field_initializers =
      self.fields.iter().map(|field| field.field_initializer());

    quote! {
      impl #generics #name #generics {
        pub fn builder() -> #builder_name #generics {
          #builder_name {
            #( #field_initializers ),*
          }
        }
      }
    }
  }

  fn type_data(&self) -> TokenStream {
    let name = self.type_name();
    let generics = &self.generics;
    let fields = self.fields();

    quote! {
      struct #name #generics { #fields }
    }
  }

  fn type_name(&self) -> syn::Ident {
    let name = &self.name;
    format_ident!("{name}Builder")
  }

  fn fields(&self) -> TokenStream {
    let fields = self.fields.iter().map(|field| {
      let name = &field.field_name();
      let ty = &field.builder_field_ty();
      quote! {
        #name: #ty
      }
    });

    quote! {
      #( #fields ),*
    }
  }

  fn builder_impl(&self) -> TokenStream {
    let methods = self.fields.iter().map(|f| f.builder_method());
    let name = self.type_name();
    let generics = &self.generics;
    let impl_header = quote! {
      impl #generics #name #generics
    };
    let build_method = self.build_method();

    quote! {
      #impl_header {
        #( #methods )*
        #build_method
      }
    }
  }

  fn build_method(&self) -> TokenStream {
    let name = &self.name;
    let check_fields = self.fields.iter().map(|f| f.validate());
    let init_fields = self.fields.iter().map(|field| {
      let ident = field.field_name();
      quote! {
        #ident,
      }
    });

    quote! {
      pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
        #( #check_fields )*
        std::result::Result::Ok(#name {#( #init_fields )*})
      }
    }
  }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let derive_input = syn::parse_macro_input!(input as DeriveInput);
  let builder_input = match BuilderInput::try_from(derive_input) {
    Ok(builder_input) => builder_input,
    Err(err) => return err.to_compile_error().into(),
  };

  let constructor = builder_input.constructor();
  let builder_type_data = builder_input.type_data();
  let builder_impl = builder_input.builder_impl();

  let output = quote! {
    #constructor
    #builder_type_data
    #builder_impl
  };

  output.into()
}

struct AttrListItem<V> {
  key: syn::Ident,
  _eq: Token![=],
  value: V,
}

impl<V> Parse for AttrListItem<V>
where
  V: Parse,
{
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let key = input.parse()?;
    let _eq = input.parse()?;
    let value = input.parse()?;

    Ok(Self { key, _eq, value })
  }
}

struct AttrList<V> {
  items: Punctuated<AttrListItem<V>, Token![,]>,
}

impl<V> Parse for AttrList<V>
where
  V: Parse,
{
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let items = Punctuated::parse_terminated(input)?;
    Ok(Self { items })
  }
}

fn builder_attrs_validate(
  attrs: &Vec<Attribute>,
  valid_keys: &[&str],
) -> Result<(), syn::Error> {
  for attr in attrs {
    let syn::Meta::List(syn::MetaList { path, tokens, .. }) = &attr.meta else {
      continue;
    };

    if !path.is_ident("builder") {
      continue;
    }

    let attr_list: AttrList<TokenStream> = syn::parse2(tokens.clone())?;
    for item in attr_list.items.into_iter() {
      let key = item.key.to_string();
      if !valid_keys.contains(&key.as_str()) {
        return Err(syn::Error::new_spanned(
          item.key,
          format!(
            "unrecognized key: {}, valid keys are: {:?}",
            key, valid_keys
          ),
        ));
      }
    }
  }

  Ok(())
}

fn builder_attr_get<V: Parse>(
  attrs: &Vec<Attribute>,
  key: &str,
) -> Result<Vec<V>, syn::Error> {
  let mut values = vec![];
  for attr in attrs {
    let syn::Meta::List(syn::MetaList { path, tokens, .. }) = &attr.meta else {
      continue;
    };

    if !path.is_ident("builder") {
      continue;
    }

    let attr_list: AttrList<V> = syn::parse2(tokens.clone())?;
    for item in attr_list.items.into_iter() {
      if item.key == key {
        values.push(item.value);
      }
    }
  }

  Ok(values)
}

// parses types like Vec<T>, Option<T>, etc into T
fn parse_generic_type(ty: &Type, container: &str) -> Option<Type> {
  let Type::Path(syn::TypePath { qself: None, path }) = ty else {
    return None;
  };

  let syn::Path { segments, .. } = path;

  let segment = segments.last()?;

  if segment.ident != container {
    return None;
  };

  let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
    ref args,
    ..
  }) = segment.arguments
  else {
    return None;
  };

  if args.len() != 1 {
    return None;
  }
  let Some(syn::GenericArgument::Type(ty)) = args.first() else {
    return None;
  };

  Some(ty.clone())
}
