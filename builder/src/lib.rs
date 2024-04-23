use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_quote, DeriveInput, Field, Type};

struct BuilderField {
  field: Field,
  optional_ty: Option<Type>,
}

impl From<Field> for BuilderField {
  fn from(field: Field) -> Self {
    let optional_ty = Self::parse_option_type(&field.ty);
    Self { field, optional_ty }
  }
}

impl BuilderField {
  // if ty is an optional type Option<T>, return Some(T). Otherwise return None.
  fn parse_option_type(ty: &Type) -> Option<Type> {
    let Type::Path(syn::TypePath { qself: None, path }) = ty else {
      return None;
    };

    let syn::Path { segments, .. } = path;

    let segment = segments.last()?;

    if segment.ident != "Option" {
      return None;
    };

    let syn::PathArguments::AngleBracketed(
      syn::AngleBracketedGenericArguments { ref args, .. },
    ) = segment.arguments
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

  fn ident(&self) -> syn::Ident {
    // unwrap is safe because we only deal with named fields
    self.field.ident.clone().unwrap()
  }

  fn builder_field_ty(&self) -> Type {
    if let Some(ty) = &self.optional_ty {
      parse_quote! { Option<#ty> }
    } else {
      let ty = &self.field.ty;
      parse_quote! { Option<#ty> }
    }
  }

  fn input_ty(&self) -> Type {
    if let Some(ty) = &self.optional_ty {
      ty.clone()
    } else {
      self.field.ty.clone()
    }
  }

  fn builder_method(&self) -> TokenStream {
    let name = self.ident();
    let ty = self.input_ty();

    quote! {
      pub fn #name(&mut self, #name: #ty) -> &mut Self {
        self.#name = Some(#name);
        self
      }
    }
  }

  fn check_not_none(&self) -> TokenStream {
    let ident = self.ident();

    if self.optional_ty.is_some() {
      return quote! {
        let #ident = self.#ident.take();
      };
    }

    let ident_str = ident.to_string();
    let err_msg = format!("missing {} field", ident_str);
    quote! {
      let Some(#ident) = self.#ident.take() else {
        return Err(std::boxed::Box::<dyn std::error::Error>::from(#err_msg));
      };
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
      syn::Fields::Named(fields) => {
        fields.named.into_iter().map(BuilderField::from).collect()
      }
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
    let field_names = &self.field_names();

    quote! {
      impl #generics #name #generics {
        pub fn builder() -> #builder_name #generics {
          #builder_name {
            #( #field_names: None ),*
          }
        }
      }
    }
  }

  fn field_names(&self) -> Vec<syn::Ident> {
    self.fields.iter().map(|field| field.ident()).collect()
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
      let name = &field.ident();
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
    let check_fields = self.fields.iter().map(|f| f.check_not_none());
    let init_fields = self.fields.iter().map(|field| {
      let ident = field.ident();
      quote! {
        #ident,
      }
    });

    quote! {
      pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
        #( #check_fields )*
        Ok(#name {#( #init_fields )*})
      }
    }
  }
}

#[proc_macro_derive(Builder)]
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
