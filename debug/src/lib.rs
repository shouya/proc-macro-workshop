use proc_macro2::TokenStream;
use quote::quote;
use syn::{
  spanned::Spanned, visit::Visit, Attribute, DeriveInput, Generics, Ident, Type,
};

struct DebugField {
  name: Ident,
  custom_format: Option<String>,
  #[allow(unused)] // for now.
  ty: syn::Type,
}

impl DebugField {
  fn impl_debug_field(&self) -> TokenStream {
    let name = &self.name;
    let name_str =
      syn::LitStr::new(self.name.to_string().as_str(), self.name.span());

    if let Some(format) = &self.custom_format {
      quote! {
        let mut fmt = fmt.field(#name_str, &format_args!(#format, &self.#name));
      }
    } else {
      quote! {
        let mut fmt = fmt.field(#name_str, &self.#name);
      }
    }
  }
}

impl TryFrom<syn::Field> for DebugField {
  type Error = syn::Error;

  fn try_from(field: syn::Field) -> Result<Self, Self::Error> {
    let name = field.ident.unwrap();
    let ty = field.ty;
    let custom_format = parse_custom_format(&field.attrs)?;

    Ok(DebugField {
      name,
      ty,
      custom_format,
    })
  }
}

struct DebugInput {
  name: Ident,
  generics: Generics,
  generics_sans_bounds: Generics,
  fields: Vec<DebugField>,
}

impl TryFrom<DeriveInput> for DebugInput {
  type Error = syn::Error;

  fn try_from(input: DeriveInput) -> Result<Self, Self::Error> {
    let span = input.span();
    let name = input.ident;
    let generics = input.generics;
    let generics_sans_bounds = remove_bounds(generics.clone());
    let syn::Data::Struct(strt) = input.data else {
      return Err(syn::Error::new(
        span,
        "CustomDebug can only be derived for structs",
      ));
    };

    let syn::Fields::Named(fields) = strt.fields else {
      return Err(syn::Error::new(
        span,
        "CustomDebug can only be derived for structs with named fields",
      ));
    };

    let fields = fields
      .named
      .into_iter()
      .map(|f| f.try_into())
      .collect::<Result<Vec<DebugField>, _>>()?;

    Ok(DebugInput {
      name,
      fields,
      generics,
      generics_sans_bounds,
    })
  }
}

impl DebugInput {
  fn impl_debug(&self) -> TokenStream {
    let name = &self.name;
    let name_str =
      syn::LitStr::new(self.name.to_string().as_str(), self.name.span());
    let generics = &self.generics;
    let generics_sans_bounds = &self.generics_sans_bounds;
    let generic_bounds = self.generic_bounds();
    let fmt_fields = self.fields.iter().map(|f| f.impl_debug_field());

    quote! {
      impl #generics std::fmt::Debug for #name #generics_sans_bounds
        where #(#generic_bounds),*
      {
        fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          let mut fmt = fmt.debug_struct(#name_str);
          #( #fmt_fields );*
          fmt.finish()
        }
      }
    }
  }

  fn generic_bounds(&self) -> Vec<TokenStream> {
    let type_params: Vec<_> = self
      .generics
      .type_params()
      .map(|p| p.ident.clone())
      .collect();

    let type_args = self
      .fields
      .iter()
      .flat_map(|f| nested_generic_args(&f.ty, &type_params));

    let bounds = type_args.map(|ty| {
      quote! {
        #ty: std::fmt::Debug
      }
    });

    bounds.collect()
  }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse_macro_input!(input as DeriveInput);
  let input = match DebugInput::try_from(input) {
    Ok(input) => input,
    Err(e) => return e.to_compile_error().into(),
  };

  let output = input.impl_debug();
  output.into()
}

fn parse_custom_format(
  attrs: &Vec<Attribute>,
) -> Result<Option<String>, syn::Error> {
  for attr in attrs {
    let syn::Meta::NameValue(name_value) = &attr.meta else {
      continue;
    };

    if !name_value.path.is_ident("debug") {
      continue;
    }

    let syn::Expr::Lit(lit) = &name_value.value else {
      return Err(syn::Error::new(
        name_value.span(),
        "expected a string literal",
      ));
    };

    let syn::Lit::Str(lit) = &lit.lit else {
      return Err(syn::Error::new(lit.span(), "expected a string literal"));
    };

    return Ok(Some(lit.value()));
  }

  Ok(None)
}

fn remove_bounds(mut generics: Generics) -> Generics {
  for param in generics.type_params_mut() {
    param.bounds.clear();
  }

  for param in generics.lifetimes_mut() {
    param.bounds.clear();
  }

  for param in generics.const_params_mut() {
    param.eq_token = None;
    param.default = None;
  }

  generics
}

// including type itself
struct NestedGenericArgExtractor<'a> {
  type_params: &'a Vec<Ident>,
  types: Vec<Type>,
}

impl<'ast> syn::visit::Visit<'ast> for NestedGenericArgExtractor<'_> {
  fn visit_type(&mut self, node: &'ast Type) {
    let Type::Path(path) = node else {
      return;
    };

    if let Some(first_seg) = path.path.segments.first() {
      // We only collect types that are part of generic type
      // parameters. This includes formats like T, T::Value, etc as
      // long as T is a generic type parameter.
      if self.type_params.contains(&first_seg.ident) {
        self.types.push(node.clone());
      }
    }

    for seg in &path.path.segments {
      if seg.ident == "PhantomData" {
        continue;
      }

      syn::visit::visit_path_segment(self, seg);
    }
  }
}

fn nested_generic_args(ty: &Type, type_params: &Vec<Ident>) -> Vec<Type> {
  let mut extractor = NestedGenericArgExtractor {
    types: vec![],
    type_params,
  };
  extractor.visit_type(ty);
  extractor.types
}
