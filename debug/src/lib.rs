use proc_macro2::TokenStream;
use quote::quote;
use syn::{spanned::Spanned, Attribute, DeriveInput, Generics, Ident};

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

  fn field_bound(&self) -> TokenStream {
    let ty = &self.ty;
    quote! {
      #ty: std::fmt::Debug
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
  fields: Vec<DebugField>,
}

impl TryFrom<DeriveInput> for DebugInput {
  type Error = syn::Error;

  fn try_from(input: DeriveInput) -> Result<Self, Self::Error> {
    let span = input.span();
    let name = input.ident;
    let generics = input.generics;
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
    })
  }
}

impl DebugInput {
  fn impl_debug(&self) -> TokenStream {
    let name = &self.name;
    let name_str =
      syn::LitStr::new(self.name.to_string().as_str(), self.name.span());
    let generics = &self.generics;
    let fmt_fields = self.fields.iter().map(|f| f.impl_debug_field());
    let field_bounds = self.fields.iter().map(|f| f.field_bound());

    quote! {
      impl #generics std::fmt::Debug for #name #generics
        where #(#field_bounds),*
      {
        fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          let mut fmt = fmt.debug_struct(#name_str);
          #( #fmt_fields );*
          fmt.finish()
        }
      }
    }
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
