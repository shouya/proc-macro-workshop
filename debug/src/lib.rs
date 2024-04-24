use proc_macro2::TokenStream;
use quote::quote;
use syn::{
  spanned::Spanned, visit::Visit, Attribute, DeriveInput, Expr, Generics,
  Ident, Type,
};

struct DebugField {
  name: Ident,
  custom_format: Option<String>,
  extra_bounds: Option<TokenStream>,
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

  fn extra_bounds(&self) -> Option<TokenStream> {
    self.extra_bounds.clone()
  }
}

impl TryFrom<syn::Field> for DebugField {
  type Error = syn::Error;

  fn try_from(field: syn::Field) -> Result<Self, Self::Error> {
    let name = field.ident.unwrap();
    let ty = field.ty;
    let custom_format = parse_custom_format(&field.attrs)?;
    let extra_bounds = parse_extra_bounds(&field.attrs)?;

    Ok(DebugField {
      name,
      ty,
      custom_format,
      extra_bounds,
    })
  }
}

struct DebugInput {
  name: Ident,
  generics: Generics,
  generics_sans_bounds: Generics,
  extra_bounds: Option<TokenStream>,
  fields: Vec<DebugField>,
}

impl TryFrom<DeriveInput> for DebugInput {
  type Error = syn::Error;

  fn try_from(input: DeriveInput) -> Result<Self, Self::Error> {
    let span = input.span();
    let name = input.ident;
    let generics = input.generics;
    let extra_bounds = parse_extra_bounds(&input.attrs)?;
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
      extra_bounds,
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
    let generic_bounds = match self.generic_bounds() {
      Some(bounds) => quote! { where #bounds },
      None => quote! {},
    };
    let fmt_fields = self.fields.iter().map(|f| f.impl_debug_field());

    quote! {
      impl #generics std::fmt::Debug for #name #generics_sans_bounds
        #generic_bounds
      {
        fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          let mut fmt = fmt.debug_struct(#name_str);
          #( #fmt_fields );*
          fmt.finish()
        }
      }
    }
  }

  fn generic_bounds(&self) -> Option<TokenStream> {
    if let Some(bounds) = self.extra_bounds() {
      return Some(bounds);
    }

    let type_params: Vec<_> = self
      .generics
      .type_params()
      .map(|p| p.ident.clone())
      .collect();

    let bounds: Vec<_> = self
      .fields
      .iter()
      .flat_map(|f| nested_generic_args(&f.ty, &type_params))
      .map(|ty| quote! {#ty: std::fmt::Debug})
      .collect();

    if bounds.is_empty() {
      None
    } else {
      Some(quote! {#(#bounds),*})
    }
  }

  fn extra_bounds(&self) -> Option<TokenStream> {
    let bounds = self
      .fields
      .iter()
      .filter_map(|f| f.extra_bounds())
      .chain(self.extra_bounds.clone())
      .collect::<Vec<_>>();

    if bounds.is_empty() {
      None
    } else {
      Some(quote! {#(#bounds),*})
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

fn parse_extra_bounds(
  attrs: &Vec<Attribute>,
) -> Result<Option<TokenStream>, syn::Error> {
  let exprs = debug_attr_get(attrs, "bound")?;
  let mut bounds = vec![];
  for expr in exprs {
    let Expr::Lit(syn::ExprLit { lit, .. }) = expr else {
      return Err(syn::Error::new(expr.span(), "expected a literal"));
    };

    let syn::Lit::Str(lit) = lit else {
      return Err(syn::Error::new(lit.span(), "expected a string literal"));
    };

    let tokens: TokenStream = syn::parse_str(&lit.value().as_str())?;
    bounds.push(tokens);
  }

  if bounds.is_empty() {
    Ok(None)
  } else {
    Ok(Some(quote! { #(#bounds),* }))
  }
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

fn debug_attr_get(
  attrs: &Vec<Attribute>,
  key: &str,
) -> Result<Vec<Expr>, syn::Error> {
  let mut values = vec![];
  for attr in attrs {
    let syn::Meta::List(syn::MetaList { path, tokens, .. }) = &attr.meta else {
      continue;
    };

    if !path.is_ident("debug") {
      continue;
    }

    if let Ok(kv) = syn::parse2::<syn::MetaNameValue>(tokens.clone()) {
      if kv.path.is_ident(key) {
        values.push(kv.value);
      }
    }
  }

  Ok(values)
}
