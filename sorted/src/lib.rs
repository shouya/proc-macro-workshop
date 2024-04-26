use std::fmt::Display;

use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{
  parse::Parse, parse_macro_input, spanned::Spanned, Error, Ident, Pat, Path,
};

#[derive(Debug, Clone)]
struct Key {
  name: String,
  span: Span,
}

impl ToTokens for Key {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.name.to_tokens(tokens)
  }
}

impl Display for Key {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name)
  }
}

impl PartialOrd for Key {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Key {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.name.cmp(&other.name)
  }
}

impl PartialEq for Key {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

impl Eq for Key {}

impl From<&Ident> for Key {
  fn from(ident: &Ident) -> Self {
    Self {
      name: ident.to_string(),
      span: ident.span(),
    }
  }
}

impl From<&Path> for Key {
  fn from(path: &Path) -> Self {
    Self {
      name: path_to_string(path),
      span: path.span(),
    }
  }
}

struct OutOfOrderKey {
  key: Key,
  should_before_key: Key,
}

impl OutOfOrderKey {
  fn try_from_keys(keys: &[Key]) -> Option<Self> {
    let mut sorted = keys.to_vec();
    sorted.sort();

    for (ksorted, kactual) in sorted.iter().zip(keys.iter()) {
      if ksorted != kactual {
        return Some(OutOfOrderKey {
          key: ksorted.clone(),
          should_before_key: kactual.clone(),
        });
      }
    }

    None
  }
}

enum SortedInput {
  Enum(syn::ItemEnum),
  Match(syn::ExprMatch),
}

impl SortedInput {
  fn keys(&self) -> Vec<Key> {
    match self {
      SortedInput::Enum(item_enum) => item_enum
        .variants
        .iter()
        .map(|v| &v.ident)
        .map(Key::from)
        .collect(),
      SortedInput::Match(expr_match) => expr_match
        .arms
        .iter()
        .flat_map(|a| pat_to_names(&a.pat).into_iter())
        .map(Key::from)
        .collect(),
    }
  }
}

fn pat_to_names(pat: &Pat) -> Vec<&syn::Path> {
  match pat {
    Pat::Paren(p) => pat_to_names(&p.pat),
    Pat::Path(p) => vec![&p.path],
    Pat::Struct(p) => vec![&p.path],
    Pat::TupleStruct(p) => vec![&p.path],
    Pat::Or(p) => p.cases.iter().flat_map(pat_to_names).collect(),
    _ => vec![],
  }
}

fn path_to_string(path: &syn::Path) -> String {
  path
    .segments
    .iter()
    .map(|s| s.ident.to_string())
    .collect::<Vec<_>>()
    .join("::")
}

impl Parse for SortedInput {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    if let Ok(expr_match) = input.parse() {
      return Ok(SortedInput::Match(expr_match));
    }

    // we parse enum after match because it takes into account
    // e.g. pub keyword and may fail several tokens away.
    if let Ok(item_enum) = input.parse() {
      return Ok(SortedInput::Enum(item_enum));
    }

    // otherwise
    let call_site = Span::call_site();
    Err(Error::new(call_site, "expected enum or match expression"))
  }
}

impl ToTokens for SortedInput {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    match self {
      SortedInput::Enum(item_enum) => item_enum.to_tokens(tokens),
      SortedInput::Match(expr_match) => expr_match.to_tokens(tokens),
    }
    let ooo_key = OutOfOrderKey::try_from_keys(&self.keys());
    if let Some(ooo_key) = ooo_key {
      let key = &ooo_key.key;
      let should_before_key = &ooo_key.should_before_key;
      let error = Error::new(
        key.span,
        format!("{} should sort before {}", key, should_before_key),
      );
      tokens.extend(error.to_compile_error());
    }
  }
}

#[proc_macro_attribute]
pub fn sorted(
  _args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let input = parse_macro_input!(input as SortedInput);
  input.into_token_stream().into()
}
