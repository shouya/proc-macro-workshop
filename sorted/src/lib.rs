use std::fmt::Display;

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
  parse::Parse, parse_macro_input, spanned::Spanned, visit_mut::VisitMut,
  Error, Ident, Pat, PatWild, Path, Token,
};

#[derive(Debug, Clone)]
enum KeyData {
  Path(Path),
  Ident(Ident),
  Underscore(Token![_]),
}

#[derive(Debug, Clone)]
struct Key {
  name: String,
  data: KeyData,
  is_underscore: bool,
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
    (self.is_underscore, &self.name).cmp(&(other.is_underscore, &other.name))
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
    let name = ident.to_string();
    let is_underscore = name.starts_with('_');
    let name = name.trim_start_matches('_');
    Self {
      name: name.to_string(),
      data: KeyData::Ident(ident.clone()),
      is_underscore,
    }
  }
}

impl From<&Path> for Key {
  fn from(path: &Path) -> Self {
    let name = path_to_string(path);
    Self {
      name,
      data: KeyData::Path(path.clone()),
      is_underscore: false,
    }
  }
}

impl From<&PatWild> for Key {
  fn from(underscore: &PatWild) -> Self {
    Self {
      name: "".to_string(),
      data: KeyData::Underscore(underscore.underscore_token.clone()),
      is_underscore: true,
    }
  }
}

impl Key {
  fn error(&self, message: &str) -> syn::Error {
    match &self.data {
      KeyData::Ident(ident) => Error::new_spanned(ident, message),
      KeyData::Path(path) => Error::new_spanned(path, message),
      KeyData::Underscore(underscore) => {
        Error::new_spanned(underscore, message)
      }
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
  fn keys(&self) -> syn::Result<Vec<Key>> {
    let keys = match self {
      SortedInput::Enum(item_enum) => item_enum
        .variants
        .iter()
        .map(|v| &v.ident)
        .map(Key::from)
        .collect(),
      SortedInput::Match(expr_match) => expr_match
        .arms
        .iter()
        .map(|a| pat_to_names(&a.pat))
        .collect::<syn::Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect(),
    };

    Ok(keys)
  }

  fn error(&self) -> Option<Error> {
    let keys = match self.keys() {
      Ok(keys) => keys,
      Err(e) => return Some(e),
    };

    let ooo_key = OutOfOrderKey::try_from_keys(&keys)?;
    let key = &ooo_key.key;
    let should_before_key = &ooo_key.should_before_key;
    let message = format!("{} should sort before {}", key, should_before_key);
    Some(key.error(&message))
  }
}

fn pat_to_names(pat: &Pat) -> syn::Result<Vec<Key>> {
  match pat {
    Pat::Ident(i) => Ok(vec![Key::from(&i.ident)]),
    Pat::Paren(p) => pat_to_names(&p.pat),
    Pat::Path(p) => Ok(vec![Key::from(&p.path)]),
    Pat::Struct(p) => Ok(vec![Key::from(&p.path)]),
    Pat::TupleStruct(p) => Ok(vec![Key::from(&p.path)]),
    Pat::Wild(p) => Ok(vec![Key::from(p)]),
    Pat::Or(p) => Ok(
      p.cases
        .iter()
        .map(pat_to_names)
        .collect::<syn::Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<_>>(),
    ),
    _ => Err(Error::new(pat.span(), "unsupported by #[sorted]")),
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
  }
}

#[proc_macro_attribute]
pub fn sorted(
  _args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let input = parse_macro_input!(input as SortedInput);
  if let Some(error) = input.error() {
    let error = error.to_compile_error();
    quote! {
      #input
      #error
    }
    .into()
  } else {
    input.into_token_stream().into()
  }
}

#[proc_macro_attribute]
pub fn check(
  _args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let mut input = parse_macro_input!(input as syn::ItemFn);
  let mut visitor = CheckVisitor::default();
  visitor.visit_item_fn_mut(&mut input);

  if let Some(error) = visitor.error {
    let error = error.to_compile_error();
    quote! {
      #input
      #error
    }
    .into()
  } else {
    input.into_token_stream().into()
  }
}

#[derive(Default)]
struct CheckVisitor {
  error: Option<Error>,
}

impl VisitMut for CheckVisitor {
  fn visit_expr_match_mut(&mut self, expr_match: &mut syn::ExprMatch) {
    let sorted_index = expr_match
      .attrs
      .iter()
      .position(|a| a.meta.path().is_ident("sorted"));

    let Some(idx) = sorted_index else {
      return syn::visit_mut::visit_expr_match_mut(self, expr_match);
    };
    // could be used to signal site for parse error
    let _attr = expr_match.attrs.remove(idx);

    let input: SortedInput = syn::parse_quote! { #expr_match };
    if let Some(error) = input.error() {
      self.error = Some(error);
    }

    // in case there are #[sorted] tagged match expressions nested
    // within this match expression.
    syn::visit_mut::visit_expr_match_mut(self, expr_match);
  }
}
