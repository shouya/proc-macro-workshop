use proc_macro2::{Literal, TokenStream, TokenTree};
use quote::{quote_spanned, ToTokens};
use syn::{
  braced, parse::Parse, parse_macro_input, parse_quote_spanned,
  spanned::Spanned, token::Brace, LitInt, Token,
};

#[allow(dead_code)]
#[derive(Debug)]
struct SeqInput {
  var: syn::Ident,
  in_token: Token![in],
  range: syn::ExprRange,
  brace_token: Brace,
  body: TokenStream,
}

impl Parse for SeqInput {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let var = input.parse::<syn::Ident>()?;
    let in_token = input.parse::<Token![in]>()?;
    let range = input.parse::<syn::ExprRange>()?;
    let body;
    let brace_token = braced!(body in input);
    let body = body.parse()?;

    Ok(SeqInput {
      var,
      in_token,
      range,
      brace_token,
      body,
    })
  }
}

impl ToTokens for SeqInput {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self
      .repeat_body()
      .unwrap_or_else(|e| e.to_compile_error())
      .to_tokens(tokens)
  }
}

impl SeqInput {
  fn repeat_body(&self) -> syn::Result<TokenStream> {
    let values = range_values(&self.range)?;
    let var = self.var.to_string();
    let repetitions = values.into_iter().map(|val| {
      let lit = TokenTree::Literal(Literal::u64_unsuffixed(val));
      substitute(self.body.clone(), &var, &lit)
    });

    Ok(quote_spanned!(self.body.span() => #( #repetitions )*))
  }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = parse_macro_input!(input as SeqInput);

  input.to_token_stream().into()
}

fn range_values(
  syn::ExprRange {
    start, limits, end, ..
  }: &syn::ExprRange,
) -> syn::Result<Vec<u64>> {
  let start: LitInt = parse_quote_spanned!(start.span() => #start);
  let end: LitInt = parse_quote_spanned!(end.span() => #end);
  let start: u64 = start.base10_parse()?;
  let end: u64 = end.base10_parse()?;

  let values = match limits {
    syn::RangeLimits::HalfOpen(_) => (start..end).collect(),
    syn::RangeLimits::Closed(_) => (start..=end).collect(),
  };

  Ok(values)
}

fn substitute(input: TokenStream, var: &str, value: &TokenTree) -> TokenStream {
  input
    .into_iter()
    .map(|token| match token {
      TokenTree::Group(group) => {
        let span = group.span();
        let new_stream = substitute(group.stream(), var, value);
        let mut new_group =
          proc_macro2::Group::new(group.delimiter(), new_stream);
        // without setting span here the span will take on the span of
        // the call site, i.e. the seq macro invocation
        new_group.set_span(span);
        TokenTree::Group(new_group)
      }
      TokenTree::Ident(ident) if ident == var => value.clone(),
      other => other,
    })
    .collect()
}
