use proc_macro2::{Literal, Span, TokenStream, TokenTree};
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
  body: Body,
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
    let repetitions = values
      .into_iter()
      .map(|val| self.body.clone().sub(&var, val));

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

#[derive(Debug, Clone)]
struct PasteId {
  ident: syn::Ident,
  tilde: Token![~],
  var: syn::Ident,
}

impl PasteId {
  fn span(&self) -> Span {
    self.ident.span()
  }

  fn sub(self, var: &str, value: u64) -> BodyToken {
    if self.var == var {
      let span = self.span();
      let name = format!("{}{}", self.ident, value);
      let new_ident = syn::Ident::new(&name, span);
      BodyToken::Ident(new_ident)
    } else {
      BodyToken::PasteId(self.clone())
    }
  }
}

impl Parse for PasteId {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    Ok(PasteId {
      ident: input.parse()?,
      tilde: input.parse()?,
      var: input.parse()?,
    })
  }
}

impl ToTokens for PasteId {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.ident.to_tokens(tokens);
    self.tilde.to_tokens(tokens);
    self.var.to_tokens(tokens);
  }
}

#[derive(Debug, Clone)]
enum BodyToken {
  Group(proc_macro2::Delimiter, Span, Body),
  Ident(proc_macro2::Ident),
  Literal(proc_macro2::Literal),
  Punct(proc_macro2::Punct),
  PasteId(PasteId),
}

impl BodyToken {
  fn sub(self, var: &str, value: u64) -> Self {
    match self {
      BodyToken::Group(delim, span, body) => {
        let body = body.sub(var, value);
        BodyToken::Group(delim, span, body)
      }
      BodyToken::Ident(ident) => {
        if ident == var {
          let mut lit = Literal::u64_unsuffixed(value);
          lit.set_span(ident.span());
          BodyToken::Literal(lit)
        } else {
          BodyToken::Ident(ident)
        }
      }
      BodyToken::PasteId(paste_id) => paste_id.sub(var, value),
      other => other,
    }
  }
}

impl Parse for BodyToken {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    // parse for special cases
    if input.fork().parse::<PasteId>().is_ok() {
      return Ok(Self::PasteId(input.parse()?));
    }

    input.step(|cursor| match cursor.token_tree() {
      None => Err(cursor.error("unexpected end of input")),
      Some((TokenTree::Ident(ident), rest)) => {
        Ok((BodyToken::Ident(ident), rest))
      }
      Some((TokenTree::Literal(literal), rest)) => {
        Ok((BodyToken::Literal(literal), rest))
      }
      Some((TokenTree::Punct(punct), rest)) => {
        Ok((BodyToken::Punct(punct), rest))
      }
      Some((TokenTree::Group(group), rest)) => {
        match syn::parse2(group.stream()) {
          Ok(body) => Ok((
            BodyToken::Group(group.delimiter(), group.span(), body),
            rest,
          )),
          Err(e) => Err(syn::Error::new(group.span(), e)),
        }
      }
    })
  }
}

impl ToTokens for BodyToken {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    match self {
      BodyToken::Group(delim, span, body) => {
        let mut group = proc_macro2::Group::new(*delim, body.to_token_stream());
        group.set_span(*span);
        group.to_tokens(tokens);
      }
      BodyToken::Ident(ident) => ident.to_tokens(tokens),
      BodyToken::Literal(literal) => literal.to_tokens(tokens),
      BodyToken::Punct(punct) => punct.to_tokens(tokens),
      BodyToken::PasteId(paste_id) => paste_id.to_tokens(tokens),
    }
  }
}

#[derive(Debug, Clone)]
struct Body {
  stream: Vec<BodyToken>,
}

impl Parse for Body {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let mut stream = vec![];
    while !input.is_empty() {
      stream.push(input.parse()?);
    }
    Ok(Self { stream })
  }
}

impl ToTokens for Body {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    for token in &self.stream {
      token.to_tokens(tokens);
    }
  }
}

impl Body {
  fn sub(self, var: &str, value: u64) -> Self {
    let stream = self
      .stream
      .into_iter()
      .map(|token| token.sub(var, value))
      .collect();

    Self { stream }
  }
}
