use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, parse_quote, ItemStruct, LitInt};

#[proc_macro_attribute]
pub fn bitfield(_args: TokenStream, input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as ItemStruct);
  let struct_name = &input.ident;

  let mut total_bits = vec![];

  for field in &input.fields {
    let ty = &field.ty;
    total_bits.push(quote! { <#ty as Specifier>::BITS });
  }

  let new_body = quote! {
    pub struct #struct_name {
      data: [u8; {{ #(#total_bits)+* }.div_ceil(8) }],
    }
  };

  let impl_specifier = quote! {
    impl Specifier for #struct_name {
      const BITS: usize = { #(#total_bits)+* };
    }
  };

  let impl_fields = {
    let mut field_impls = vec![];

    for field in &input.fields {
      let ty = &field.ty;
    }
  };

  quote! {
    #new_body
    #impl_specifier
  }
  .into()
}

#[proc_macro]
pub fn define_bitfield_types(input: TokenStream) -> TokenStream {
  let syn::ExprRange {
    limits, start, end, ..
  } = parse_macro_input!(input);
  if !matches!(limits, syn::RangeLimits::HalfOpen(_)) {
    return syn::Error::new_spanned(limits, "expected half-open range (..)")
      .to_compile_error()
      .into();
  }

  let start: LitInt = parse_quote! { #start };
  let end: LitInt = parse_quote! { #end };

  let Ok(start) = start.base10_parse::<usize>() else {
    return syn::Error::new_spanned(start, "expected integer literal")
      .to_compile_error()
      .into();
  };
  let Ok(end) = end.base10_parse::<usize>() else {
    return syn::Error::new_spanned(end, "expected integer literal")
      .to_compile_error()
      .into();
  };

  let mut defns = vec![];

  for i in start..end {
    let ident = format_ident!("B{}", i);
    defns.push(quote! {
      pub enum #ident {}
      impl Specifier for #ident {
        const BITS: usize = #i;
      }
    });
  }

  quote! {
    #(#defns)*
  }
  .into()
}
