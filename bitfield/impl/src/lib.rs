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
    let mut accessor_impls = vec![];
    let mut acc_offset = quote!(0);

    for field in &input.fields {
      let ty = &field.ty;
      let get_field = format_ident!("get_{}", field.ident.as_ref().unwrap());
      let set_field = format_ident!("set_{}", field.ident.as_ref().unwrap());

      // TODO: change u64 to #ty::Repr
      let accessor_impl = quote! {
        fn #get_field(&self) -> u64 {
          // they all should really be constexpr
          let offset_start = { (#acc_offset) / 8 };
          let offset_end = { offset_start + ({ <#ty as Specifier>::BITS } / 8)};
          let shift_end = { offset_end % 8 };

          let mut acc = 0u128;

          for b in (self.data[offset_start..=offset_end]).iter().rev() {
            acc <<= 8;
            acc |= *b as u128;
          }

          ((acc >> shift_end) & 0xffff_ffff_ffff_ffff) as u64
        }

        fn #set_field(&mut self, value: u64) {
          let offset_start = { (#acc_offset) / 8 };
          let offset_end = { offset_start + ({ <#ty as Specifier>::BITS } / 8)};
          let shift_start = { offset_start % 8 };
          let shift_end = { offset_end % 8 };
          let starting_byte_mask = 0xff >> shift_start;
          let ending_byte_mask = 0xff << shift_end;

          let value = value as u128;
          for i in (offset_start..=offset_end).into_iter() {
            let mask = if i == offset_start {
              starting_byte_mask
            } else if i == offset_end {
              ending_byte_mask
            } else {
              0xff
            };

            let value_b = (value >> (8 * (offset_end - i)) & 0xff) as u8;
            let source_b = self.data[i];

            self.data[i] = (source_b & !mask) | (value_b & mask);
          }
        }
      };

      accessor_impls.push(accessor_impl);
      acc_offset = quote! { (#acc_offset) + <#ty as Specifier>::BITS };
    }

    accessor_impls
  };

  quote! {
    #new_body
    #impl_specifier

    impl #struct_name {
      fn new() -> Self {
        Self { data: Default::default() }
      }

      #( #impl_fields )*
    }
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

#[proc_macro]
pub fn define_cyclic_add(_: TokenStream) -> TokenStream {
  let mut defns = vec![];

  for a in 0..8 {
    for b in 0..8 {
      let a_name = format_ident!("{}Mod8", num_name(a));
      let b_name = format_ident!("{}Mod8", num_name(b));
      let sum_name = format_ident!("{}Mod8", num_name((a + b) % 8));

      let defn = quote! {
         impl CyclicAdd<#a_name> for #b_name {
           type O = #sum_name;
         }
      };

      defns.push(defn);
    }
  }

  quote! {
    #(#defns)*
  }
  .into()
}

fn num_name(n: usize) -> &'static str {
  match n {
    0 => "Zero",
    1 => "One",
    2 => "Two",
    3 => "Three",
    4 => "Four",
    5 => "Five",
    6 => "Six",
    7 => "Seven",
    _ => unreachable!(),
  }
}
