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
          let start_bit = { #acc_offset };
          let len = { <#ty as Specifier>::BITS };

          let mut acc: u64 = 0;
          for i in 0..len {
            let n = start_bit + i;
            if self.get_bit(n) {
              acc |= 1 << i;
            }
          }

          acc
        }

        fn #set_field(&mut self, value: u64) {
          assert!(value < (1 << <#ty as Specifier>::BITS));

          let start_bit = { #acc_offset };
          let len = { <#ty as Specifier>::BITS };

          for i in 0..len {
            let n = start_bit + i;
            let bit = value & (1 << i) != 0;
            self.set_bit(n, bit);
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

      fn get_bit(&self, n: usize) -> bool {
        self.data[n / 8] & (1 << (n % 8)) != 0
      }

      fn set_bit(&mut self, n: usize, value: bool) {
        if value {
          self.data[n / 8] |= 1 << (n % 8);
        } else {
          self.data[n / 8] &= !(1 << (n % 8));
        }
      }

      fn debug(&self) {
        for b in &self.data {
          let bits_msb_left = format!("{:08b}", b);
          let bits_lsb_left = bits_msb_left.chars().rev().collect::<String>();

          print!("{} ", bits_lsb_left);
        }
        println!();
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
