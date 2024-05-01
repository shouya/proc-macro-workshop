use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DataEnum, DeriveInput, ItemStruct};

#[proc_macro_attribute]
pub fn bitfield(_args: TokenStream, input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as ItemStruct);
  let struct_name = &input.ident;

  let mut total_bits = vec![];
  let mut alignment = quote! { ::bitfield::checks::ZeroMod8 };

  for field in &input.fields {
    let ty = &field.ty;
    total_bits.push(quote! { <#ty as Specifier>::BITS });
    alignment = quote! { <#alignment as ::bitfield::checks::CyclicAdd<<#ty as Specifier>::Alignment>>::O };
  }

  let new_body = quote! {
    pub struct #struct_name {
      data: [u8; {{ #(#total_bits)+* }.div_ceil(8) }],
    }
  };

  let impl_specifier = quote! {
    impl Specifier for #struct_name
    where #alignment: ::bitfield::TotalSizeIsMultipleOfEightBits
    {
      const BITS: usize = { #(#total_bits)+* };
      type Alignment = #alignment;
      type Repr = [u8; {{ { #(#total_bits)+* }.div_ceil(8) }}];
    }
  };

  let impl_fields = {
    let mut accessor_impls = vec![];
    let mut acc_offset = quote!(0);

    for field in &input.fields {
      let ty = &field.ty;
      let get_field = format_ident!("get_{}", field.ident.as_ref().unwrap());
      let set_field = format_ident!("set_{}", field.ident.as_ref().unwrap());

      let accessor_impl = quote! {
        fn #get_field(&self) -> <#ty as Specifier>::Repr {
          let start_bit = { #acc_offset };
          let len = { <#ty as Specifier>::BITS };

          let iter = (start_bit..(start_bit+len)).map(|i| self.get_bit(i));
          <#ty as Specifier>::Repr::from_bits(iter)
        }

        fn #set_field(&mut self, value: <#ty as Specifier>::Repr) {
          // TODO: value may not be numerical value (e.g. enum variant)
          // assert!(value < (1 << <#ty as Specifier>::BITS));

          let start_bit = { #acc_offset };
          let len = { <#ty as Specifier>::BITS };

          for (i, bit) in (0..len).zip(value.to_bits()) {
            let n = start_bit + i;
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
pub fn define_bitfield_types(_input: TokenStream) -> TokenStream {
  let mut defns = vec![];

  for i in 1..=64 {
    let ident = format_ident!("B{}", i);
    let alignment = format_ident!("{}Mod8", num_name(i % 8));
    let repr = if i <= 8 {
      quote!(u8)
    } else if i <= 16 {
      quote!(u16)
    } else if i <= 32 {
      quote!(u32)
    } else {
      quote!(u64)
    };

    defns.push(quote! {
      pub enum #ident {}
      impl Specifier for #ident {
        const BITS: usize = #i;
        type Alignment = checks::#alignment;
        type Repr = #repr;
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

#[proc_macro_derive(BitfieldSpecifier)]
pub fn derive_bitfield_specifier(input: TokenStream) -> TokenStream {
  let DeriveInput { ident, data, .. } =
    parse_macro_input!(input as DeriveInput);
  let syn::Data::Enum(DataEnum { variants, .. }) = data else {
    return syn::Error::new_spanned(ident, "expected enum")
      .to_compile_error()
      .into();
  };

  if !variants.len().is_power_of_two() {
    return syn::Error::new_spanned(
      ident,
      "expected a power of two number of variants",
    )
    .to_compile_error()
    .into();
  }

  let bits = variants.len().trailing_zeros() as usize;
  let repr = if bits <= 8 {
    quote!(u8)
  } else if bits <= 16 {
    quote!(u16)
  } else if bits <= 32 {
    quote!(u32)
  } else {
    quote!(u64)
  };
  let alignment = format_ident!("{}Mod8", num_name(bits % 8));
  let mut from_val = vec![];
  let mut to_val = vec![];

  for variant in variants {
    let variant_ident = &variant.ident;
    let Some((_eq, syn::Expr::Lit(lit))) = variant.discriminant else {
      return syn::Error::new_spanned(
        variant,
        "expected explicit literal discriminant",
      )
      .to_compile_error()
      .into();
    };

    let syn::ExprLit {
      lit: syn::Lit::Int(ref int),
      ..
    } = lit
    else {
      return syn::Error::new_spanned(
        lit,
        "expected integer in discriminant like Variant = 1 or Variant = 0b011",
      )
      .to_compile_error()
      .into();
    };

    let value = int.base10_parse::<usize>().unwrap();
    if value >= 1 << bits {
      return syn::Error::new_spanned(
        lit,
        format!(
          "discriminant value {} is too large for {} bits",
          value, bits
        ),
      )
      .to_compile_error()
      .into();
    }

    from_val.push(quote! { #int => #ident::#variant_ident });
    to_val.push(quote! { #ident::#variant_ident => #int });
  }

  quote! {
    impl Specifier for #ident {
      const BITS: usize = #bits;
      type Alignment = checks::#alignment;
      type Repr = #ident;
    }

    impl BitfieldRepr for #ident {
      fn from_bits<I: std::iter::Iterator<Item = bool>>(bits: I) -> Self {
        let value = <#repr as BitfieldRepr>::from_bits(bits);
        match value {
          #( #from_val ),* ,
          _ => unreachable!(),
        }
      }

      fn to_bits(&self) -> impl Iterator<Item = bool> + '_ {
        let mut value: dyn BitfieldRepr = match *self {
          #( #to_val ),*
        };

        <#repr as BitfieldRepr>::to_bits(&value)
      }
    }
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
