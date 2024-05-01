use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote, quote_spanned};
use syn::{parse_macro_input, DataEnum, DeriveInput, ItemStruct};

#[proc_macro_attribute]
pub fn bitfield(_args: TokenStream, input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as ItemStruct);
  let struct_name = &input.ident;

  let mut total_bits = vec![];
  let mut alignment = quote_spanned! {
    Span::mixed_site() => ::bitfield::checks::ZeroMod8
  };

  for field in &input.fields {
    let ty = &field.ty;
    total_bits.push(quote! { <#ty as Specifier>::BITS });
    alignment = quote_spanned! {
      Span::mixed_site() =>
        <#alignment as ::bitfield::checks::CyclicAdd<<#ty as Specifier>::Alignment>>::O
    };
  }

  let new_body = quote! {
    pub struct #struct_name {
      data: [u8; {{ #(#total_bits)+* }.div_ceil(8) }],
    }
  };

  let impl_specifier = quote_spanned! { Span::mixed_site() =>
    impl Specifier for #struct_name
    where #alignment: ::bitfield::checks::TotalSizeIsMultipleOfEightBits
    {
      const BITS: usize = { #(#total_bits)+* };
      type Alignment = #alignment;
      type Repr = #struct_name;

      fn from_bits(bits: &[bool]) -> Self::Repr {
        let mut data = [0; {{ Self::BITS }.div_ceil(8) }];
        for (i, &bit) in bits.iter().enumerate() {
          if bit {
            data[i / 8] |= 1 << (i % 8);
          }
        }
        Self { data }
      }

      fn to_bits(repr: Self::Repr) -> Box<[bool]> {
        let mut bits = Vec::with_capacity(Self::BITS);
        for byte in repr.data {
          for i in 0..8 {
            bits.push((byte & (1 << i)) != 0);
          }
        }
        bits.into_boxed_slice()
      }
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
          let bits = (0..len).map(|i| self.get_bit(start_bit + i)).collect::<Vec<_>>();
          <#ty as Specifier>::from_bits(&bits)
        }

        fn #set_field(&mut self, value: <#ty as Specifier>::Repr) {
          // TODO: value may not be numerical value (e.g. enum variant)
          // assert!(value < (1 << <#ty as Specifier>::BITS));

          let start_bit = { #acc_offset };
          let len = { <#ty as Specifier>::BITS };
          let bits = <#ty as Specifier>::to_bits(value);
          // take(..) is needed because #ty's Repr may be longer than the actual bit width
          for (i, &bit) in bits.iter().take(len).enumerate() {
            self.set_bit(start_bit + i, bit);
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

    let from_bits = (0..i).map(|j| quote! {if bits[#j] {value |= 1 << #j;}});
    let to_bits = (0..i).map(|j| quote! {(repr & (1 << #j)) != 0});

    defns.push(quote! {
      // these types are not constructible
      pub enum #ident {}

      impl Specifier for #ident {
        const BITS: usize = #i;
        type Alignment = checks::#alignment;
        type Repr = #repr;

        fn from_bits(bits: &[bool]) -> Self::Repr {
          let mut value: Self::Repr = 0;
          #( #from_bits )*
          value
        }

        fn to_bits(repr: Self::Repr) -> Box<[bool]> {
          Box::new([ #( #to_bits ),* ])
        }
      }
    });
  }

  quote! {
    #(#defns)*
  }
  .into()
}

#[proc_macro]
pub fn impl_specifier_for_primitive_types(_: TokenStream) -> TokenStream {
  let mut defns = vec![];
  for (ty, bits) in [("u8", 8usize), ("u16", 16), ("u32", 32), ("u64", 64)] {
    let ident = format_ident!("{}", ty);

    let from_bits = (0..bits).map(|j| quote! {if bits[#j] {value |= 1 << #j;}});
    let to_bits = (0..bits).map(|j| quote! {(repr & (1 << #j)) != 0});

    let defn = quote! {
      impl Specifier for #ident {
        const BITS: usize = #bits;
        // primitive number types are always aligned to 8 bits
        type Alignment = checks::ZeroMod8;
        type Repr = #ident;

        fn from_bits(bits: &[bool]) -> Self::Repr {
          let mut value: Self::Repr = 0;
          #( #from_bits )*
          value
        }

        fn to_bits(repr: Self::Repr) -> Box<[bool]> {
          Box::new([ #( #to_bits ),* ])
        }
      }
    };

    defns.push(defn);
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
    return syn::Error::new(
      Span::call_site(),
      "BitfieldSpecifier expected a number of variants which is a power of 2",
    )
    .to_compile_error()
    .into();
  }

  let bits = variants.len().trailing_zeros() as usize;
  let max_value: usize = 1 << bits;
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
  let mut discriminant_checks = vec![];

  for variant in variants {
    let variant_ident = &variant.ident;

    from_val.push(quote! {
      if value == #ident::#variant_ident as #repr {
        return #ident::#variant_ident;
      }
    });

    let arr =
      quote!([(); ((#ident::#variant_ident as usize) < #max_value) as usize]);
    discriminant_checks.push(quote! {
      <#arr as ::bitfield::checks::ArrayLenEqOne>::Val :
      ::bitfield::checks::DiscriminantInRange
    });
  }

  quote! {
    impl Specifier for #ident where
      #( #discriminant_checks ),*
    {
      const BITS: usize = #bits;
      type Alignment = checks::#alignment;
      type Repr = #ident;

      fn from_bits(bits: &[bool]) -> Self::Repr {
        let value = bits.iter().enumerate().fold(0, |acc, (i, &b)| {
          acc | (b as #repr) << i
        });

        #( #from_val )*

        unreachable!("bit pattern does not match any variant")
      }

      fn to_bits(repr: Self::Repr) -> Box<[bool]> {
        let value = repr as #repr;
        (0..#bits).map(|i| (value & (1 << i)) != 0).collect::<Vec<_>>().into_boxed_slice()
      }

    }
  }.into()
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
