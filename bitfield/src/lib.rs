// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::{bitfield, define_bitfield_types};

pub trait Specifier {
  const BITS: usize;

  // One of XMod8 types from checks module. Used for checking
  // alignment on compile time.
  type Alignment;

  // The narrowest integer type that can hold the number of bits, or
  // [u8; N]
  type Repr: BitfieldRepr;
}

define_bitfield_types!();

mod bitfield_repr {
  pub trait Sealed {}

  impl Sealed for u8 {}
  impl Sealed for u16 {}
  impl Sealed for u32 {}
  impl Sealed for u64 {}
  impl<const N: usize> Sealed for [u8; N] {}
}

pub trait BitfieldRepr: bitfield_repr::Sealed {
  // from LSB to MSB
  fn from_bits<I: std::iter::Iterator<Item = bool>>(bits: I) -> Self;

  // from LSB to MSB
  fn to_bits(&self) -> impl Iterator<Item = bool> + '_;
}

macro_rules! define_bitfield_repr {
  ($t:ty, $b:literal) => {
    impl BitfieldRepr for $t {
      fn from_bits<I: std::iter::Iterator<Item = bool>>(bits: I) -> Self {
        bits
          .enumerate()
          .fold(0, |acc, (i, b)| acc | ((b as $t) << i))
      }

      fn to_bits(&self) -> impl Iterator<Item = bool> + '_ {
        let mut n = *self;
        (0..$b).into_iter().map(move |_| {
          let bit = n & 1;
          n >>= 1;
          bit != 0
        })
      }
    }
  };
}

define_bitfield_repr!(u8, 8);
define_bitfield_repr!(u16, 16);
define_bitfield_repr!(u32, 32);
define_bitfield_repr!(u64, 64);

impl<const N: usize> BitfieldRepr for [u8; N] {
  fn from_bits<I: std::iter::Iterator<Item = bool>>(bits: I) -> Self {
    let mut acc = [0; N];
    for (i, b) in bits.enumerate() {
      let byte = &mut acc[i / 8];
      if b {
        *byte |= 1 << (i % 8);
      }
    }
    acc
  }

  fn to_bits(&self) -> impl Iterator<Item = bool> + '_ {
    (0..N)
      .flat_map(move |i| (0..8).map(move |j| (self[i] & (1 << (8 - j))) != 0))
  }
}

pub trait TotalSizeIsMultipleOfEightBits: checks::Sealed {}
impl TotalSizeIsMultipleOfEightBits for checks::ZeroMod8 {}

pub mod checks {
  pub trait Sealed {}

  impl Sealed for ZeroMod8 {}

  pub trait CyclicAdd<A> {
    type O;
  }

  pub enum ZeroMod8 {}
  pub enum OneMod8 {}
  pub enum TwoMod8 {}
  pub enum ThreeMod8 {}
  pub enum FourMod8 {}
  pub enum FiveMod8 {}
  pub enum SixMod8 {}
  pub enum SevenMod8 {}

  bitfield_impl::define_cyclic_add!();
}
