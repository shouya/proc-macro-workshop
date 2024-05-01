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
pub use bitfield_impl::bitfield;
pub use bitfield_impl::BitfieldSpecifier;

use bitfield_impl::{
  define_bitfield_types, impl_specifier_for_primitive_types,
};

pub trait Specifier {
  const BITS: usize;

  // The narrowest integer type that can hold the number of bits, or
  // [u8; N]
  type Repr;

  // slice/array length: Self::BITS
  // note: type parameters may not be used in const expressions

  // from LSB to MSB
  fn from_bits(bits: &[bool]) -> Self::Repr;
  fn to_bits(repr: Self::Repr) -> Box<[bool]>;
}

impl Specifier for bool {
  const BITS: usize = 1;
  type Repr = bool;

  fn from_bits(bits: &[bool]) -> Self::Repr {
    bits[0]
  }

  fn to_bits(repr: Self::Repr) -> Box<[bool]> {
    Box::new([repr])
  }
}

// define B1, B2, ..., B64
define_bitfield_types!();

// impl Specifier for u8, etc
impl_specifier_for_primitive_types!();

pub mod checks {
  mod private {
    pub trait Sealed {}
  }

  use private::Sealed;

  pub trait TotalSizeIsMultipleOfEightBits: Sealed {}
  impl TotalSizeIsMultipleOfEightBits for [(); 0] {}

  // check for enum discriminant range
  pub enum False {}
  pub enum True {}

  impl Sealed for False {}
  impl Sealed for True {}

  pub trait DiscriminantInRange: Sealed {}
  impl DiscriminantInRange for True {}

  pub trait ArrayLenEqOne: Sealed {
    type Val;
  }

  impl Sealed for [(); 1] {}
  impl ArrayLenEqOne for [(); 1] {
    type Val = True;
  }

  impl Sealed for [(); 0] {}
  impl ArrayLenEqOne for [(); 0] {
    type Val = False;
  }

  // check for #[bit=N] attribute matching the number of BITS
  pub trait TypeEq<T> {}

  impl<T> TypeEq<T> for T {}
}
