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

  // One of XMod8 types from checks module. Used for checking
  // alignment on compile time.
  type Alignment;

  // The narrowest integer type that can hold the number of bits, or
  // [u8; N]
  type Repr;

  // slice/array length: Self::BITS
  // note: type parameters may not be used in const expressions

  // from LSB to MSB
  fn from_bits(bits: &[bool]) -> Self::Repr;
  fn to_bits(repr: &Self::Repr) -> Box<[bool]>;
}

impl Specifier for bool {
  const BITS: usize = 1;
  type Alignment = checks::OneMod8;
  type Repr = bool;

  fn from_bits(bits: &[bool]) -> Self::Repr {
    bits[0]
  }

  fn to_bits(repr: &Self::Repr) -> Box<[bool]> {
    Box::new([*repr])
  }
}

// define B1, B2, ..., B64
define_bitfield_types!();

// impl Specifier for u8, etc
impl_specifier_for_primitive_types!();

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
