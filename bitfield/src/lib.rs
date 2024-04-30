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
  type Alignment;
}

define_bitfield_types!(0..64);

pub mod checks {
  pub trait TotalSizeIsMultipleOfEightBits {}
  impl TotalSizeIsMultipleOfEightBits for ZeroMod8 {}

  pub trait CyclicAdd<A> {
    type O;
  }

  pub struct ZeroMod8;
  pub struct OneMod8;
  pub struct TwoMod8;
  pub struct ThreeMod8;
  pub struct FourMod8;
  pub struct FiveMod8;
  pub struct SixMod8;
  pub struct SevenMod8;

  bitfield_impl::define_cyclic_add!();
}
