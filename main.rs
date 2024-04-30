// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use bitfield::*;

#[bitfield]
pub struct MyFourBytes {
  a: B1,
  b: B3,
  c: B4,
  d: B24,
}

fn main() {
  let mut bitfield = MyFourBytes::new();
  assert_eq!(0, bitfield.get_a());
  assert_eq!(0, bitfield.get_b());
  assert_eq!(0, bitfield.get_c());
  assert_eq!(0, bitfield.get_d());

  bitfield.debug();

  bitfield.set_a(1);
  bitfield.set_b(2);
  bitfield.set_c(14);
  bitfield.set_d(2999);
  bitfield.debug();
  assert_eq!(1, bitfield.get_a());
  assert_eq!(2, bitfield.get_b());
  assert_eq!(14, bitfield.get_c());
  assert_eq!(2999, bitfield.get_d());
}
