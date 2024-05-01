// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use bitfield::*;

#[bitfield]
pub struct RedirectionTableEntry {
  #[bits = 9]
  trigger_mode: TriggerMode,
  reserved: B7,
}

#[derive(BitfieldSpecifier, Debug)]
pub enum TriggerMode {
  Edge = 0,
  Level = 1,
}

fn main() {}
