use bitfield::*;

type A = B1;
type B = B3;
type C = B4;
type D = B23;

#[bitfield]
pub struct NotQuiteFourBytes {
    a: A,
    b: B,
    c: C,
    d: D,
}

fn main() {}
