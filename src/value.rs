use fixed::FixedI128;
use fixed::types::extra::U32;

pub type Number = FixedI128<U32>;

#[derive(Debug, PartialEq, Eq, PartialOrd)]
pub enum Value {
    Num(Number),
    Str(String),
    Bool(bool)
}