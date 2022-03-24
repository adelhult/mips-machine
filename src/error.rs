#[derive(Debug, PartialEq)]
/// Representing all possible errors produced by the machine
pub enum Error {
    InvalidRegisterNumber(u32),
    InvalidInstruction(u32),
    InstructionNotImplemented(String),
}
