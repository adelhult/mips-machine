mod assembler;
mod error;
mod instructions;
mod machine;
pub use self::error::Error;
pub use self::instructions::*;
pub use self::machine::*;
pub use assembler::Parser;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_machine() {
        let machine = Machine::new();
        assert_eq!(machine.read_word(0), 0);
    }

    #[test]
    fn read_and_write() {
        const NUMBER: u32 = 5;
        let mut machine = Machine::new();
        machine.write_word(0, NUMBER);
        assert_eq!(machine.read_word(0), NUMBER);
    }
}
