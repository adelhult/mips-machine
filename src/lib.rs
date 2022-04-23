//! A small experiment to learn more about MIPS. This repo includes a a virtual machine/mips simulator,
//! and assembler.
//!
//! # Example usage of the library:
//! ```
//! use mips_machine::*;
//! let program = "addi $t0, $zero, 10";
//!
//! let (memory, _) = Parser::new(program).assemble().unwrap();
//! let mut machine = Machine::from(memory);
//!
//! assert_eq!(machine.step(), Ok(Instruction::Addi(Register::Zero, Register::T0, 10)));
//! assert_eq!(machine.read_register(Register::T0), 10);
//! ```

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
    use Instruction::*;
    use Register::*;

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

    #[test]
    fn simple_instruction() {
        let source = "addi $t0, $zero, 10";

        let (memory, _) = Parser::new(source).assemble().unwrap();
        let mut machine = Machine::from(memory);

        assert_eq!(machine.step(), Ok(Addi(Zero, T0, 10)));
        assert_eq!(machine.read_register(T0), 10);
    }

    #[test]
    fn pseudo_instructions() {
        let source = r#"
        main:   li $t0, 10     # load a small number < u16::MAX
        foo:    li $t1, 65540  # load a big number > u16::MAX
                la $t2, foo"#;

        let (memory, labels) = Parser::new(source).assemble().unwrap();
        let mut machine = Machine::from(memory);

        // load a small imm number < u16::MAX
        assert_eq!(machine.step(), Ok(Addi(Zero, T0, 10)));
        assert_eq!(machine.read_register(T0), 10);

        // load a big imm number > u16::Max
        machine.step().unwrap();
        machine.step().unwrap();
        assert_eq!(machine.read_register(T1), 65540);

        // load address
        machine.step().unwrap();
        machine.step().unwrap();
        assert_eq!(
            machine.read_register(T2),
            *labels.get("foo").unwrap() as u32
        );
    }
}
