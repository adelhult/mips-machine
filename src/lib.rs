mod error;
mod instructions;
pub use self::error::Error;
pub use self::instructions::*;

pub const MEMORY_SIZE: usize = 1024;

pub struct Machine {
    pc: u32,
    registers: [u32; 32],
    memory: [u8; MEMORY_SIZE],
}

impl Machine {
    /// Create a new machine with every address and register set to 0.
    pub fn new() -> Self {
        Self {
            pc: 0,
            registers: [0; 32],
            memory: [0; MEMORY_SIZE],
        }
    }

    /// Create a new machine with a given memory
    pub fn from(memory: [u8; MEMORY_SIZE]) -> Self {
        Self {
            pc: 0,
            registers: [0; 32],
            memory,
        }
    }

    /// Set the entire memory for the machine starting at addr 0
    pub fn set_memory(&mut self, memory: &[u8]) {
        self.set_memory_from(memory, 0);
    }

    /// Set a part of the memory starting at address `start`.
    pub fn set_memory_from(&mut self, memory: &[u8], start_addr: usize) {
        let mut cursor = 0;
        while let Some(byte) = memory.get(cursor) {
            // exit if there is no memory locations left
            if start_addr + cursor >= self.memory.len() {
                break;
            }

            self.memory[start_addr + cursor] = *byte;
            cursor += 1;
        }
    }

    /// Set the program counter (PC) to a given value
    pub fn set_pc(&mut self, value: u32) {
        self.pc = value;
    }

    pub fn read_pc(&self) -> u32 {
        self.pc
    }

    /// Read a word starting at the given address
    pub fn read_word(&self, address: usize) -> u32 {
        ((self.memory[address] as u32) << 24)
            + ((self.memory[address + 1] as u32) << 16)
            + ((self.memory[address + 2] as u32) << 8)
            + (self.memory[address + 3] as u32)
    }

    /// A helper function to perform a operation on two registers and
    /// return the resulting 32 bit integer
    fn op(&self, a: Register, b: Register, op: fn(u32, u32) -> u32) -> u32 {
        let x = self.read_register(a);
        let y = self.read_register(b);
        op(x, y)
    }

    /// Execute one instruction and increment the program counter.
    /// The function returns the instruction that was executed.
    pub fn step(&mut self) -> Result<Instruction, Error> {
        let instruction: Instruction = self.read_word(self.pc as usize).try_into()?;
        match instruction {
            Instruction::Add(dest, source, target, _) => {
                let sum = self.op(source, target, |a, b| (a as i32 + b as i32) as u32);
                self.set_register(dest, sum);
            }
            Instruction::Addi(_, _, _) => {
                return Err(Error::InstructionNotImplemented("addi".into()))
            }
            Instruction::Addiu(_, _, _) => {
                return Err(Error::InstructionNotImplemented("addiu".into()))
            }

            Instruction::Addu(dest, source, target, _) => {
                let sum = self.op(source, target, |a, b| a + b);
                self.set_register(dest, sum);
            }

            Instruction::And(dest, source, target, _) => {
                self.set_register(dest, self.op(source, target, |a, b| a & b));
            }
            Instruction::Andi(_, _, _) => {
                return Err(Error::InstructionNotImplemented("andi".into()))
            }
            Instruction::Beq(_, _, _) => {
                return Err(Error::InstructionNotImplemented("beq".into()))
            }
            Instruction::Blez(_, _, _) => {
                return Err(Error::InstructionNotImplemented("blez".into()))
            }
            Instruction::Bne(_, _, _) => {
                return Err(Error::InstructionNotImplemented("bne".into()))
            }
            Instruction::Bgtz(_, _, _) => {
                return Err(Error::InstructionNotImplemented("bgtz".into()))
            }
            Instruction::Div(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("div".into()))
            }
            Instruction::Divu(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("divu".into()))
            }
            Instruction::J(_) => return Err(Error::InstructionNotImplemented("j".into())),
            Instruction::Jal(_) => return Err(Error::InstructionNotImplemented("jal".into())),
            Instruction::Jalr(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("jalr".into()))
            }
            Instruction::Jr(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("jr".into()))
            }
            Instruction::Lb(_, _, _) => return Err(Error::InstructionNotImplemented("lb".into())),
            Instruction::Lbu(_, _, _) => {
                return Err(Error::InstructionNotImplemented("lbu".into()))
            }
            Instruction::Lhu(_, _, _) => {
                return Err(Error::InstructionNotImplemented("lhu".into()))
            }
            Instruction::Lui(_, _, _) => {
                return Err(Error::InstructionNotImplemented("lui".into()))
            }
            Instruction::Lw(_, _, _) => return Err(Error::InstructionNotImplemented("lw".into())),
            Instruction::Mfhi(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mfhi".into()))
            }
            Instruction::Mthi(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mthi".into()))
            }
            Instruction::Mflo(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mflo".into()))
            }
            Instruction::Mtlo(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mtlo".into()))
            }
            Instruction::Mfc0(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mfc0".into()))
            }
            Instruction::Mult(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mult".into()))
            }
            Instruction::Multu(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("multu".into()))
            }
            Instruction::Nor(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("nor".into()))
            }
            Instruction::Xor(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("xor".into()))
            }
            Instruction::Or(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("or".into()))
            }
            Instruction::Ori(_, _, _) => {
                return Err(Error::InstructionNotImplemented("ori".into()))
            }
            Instruction::Sb(_, _, _) => return Err(Error::InstructionNotImplemented("sb".into())),
            Instruction::Sh(_, _, _) => return Err(Error::InstructionNotImplemented("sh".into())),
            Instruction::Slt(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("slt".into()))
            }
            Instruction::Slti(_, _, _) => {
                return Err(Error::InstructionNotImplemented("slti".into()))
            }
            Instruction::Sltiu(_, _, _) => {
                return Err(Error::InstructionNotImplemented("sltiu".into()))
            }
            Instruction::Sltu(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("sltu".into()))
            }
            Instruction::Sll(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("sll".into()))
            }
            Instruction::Srl(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("srl".into()))
            }
            Instruction::Sra(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("sra".into()))
            }
            Instruction::Sub(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("sub".into()))
            }
            Instruction::Subu(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("subu".into()))
            }
            Instruction::Sw(_, _, _) => return Err(Error::InstructionNotImplemented("sw".into())),
        }
        self.pc += 4;
        Ok(instruction)
    }

    /// Get the current word stored in a given register
    pub fn read_register(&self, r: Register) -> u32 {
        self.registers[r as usize]
    }

    /// Set a register to a given value
    fn set_register(&mut self, r: Register, value: u32) {
        self.registers[r as usize] = value;
    }
}

impl Default for Machine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_machine() {
        let machine = Machine::new();
        assert_eq!(machine.read_word(0), 0);
    }
}
