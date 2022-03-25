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
            | ((self.memory[address + 1] as u32) << 16)
            | ((self.memory[address + 2] as u32) << 8)
            | (self.memory[address + 3] as u32)
    }

    /// Write a word starting at the given word-aligned address
    pub fn write_word(&mut self, address: usize, word: u32) {
        self.memory[address] = word as u8;
        self.memory[address + 1] = (word >> 8) as u8;
        self.memory[address + 2] = (word >> 16) as u8;
        self.memory[address + 3] = (word >> 24) as u8;
    }

    /// Write a halfword starting at the given halfword-aligned address
    pub fn write_halfword(&mut self, address: usize, halfword: u16) {
        self.memory[address] = halfword as u8;
        self.memory[address + 1] = (halfword >> 8) as u8;
    }

    /// Write a byte starting at the given address
    pub fn write_byte(&mut self, address: usize, byte: u8) {
        self.memory[address] = byte;
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
            Instruction::Add(dest, source0, source1, _) => {
                let sum = self.op(source0, source1, |a, b| (a as i32 + b as i32) as u32);
                self.set_register(dest, sum);
            }

            Instruction::Addi(rs, rt, immediate) => {
                let sum = self.read_register(rs) + ((immediate as i32) as u32);
                self.set_register(rt, sum);
            }

            Instruction::Addiu(_, _, _) => {
                return Err(Error::InstructionNotImplemented("addiu".into()))
            }

            Instruction::Addu(dest, source0, source1, _) => {
                let sum = self.op(source0, source1, |a, b| a + b);
                self.set_register(dest, sum);
            }

            Instruction::And(dest, source0, source1, _) => {
                self.set_register(dest, self.op(source0, source1, |a, b| a & b));
            }

            Instruction::Andi(dest, source0, constant) => {
                self.set_register(dest, self.read_register(source0) & constant as u32);
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

            Instruction::Div(dest, source0, source1, _) => {
                let result = self.op(source0, source1, |a, b| (a as i32 / b as i32) as u32);
                self.set_register(dest, result);
            }

            Instruction::Divu(dest, source0, source1, _) => {
                let result = self.op(source0, source1, |a, b| a / b);
                self.set_register(dest, result);
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
            Instruction::Sb(base, rt, offset) => {
                let dest_adr = self.read_register(base) + (offset as i16) as u32; // TODO: use wrapping u32
                self.write_word(dest_adr as usize, self.read_register(rt));
            }
            Instruction::Sh(base, rt, offset) => {
                let dest_adr = self.read_register(base) + (offset as i16) as u32;
                self.write_halfword(dest_adr as usize, self.read_register(rt) as u16);
            }

            Instruction::Slt(rd, rt, rs, _) => {
                self.set_register(
                    rd,
                    if (self.read_register(rs) as i32) < (self.read_register(rt) as i32) {
                        1
                    } else {
                        0
                    },
                );
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

    #[test]
    fn write_word() {
        const BRUH: u32 = 0xFAFAFAFA;
        let mut machine = Machine::new();
        machine.write_word(0, BRUH);
        assert_eq!(machine.read_word(0), BRUH);
    }

    #[test]
    fn write_halfword() {
        let mut machine = Machine::new();
        //let x: [u32; 7] = [0x3c011001,0x34210000,0x00011020,0x20010005,0xa4410000, 0x20010006, 0xa4410002];
        let x: &[u32] = &[0x20020100, 0x20030005, 0xa4430000, 0x20030006, 0xa4430002];
        let v = x
            .iter()
            .copied()
            .map(|word| word.to_be_bytes().into_iter())
            .flatten()
            .collect::<Vec<u8>>();
        machine.set_memory_from(&v, 0);
        for _ in 1..=x.len() {
            let res = machine.step().unwrap();
            dbg!(res);
            dbg!(machine.read_pc());
        }
        dbg!(machine.read_register(Register::Zero));
        dbg!(machine.read_register(Register::V0));
        dbg!(machine.read_register(Register::V1));
        dbg!(machine.read_register(Register::A0));
        dbg!(machine.read_register(Register::A1));
        assert_eq!(machine.read_word(0x0100), 0x00060005_u32.to_be());
    }
}
