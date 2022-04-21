use crate::error::Error;
use crate::instructions::*;

pub const MEMORY_SIZE: usize = 0x7FFF; // 4096 bytes
pub const STACK_BASE_ADDRESS: usize = 0x7FFF;
pub const HEAP_BASE_ADDRESS: usize = 0x3000;
pub const DATA_BASE_ADDRESS: usize = 0x2000;
pub const TEXT_BASE_ADDRESS: usize = 0x0000;

pub struct Machine {
    pc: u32,
    registers: [u32; 32],
    memory: [u8; MEMORY_SIZE],
}

impl Machine {
    /// Create a new machine with every address and register set to 0.
    pub fn new() -> Self {
        Self {
            pc: TEXT_BASE_ADDRESS as u32,
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
        u32::from_be_bytes([
            self.memory[address],
            self.memory[address + 1],
            self.memory[address + 2],
            self.memory[address + 3],
        ])
    }

    /// Read a word starting at the given address
    pub fn read_halfword(&self, address: usize) -> u16 {
        u16::from_be_bytes([self.memory[address], self.memory[address + 1]])
    }

    /// Read a byte starting at the given address
    pub fn read_byte(&self, address: usize) -> u8 {
        self.memory[address]
    }

    /// Write a word starting at the given word-aligned address
    pub fn write_word(&mut self, address: usize, word: u32) {
        self.memory[address + 3] = word as u8;
        self.memory[address + 2] = (word >> 8) as u8;
        self.memory[address + 1] = (word >> 16) as u8;
        self.memory[address] = (word >> 24) as u8;
    }

    /// Write a halfword starting at the given halfword-aligned address
    pub fn write_halfword(&mut self, address: usize, halfword: u16) {
        self.memory[address + 1] = halfword as u8;
        self.memory[address] = (halfword >> 8) as u8;
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

        self.pc += 4;
        match instruction {
            // TODO: If the addition results in 32-bit 2’s complement arithmetic overflow,
            // the destination register is not modified and an Integer Overflow exception occurs.
            Instruction::Add(rs, rt, rd, _) => {
                let sum = self.op(rs, rt, |a, b| (a as i32 + b as i32) as u32);
                self.set_register(rd, sum);
            }

            // TODO: If the addition results in 32-bit 2’s complement arithmetic overflow,
            // the destination register is not modified and an Integer Overflow exception occurs.
            Instruction::Addi(rs, rt, immediate) => {
                let sum = (self.read_register(rs) as i32 + immediate as i32) as u32;
                self.set_register(rt, sum);
            }

            Instruction::Addiu(rs, rt, immediate) => {
                let sum = (self.read_register(rs) as i32 + immediate as i32) as u32;
                self.set_register(rt, sum);
            }

            Instruction::Addu(rs, rt, rd, _) => {
                let sum = self.op(rs, rt, |a, b| a + b);
                self.set_register(rd, sum);
            }

            Instruction::And(rs, rt, rd, _) => {
                self.set_register(rd, self.op(rs, rt, |a, b| a & b));
            }

            Instruction::Andi(rs, rt, immediate) => {
                self.set_register(rt, self.read_register(rs) & immediate as u32);
            }

            // TODO: Control Transfer Instructions (CTIs) should not be placed in branch delay slots
            // or Release 6 forbidden slots.
            Instruction::Beq(rs, rt, offset) => {
                if self.read_register(rs) == self.read_register(rt) {
                    self.set_pc((self.read_pc() as i32 + (offset << 2) as i32) as u32);
                }
            }

            // TODO: Control Transfer Instructions (CTIs) should not be placed in branch delay slots
            // or Release 6 forbidden slots.
            Instruction::Blez(rs, _, offset) => {
                if self.read_register(rs) as i32 <= 0 {
                    self.set_pc((self.read_pc() as i32 + (offset << 2) as i32) as u32);
                }
            }

            // TODO: Control Transfer Instructions (CTIs) should not be placed in branch delay slots
            // or Release 6 forbidden slots.
            Instruction::Bne(rs, rt, offset) => {
                if self.read_register(rs) != self.read_register(rt) {
                    self.set_pc((self.read_pc() as i32 + (offset << 2) as i32) as u32);
                }
            }

            // TODO: Control Transfer Instructions (CTIs) should not be placed in branch delay slots
            // or Release 6 forbidden slots.
            Instruction::Bgtz(rs, _, offset) => {
                if self.read_register(rs) as i32 > 0 {
                    self.set_pc((self.read_pc() as i32 + (offset << 2) as i32) as u32);
                }
            }

            // NOTE: This is the MIPS32 Release 6 version of this operation.
            // TODO: No arithmetic exceptions occur. Division by zero produces an UNPREDICTABLE result.
            Instruction::Div(dest, source0, source1, _) => {
                let result = self.op(source0, source1, |a, b| (a as i32 / b as i32) as u32);
                self.set_register(dest, result);
            }

            // NOTE: This is removed since MIPS32 Release 6.
            Instruction::DivOLD(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("divOLD".into()))
            }

            Instruction::Divu(dest, source0, source1, _) => {
                let result = self.op(source0, source1, |a, b| a / b);
                self.set_register(dest, result);
            }

            // NOTE: This is removed since MIPS32 Release 6.
            Instruction::DivuOLD(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("divuOLD".into()))
            }

            // NOTE: The Jump instruction has been deprecated in Release 6. Use BC instead.
            // TODO: Control Transfer Instructions (CTIs) should not be placed in branch delay slots or Release 6 forbidden slots.
            // TODO: Jump to the effective target address. Execute the instruction that follows the jump, in the branch delay slot,
            // before executing the jump itself.
            Instruction::J(instr_index) => {
                let dest_adr = (self.read_pc() << 28) | (instr_index << 2);
                self.set_pc(dest_adr);
            }

            // NOTE: The Jump-and-Link instruction has been deprecated in Release 6. Use BALC instead.
            // TODO: Control Transfer Instructions (CTIs) should not be placed in branch delay slots or Release 6 forbidden slots.
            // TODO: Release 6: If a control transfer instruction (CTI) is executed in the delay slot of a branch or jump,
            // Release 6 implementations are required to signal a Reserved Instruction exception.
            Instruction::Jal(instr_index) => {
                self.set_register(Register::Ra, self.read_pc());
                let dest_adr = (self.read_pc() << 28) | (instr_index << 2);
                self.set_pc(dest_adr);
            }

            // TODO: Control Transfer Instructions (CTIs) should not be placed in branch delay slots or Release 6 forbidden slots.
            Instruction::Jalr(rs, _, rd, _hint) => {
                // TODO: The return link is the address of the second instruction following the branch,
                // where execution continues after a procedure call. <--- what does this really
                // mean? // Enaya
                self.set_register(rd, self.read_pc());
                self.set_pc(self.read_register(rs));
            }

            // TODO: Control Transfer Instructions (CTIs) should not be placed in branch delay slots or Release 6 forbidden slots.
            // TODO:  Execute the instruction following the jump, in the branch delay slot, before jumping. <---- what????? // Enaya
            Instruction::Jr(rs, _, _, _hint) => {
                self.set_pc(self.read_register(rs));
            }

            Instruction::Lb(base, rt, offset) => {
                let dest_adr = (self.read_register(base) as i32 + offset as i32) as u32;
                self.set_register(rt, (self.read_byte(dest_adr as usize) as i32) as u32);
            }

            Instruction::Lbu(base, rt, offset) => {
                let dest_adr = (self.read_register(base) as i32 + offset as i32) as u32;
                self.set_register(rt, self.read_byte(dest_adr as usize) as u32);
            }

            Instruction::Lhu(base, rt, offset) => {
                let dest_adr = (self.read_register(base) as i32 + offset as i32) as u32;
                self.set_register(rt, self.read_halfword(dest_adr as usize) as u32);
            }

            Instruction::Lui(_, rt, immediate) => {
                self.set_register(rt, (immediate as u32) << 16);
            }

            Instruction::Lw(base, rt, offset) => {
                let dest_adr = (self.read_register(base) as i32 + offset as i32) as u32;
                self.set_register(rt, self.read_word(dest_adr as usize));
            }

            // TODO: The contents of special register HI are loaded into GPR rd. `GPR[rd] <- HI`
            // TODO: what the crap is the HI register // Enaya
            // NOTE: This instruction has been removed in Release 6.
            Instruction::Mfhi(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mfhi".into()))
            }

            // TODO: The contents of GPR rs are loaded into special register HI.
            // NOTE: This instruction has been removed in Release 6.
            Instruction::Mthi(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mthi".into()))
            }

            // TODO: The contents of special register LO are loaded into GPR rd.
            // NOTE: This instruction has been removed in Release 6.
            Instruction::Mflo(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mflo".into()))
            }

            // TODO: The contents of GPR rs are loaded into special register LO.
            // NOTE: This instruction has been removed in Release 6.
            Instruction::Mtlo(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mtlo".into()))
            }

            // TODO: Move the contents of a coprocessor 0 register to a general register.
            // what the crap is the coprocessor 0 register????? // Enaya
            Instruction::Mfc0(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mfc0".into()))
            }

            // TODO: No arithmetic exceptions occur. Division by zero produces an UNPREDICTABLE result.
            Instruction::Mod(rs, rt, rd, _) => {
                let result = self.op(rs, rt, |a, b| (a as i32).rem_euclid(b as i32) as u32);
                self.set_register(rd, result);
            }

            // TODO: No arithmetic exceptions occur. Division by zero produces an UNPREDICTABLE result.
            Instruction::Modu(rs, rt, rd, _) => {
                let result = self.op(rs, rt, |a, b| a % b);
                self.set_register(rd, result);
            }

            // NOTE: MIPS32, removed in Release 6
            Instruction::Mult(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("mult".into()))
            }

            // NOTE: MIPS32, removed in Release 6
            Instruction::Multu(_, _, _, _) => {
                return Err(Error::InstructionNotImplemented("multu".into()))
            }

            Instruction::Nor(rs, rt, rd, _) => {
                let res = self.op(rs, rt, |a, b| !(a | b));
                self.set_register(rd, res);
            }

            Instruction::Xor(rs, rt, rd, _) => {
                let res = self.op(rs, rt, |a, b| (a ^ b));
                self.set_register(rd, res);
            }

            Instruction::Or(rs, rt, rd, _) => {
                let res = self.op(rs, rt, |a, b| a | b);
                self.set_register(rd, res);
            }

            Instruction::Ori(rs, rt, immediate) => {
                self.set_register(rt, self.read_register(rs) | immediate as u32);
            }
            Instruction::Sb(base, rt, offset) => {
                let dest_adr = (self.read_register(base) as i32 + offset as i32) as u32; // TODO: use wrapping u32
                self.write_byte(dest_adr as usize, self.read_register(rt) as u8);
            }
            Instruction::Sh(base, rt, offset) => {
                let dest_adr = (self.read_register(base) as i32 + offset as i32) as u32;
                self.write_halfword(dest_adr as usize, self.read_register(rt) as u16);
            }

            Instruction::Slt(rs, rt, rd, _) => {
                self.set_register(
                    rd,
                    ((self.read_register(rs) as i32) < (self.read_register(rt) as i32)) as u32,
                );
            }

            Instruction::Slti(rs, rt, immediate) => {
                self.set_register(
                    rt,
                    ((self.read_register(rs) as i32) < (immediate as i32)) as u32,
                );
            }

            Instruction::Sltiu(rs, rt, immediate) => {
                self.set_register(rt, (self.read_register(rs) < (immediate as u32)) as u32);
            }

            Instruction::Sltu(rs, rt, rd, _) => {
                self.set_register(rd, self.op(rs, rt, |a, b| (a < b) as u32))
            }

            Instruction::Sll(_, rt, rd, sa) => {
                self.set_register(rd, self.read_register(rt) << sa);
            }

            Instruction::Srl(_, rt, rd, sa) => {
                self.set_register(rd, self.read_register(rt) >> sa);
            }

            Instruction::Sra(_, rt, rd, sa) => {
                // The language will give you either LSR or ASR, depending on what type integer you give it
                self.set_register(rd, (self.read_register(rt) as i32 >> sa) as u32);
            }

            // TODO: If the subtraction results in 32-bit 2’s complement arithmetic overflow,
            // then the destination register is not modified and an Integer Overflow exception occurs.
            Instruction::Sub(rs, rt, rd, _) => {
                let res = self.op(rs, rt, |a, b| ((a as i32) - (b as i32)) as u32);
                self.set_register(rd, res);
            }

            Instruction::Subu(rs, rt, rd, _) => {
                let res = self.op(rs, rt, |a, b| a - b);
                self.set_register(rd, res);
            }

            Instruction::Sw(base, rt, offset) => {
                let dest_adr = (self.read_register(base) as i32 + offset as i32) as u32;
                self.write_word(dest_adr as usize, self.read_register(rt));
            }
        }
        Ok(instruction)
    }

    /// Get the current word stored in a given register
    pub fn read_register(&self, r: Register) -> u32 {
        self.registers[r as usize]
    }

    /// Set a register to a given value
    fn set_register(&mut self, r: Register, value: u32) {
        // You are not allowed to assign values to register $zero.
        if r == Register::Zero {
            return;
        }
        self.registers[r as usize] = value;
    }
}

impl Default for Machine {
    fn default() -> Self {
        Self::new()
    }
}
