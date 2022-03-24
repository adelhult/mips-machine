mod instructions;
mod error;
pub use self::error::Error;
pub use self::instructions::*;

pub const MEMORY_SIZE: usize = 1024;

pub struct Machine {
    pc: usize,
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
        self.pc = value as usize;
    }

    /// Read a word starting at the given address
    pub fn read_word(&self, address: usize) -> u32 {
        ((self.memory[address]     as u32) << 24) +
        ((self.memory[address + 1] as u32) << 16) +
        ((self.memory[address + 2] as u32) <<  8) +
         (self.memory[address + 3] as u32)
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
        let instruction: Instruction = self.read_word(self.pc).try_into()?;
        match instruction {
            Instruction::Add(dest, source, target, _) => {
                let sum = self.op(source, target, |a, b| {
                    (a as i32 + b as i32) as u32
                });                
                self.set_register(dest, sum);
            }
            Instruction::Addi(_, _, _) => todo!(),
            Instruction::Addiu(_, _, _) => todo!(),

            Instruction::Addu(dest, source, target, _) => {
                let sum = self.op(source, target, |a, b| a + b);                
                self.set_register(dest, sum);
            }

            Instruction::And(dest, source,target, _) => {
                self.set_register(dest, self.op(source, target, |a, b| a & b));
            },
            Instruction::Andi(_, _, _) => todo!(),
            Instruction::Beq(_, _, _) => todo!(),
            Instruction::Blez(_, _, _) => todo!(),
            Instruction::Bne(_, _, _) => todo!(),
            Instruction::Bgtz(_, _, _) => todo!(),
            Instruction::Div(_, _, _, _) => todo!(),
            Instruction::Divu(_, _, _, _) => todo!(),
            Instruction::J(_) => todo!(),
            Instruction::Jal(_) => todo!(),
            Instruction::Jalr(_, _, _, _) => todo!(),
            Instruction::Jr(_, _, _, _) => todo!(),
            Instruction::Lb(_, _, _) => todo!(),
            Instruction::Lbu(_, _, _) => todo!(),
            Instruction::Lhu(_, _, _) => todo!(),
            Instruction::Lui(_, _, _) => todo!(),
            Instruction::Lw(_, _, _) => todo!(),
            Instruction::Mfhi(_, _, _, _) => todo!(),
            Instruction::Mthi(_, _, _, _) => todo!(),
            Instruction::Mflo(_, _, _, _) => todo!(),
            Instruction::Mtlo(_, _, _, _) => todo!(),
            Instruction::Mfc0(_, _, _, _) => todo!(),
            Instruction::Mult(_, _, _, _) => todo!(),
            Instruction::Multu(_, _, _, _) => todo!(),
            Instruction::Nor(_, _, _, _) => todo!(),
            Instruction::Xor(_, _, _, _) => todo!(),
            Instruction::Or(_, _, _, _) => todo!(),
            Instruction::Ori(_, _, _) => todo!(),
            Instruction::Sb(_, _, _) => todo!(),
            Instruction::Sh(_, _, _) => todo!(),
            Instruction::Slt(_, _, _, _) => todo!(),
            Instruction::Slti(_, _, _) => todo!(),
            Instruction::Sltiu(_, _, _) => todo!(),
            Instruction::Sltu(_, _, _, _) => todo!(),
            Instruction::Sll(_, _, _, _) => todo!(),
            Instruction::Srl(_, _, _, _) => todo!(),
            Instruction::Sra(_, _, _, _) => todo!(),
            Instruction::Sub(_, _, _, _) => todo!(),
            Instruction::Subu(_, _, _, _) => todo!(),
            Instruction::Sw(_, _, _) => todo!(),
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
