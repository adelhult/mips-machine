use crate::error::Error;

/// All of  the general purpose registers that
/// are allowed to be used.
///
/// Reference: <https://en.wikibooks.org/wiki/MIPS_Assembly/Register_File>
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Register {
    /// `$zero` is always set to zero.
    Zero = 0,

    /// Assembler Temporary, may not be used.
    // At = 1,

    /// `$v0` is used for returning values from functions.
    /// Not preserved across function calls.
    V0 = 2,
    /// `$v1` is used for returning values from functions.
    /// Not preserved across function calls.
    V1 = 3,

    /// `$a0` is used for passing arguments to functions.
    /// Not preserved across function calls.
    A0 = 4,
    /// `$a1` is used for passing arguments to functions.
    /// Not preserved across function calls.
    A1 = 5,
    /// `$a2` is used for passing arguments to functions.
    /// Not preserved across function calls.
    A2 = 6,
    /// `$a3` is used for passing arguments to functions.
    /// Not preserved across function calls.
    A3 = 7,

    /// `$t0` is a temporary register.
    /// Not preserved across function calls.
    T0 = 8,
    /// `$t1` is a temporary register.
    /// Not preserved across function calls.
    T1 = 9,
    /// `$t2` is a temporary register.
    /// Not preserved across function calls.
    T2 = 10,
    /// `$t3` is a temporary register.
    /// Not preserved across function calls.
    T3 = 11,
    /// `$t4` is a temporary register.
    /// Not preserved across function calls.
    T4 = 12,
    /// `$t5` is a temporary register.
    /// Not preserved across function calls.
    T5 = 13,
    /// `$t6` is a temporary register.
    /// Not preserved across function calls.
    T6 = 14,
    /// `$t7` is a temporary register.
    /// Not preserved across function calls.
    T7 = 15,
    /// `$t8` is a temporary register.
    /// Not preserved across function calls.
    T8 = 24,
    /// `$t9` is a temporary register.
    /// Not preserved across function calls.
    T9 = 25,

    /// `$s0` is a **saved** temporary register.
    /// Preserved across function calls.  
    S0 = 16,
    /// `$s1` is a **saved** temporary register.
    /// Preserved across function calls.  
    S1 = 17,
    /// `$s2` is a **saved** temporary register.
    /// Preserved across function calls.
    S2 = 18,
    /// `$s3` is a **saved** temporary register.
    /// Preserved across function calls.
    S3 = 19,
    /// `$s4` is a **saved** temporary register.
    /// Preserved across function calls.
    S4 = 20,
    /// `$s5` is a **saved** temporary register.
    /// Preserved across function calls.
    S5 = 21,
    /// `$s6` is a **saved** temporary register.
    /// Preserved across function calls.
    S6 = 22,
    /// `$s7` is a **saved** temporary register.
    /// Preserved across function calls.
    S7 = 23,

    /// `$k0` is reserved for use by the OS kernel.
    K0 = 26,
    /// `$k1` is reserved for use by the OS kernel.
    K1 = 27,

    /// `$gp` is the so called "global pointer". A pointer to the global data area.
    Gp = 28,

    /// `$sp`. Stack pointer.
    Sp = 29,

    /// `$sp`. Frame pointer.
    Fp = 30,

    /// Return address
    Ra = 31,
}

impl TryFrom<u32> for Register {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        use Register::*;
        match value {
            0 => Ok(Zero),
            2 => Ok(V0),
            3 => Ok(V1),
            4 => Ok(A0),
            5 => Ok(A1),
            6 => Ok(A2),
            7 => Ok(A3),
            8 => Ok(T0),
            9 => Ok(T1),
            10 => Ok(T2),
            11 => Ok(T3),
            12 => Ok(T4),
            13 => Ok(T5),
            14 => Ok(T6),
            15 => Ok(T7),
            16 => Ok(S0),
            17 => Ok(S1),
            18 => Ok(S2),
            19 => Ok(S3),
            20 => Ok(S4),
            21 => Ok(S5),
            22 => Ok(S6),
            23 => Ok(S7),
            24 => Ok(T8),
            25 => Ok(T9),
            26 => Ok(K0),
            27 => Ok(K1),
            28 => Ok(Gp),
            29 => Ok(Sp),
            30 => Ok(Fp),
            31 => Ok(Ra),
            n => Err(Error::InvalidRegisterNumber(n)),
        }
    }
}

/// All of the machine instructions
///
/// Reference: <https://en.wikibooks.org/wiki/MIPS_Assembly/Instruction_Formats>
#[derive(Debug, PartialEq)]
pub enum Instruction {
    /// Add
    Add(Register, Register, Register, u8),
    /// Add Immediate
    Addi(Register, Register, u16),
    /// Add Unsigned Immediate
    Addiu(Register, Register, u16),
    /// Add Unsigned
    Addu(Register, Register, Register, u8),
    /// Bitwise AND
    And(Register, Register, Register, u8),
    /// Bitwise AND Immediate
    Andi(Register, Register, u16),
    /// Branch if Equal
    Beq(Register, Register, u16),
    /// Branch if Less Than or Equal to Zero
    Blez(Register, Register, u16),
    /// Branch if Not Equal
    Bne(Register, Register, u16),
    /// Branch on Greater Than Zero
    Bgtz(Register, Register, u16),
    /// Divide
    Div(Register, Register, Register, u8),
    /// Unsigned Divide
    Divu(Register, Register, Register, u8),
    /// Jump to Address
    J(u32),
    /// Jump and Link
    Jal(u32),
    /// Jump and Link Register
    Jalr(Register, Register, Register, u8),
    /// Jump to Address in Register
    Jr(Register, Register, Register, u8),
    /// Load Byte
    Lb(Register, Register, u16),
    /// Load Byte Unsigned
    Lbu(Register, Register, u16),
    /// Load Halfword Unsigned
    Lhu(Register, Register, u16),
    /// Load Upper Immediate
    Lui(Register, Register, u16),
    /// Load Word
    Lw(Register, Register, u16),
    /// Move from HI Register
    Mfhi(Register, Register, Register, u8),
    /// Move to HI Register
    Mthi(Register, Register, Register, u8),
    /// Move from LO Register
    Mflo(Register, Register, Register, u8),
    /// Move to LO Register
    Mtlo(Register, Register, Register, u8),
    /// Move from Coprocessor 0
    Mfc0(Register, Register, Register, u8),
    /// Multiply
    Mult(Register, Register, Register, u8),
    /// Unsigned Multiply
    Multu(Register, Register, Register, u8),
    /// Bitwise NOR
    Nor(Register, Register, Register, u8),
    /// Bitwise XOR (Exclusive-OR)
    Xor(Register, Register, Register, u8),
    /// Bitwise OR
    Or(Register, Register, Register, u8),
    /// Bitwise OR Immediate
    Ori(Register, Register, u16),
    /// Store Byte
    Sb(Register, Register, u16),
    /// Store Halfword
    Sh(Register, Register, u16),
    /// Set to 1 if Less Than
    Slt(Register, Register, Register, u8),
    /// Set to 1 if Less Than Immediate
    Slti(Register, Register, u16),
    /// Set to 1 if Less Than Unsigned Immediate
    Sltiu(Register, Register, u16),
    /// Set to 1 if Less Than Unsigned
    Sltu(Register, Register, Register, u8),
    /// Logical Shift Left
    Sll(Register, Register, Register, u8),
    /// Logical Shift Right (0-extended)
    Srl(Register, Register, Register, u8),
    /// Arithmetic Shift Right (sign-extended)
    Sra(Register, Register, Register, u8),
    /// Subtract
    Sub(Register, Register, Register, u8),
    /// Unsigned Subtract
    Subu(Register, Register, Register, u8),
    /// Store Word
    Sw(Register, Register, u16),
}

impl TryFrom<u32> for Instruction {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let opcode = value >> 26;
        match opcode {
            0x00 | 0x10 => from_r_format(value),
            0x02 => from_j_format(value), // Instruction::J(...),
            0x03 => from_j_format(value), // Instruction::Jal(...),
            _ => from_i_format(value),
        }
    }
}

const MASK6: u32 = 0b111111;
const MASK5: u32 = 0b11111;

/// Convert a raw u32 to a Instruction written in the J format:
/// `opcode 6 | address 26`
fn from_j_format(value: u32) -> Result<Instruction, Error> {
    let opcode = (value >> 26) & MASK6;
    let pseudo_addr = value & 0x3ffffff; // 26 bitmask
    match opcode {
        0x02 => Ok(Instruction::J(pseudo_addr)),
        0x03 => Ok(Instruction::Jal(pseudo_addr)),
        _ => Err(Error::InvalidInstruction(value)),
    }
}

/// Convert a raw u32 to a Instruction written in the R format:
/// `opcode 6 | rs 5 | rt 5 | rd 5 | shift 5 | funct 6`
fn from_r_format(value: u32) -> Result<Instruction, Error> {
    let funct = value & MASK6;
    let shift = (value >> 6) & MASK5;
    let opcode = (value >> 26) & MASK6;

    let destination_r = ((value >> 11) & MASK5).try_into()?;
    let source1_r = ((value >> 16) & MASK5).try_into()?;
    let source0_r = ((value >> 21) & MASK5).try_into()?;

    // mfc0 is the only R format instruction with a opcode that is not 0x00
    if opcode == 0x10 {
        return Ok(Instruction::Mfc0(
            destination_r,
            source0_r,
            source1_r,
            shift as u8,
        ));
    }

    // Find the correct instruction based on the funct
    type InstrConstructor = Option<fn(Register, Register, Register, u8) -> Instruction>;
    let instruction: InstrConstructor = match funct {
        0x20 => Some(Instruction::Add),
        0x21 => Some(Instruction::Addu),
        0x24 => Some(Instruction::And),
        0x1A => Some(Instruction::Div),
        0x1B => Some(Instruction::Divu),
        0x09 => Some(Instruction::Jalr),
        0x08 => Some(Instruction::Jr),
        0x10 => Some(Instruction::Mfhi),
        0x11 => Some(Instruction::Mthi),
        0x12 => Some(Instruction::Mflo),
        0x13 => Some(Instruction::Mfc0),
        0x18 => Some(Instruction::Mult),
        0x19 => Some(Instruction::Multu),
        0x27 => Some(Instruction::Nor),
        0x26 => Some(Instruction::Xor),
        0x25 => Some(Instruction::Or),
        0x2A => Some(Instruction::Slt),
        0x2B => Some(Instruction::Sltu),
        0x00 => Some(Instruction::Sll),
        0x02 => Some(Instruction::Srl),
        0x03 => Some(Instruction::Sra),
        0x22 => Some(Instruction::Sub),
        0x23 => Some(Instruction::Subu),
        _ => None,
    };

    // Attach the three registers to the instruction constructor or
    // return an error
    instruction.map_or_else(
        || Err(Error::InvalidInstruction(value)),
        |i| Ok(i(destination_r, source0_r, source1_r, shift as u8)),
    )
}

/// Convert a raw u32 to a Instruction written in the I format:
/// `opcode 6 | rs 5 | rt 5 | IMM 16`. Where IMM is the 16 bit immediate value.
fn from_i_format(value: u32) -> Result<Instruction, Error> {
    let opcode = value >> 26;
    let source0_r = ((value >> 21) & MASK5).try_into()?;
    let source1_r = ((value >> 16) & MASK5).try_into()?;
    let immediate = value & 0xFF;

    let instruction: Option<fn(Register, Register, u16) -> Instruction> = match opcode {
        0x08 => Some(Instruction::Addi),
        0x0C => Some(Instruction::Andi),
        0x04 => Some(Instruction::Beq),
        0x06 => Some(Instruction::Blez),
        0x05 => Some(Instruction::Bne),
        0x07 => Some(Instruction::Bgtz),
        0x09 => Some(Instruction::Addiu),
        0x20 => Some(Instruction::Lb),
        0x24 => Some(Instruction::Lbu),
        0x25 => Some(Instruction::Lhu),
        0x0F => Some(Instruction::Lui),
        0x23 => Some(Instruction::Lw),
        0x0D => Some(Instruction::Ori),
        0x28 => Some(Instruction::Sb),
        0x29 => Some(Instruction::Sh),
        0x0A => Some(Instruction::Slti),
        0x0B => Some(Instruction::Sltiu),
        0x2B => Some(Instruction::Sw),
        _ => None,
    };

    instruction.map_or_else(
        || Err(Error::InvalidInstruction(value)),
        |i| Ok(i(source0_r, source1_r, immediate as u16)),
    )
}
