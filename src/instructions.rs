use crate::error::Error;

/// All of  the general purpose registers that
/// are allowed to be used.
///
/// Reference: <https://s3-eu-west-1.amazonaws.com/downloads-mips/documents/MD00086-2B-MIPS32BIS-AFP-6.06.pdf>
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Register {
    /// `$zero` is always set to zero.
    Zero = 0,

    /// Assembler Temporary, may not be used.
    At = 1,

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

impl TryFrom<&str> for Register {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use Register::*;
        match value {
            "$zero" => Ok(Zero),
            "$at" => Ok(At),
            "$v0" => Ok(V0),
            "$v1" => Ok(V1),
            "$a0" => Ok(A0),
            "$a1" => Ok(A1),
            "$a2" => Ok(A2),
            "$a3" => Ok(A3),
            "$t0" => Ok(T0),
            "$t1" => Ok(T1),
            "$t2" => Ok(T2),
            "$t3" => Ok(T3),
            "$t4" => Ok(T4),
            "$t5" => Ok(T5),
            "$t6" => Ok(T6),
            "$t7" => Ok(T7),
            "$t8" => Ok(T8),
            "$t9" => Ok(T9),
            "$s0" => Ok(S0),
            "$s1" => Ok(S1),
            "$s2" => Ok(S2),
            "$s3" => Ok(S3),
            "$s4" => Ok(S4),
            "$s5" => Ok(S5),
            "$s6" => Ok(S6),
            "$s7" => Ok(S7),
            "$k0" => Ok(K0),
            "$k1" => Ok(K1),
            "$gp" => Ok(Gp),
            "$sp" => Ok(Sp),
            "$fp" => Ok(Fp),
            "$ra" => Ok(Ra),
            other => {
                if other.starts_with('$') {
                    let number = other
                        .chars()
                        .skip(1)
                        .collect::<String>()
                        .parse::<u32>()
                        .map_err(|_| Error::InvalidRegisterName(other.into()))?;
                    Register::try_from(number)
                } else {
                    Err(Error::InvalidRegisterName(other.into()))
                }
            }
        }
    }
}

impl TryFrom<u32> for Register {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        use Register::*;
        match value {
            0 => Ok(Zero),
            1 => Ok(At),
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

type Rs = Register;
type Rt = Register;
type Rd = Register;
/// **Note:** Only 5 bits are allowed to be used
type Shift = u8;
type Immediate = i16;

/// Machine instructions (a subset of MIPS32).
///
/// Reference: <https://s3-eu-west-1.amazonaws.com/downloads-mips/documents/MD00086-2B-MIPS32BIS-AFP-6.06.pdf>
#[derive(Debug, PartialEq)]
pub enum Instruction {
    /// Add
    Add(Rs, Rt, Rd, Shift),
    /// Add Immediate
    Addi(Rs, Rt, Immediate),
    /// Add Unsigned Immediate
    Addiu(Rs, Rt, Immediate),
    /// Add Unsigned
    Addu(Rs, Rt, Rd, Shift),
    /// Bitwise AND
    And(Rs, Rt, Rd, Shift),
    /// Bitwise AND Immediate
    Andi(Rs, Rt, Immediate),
    /// Branch if Equal
    Beq(Rs, Rt, Immediate),
    /// Branch if Less Than or Equal to Zero
    Blez(Rs, Rt, Immediate),
    /// Branch if Not Equal
    Bne(Rs, Rt, Immediate),
    /// Branch on Greater Than Zero
    Bgtz(Rs, Rt, Immediate),
    /// Divide
    DivOLD(Rs, Rt, Rd, Shift),
    Div(Rs, Rt, Rd, Shift),
    /// Mod
    Mod(Rs, Rt, Rd, Shift),
    /// Unsigned Divide
    DivuOLD(Rs, Rt, Rd, Shift),
    Divu(Rs, Rt, Rd, Shift),
    /// Unsigned Mod
    Modu(Rs, Rt, Rd, Shift),
    /// Jump to Address
    J(u32),
    /// Jump and Link
    Jal(u32),
    /// Jump and Link Register
    Jalr(Rs, Rt, Rd, Shift),
    /// Jump to Address in Register
    Jr(Rs, Rt, Rd, Shift),
    /// Load Byte
    Lb(Rs, Rt, Immediate),
    /// Load Byte Unsigned
    Lbu(Rs, Rt, Immediate),
    /// Load Halfword Unsigned
    Lhu(Rs, Rt, Immediate),
    /// Load Upper Immediate
    Lui(Rs, Rt, Immediate),
    /// Load Word
    Lw(Rs, Rt, Immediate),
    /// Move from HI Register
    Mfhi(Rs, Rt, Rd, Shift),
    /// Move to HI Register
    Mthi(Rs, Rt, Rd, Shift),
    /// Move from LO Register
    Mflo(Rs, Rt, Rd, Shift),
    /// Move to LO Register
    Mtlo(Rs, Rt, Rd, Shift),
    /// Move from Coprocessor 0
    Mfc0(Rs, Rt, Rd, Shift),
    /// Multiply
    Mult(Rs, Rt, Rd, Shift),
    /// Unsigned Multiply
    Multu(Rs, Rt, Rd, Shift),
    /// Bitwise NOR
    Nor(Rs, Rt, Rd, Shift),
    /// Bitwise XOR (Exclusive-OR)
    Xor(Rs, Rt, Rd, Shift),
    /// Bitwise OR
    Or(Rs, Rt, Rd, Shift),
    /// Bitwise OR Immediate
    Ori(Rs, Rt, Immediate),
    /// Store Byte
    Sb(Rs, Rt, Immediate),
    /// Store Halfword
    Sh(Rs, Rt, Immediate),
    /// Set to 1 if Less Than
    Slt(Rs, Rt, Rd, Shift),
    /// Set to 1 if Less Than Immediate
    Slti(Rs, Rt, Immediate),
    /// Set to 1 if Less Than Unsigned Immediate
    Sltiu(Rs, Rt, Immediate),
    /// Set to 1 if Less Than Unsigned
    Sltu(Rs, Rt, Rd, Shift),
    /// Logical Shift Left
    Sll(Rs, Rt, Rd, Shift),
    /// Logical Shift Right (0-extended)
    Srl(Rs, Rt, Rd, Shift),
    /// Arithmetic Shift Right (sign-extended)
    Sra(Rs, Rt, Rd, Shift),
    /// Subtract
    Sub(Rs, Rt, Rd, Shift),
    /// Unsigned Subtract
    Subu(Rs, Rt, Rd, Shift),
    /// Store Word
    Sw(Rs, Rt, Immediate),
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

    let rs = ((value >> 21) & MASK5).try_into()?;
    let rt = ((value >> 16) & MASK5).try_into()?;
    let rd = ((value >> 11) & MASK5).try_into()?;

    // mfc0 is the only R format instruction with a opcode that is not 0x00
    if opcode == 0x10 {
        return Ok(Instruction::Mfc0(rd, rs, rt, shift as u8));
    }

    // Find the correct instruction based on the funct
    type InstrConstructor = Option<fn(Rs, Rt, Rd, Shift) -> Instruction>;
    let instruction: InstrConstructor = match funct {
        0x20 => Some(Instruction::Add),
        0x21 => Some(Instruction::Addu),
        0x24 => Some(Instruction::And),
        0x1A => match shift {
            // MIPS32 version 6 operations
            0x02 => Some(Instruction::Div),
            0x03 => Some(Instruction::Mod),
            _ => None, // TODO: We don't handle DivOLD, the Div instruction before MIPS32 version 6
        },
        0x1B => match shift {
            // MIPS32 version 6 operations
            0x02 => Some(Instruction::Divu),
            0x03 => Some(Instruction::Modu),
            _ => None, // TODO: We don't handle DivuOLD, the Divu instruction before MIPS32 version 6
        },
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
        |i| Ok(i(rs, rt, rd, shift as u8)),
    )
}

/// Convert a raw u32 to a Instruction written in the I format:
/// `opcode 6 | rs 5 | rt 5 | IMM 16`. Where IMM is the 16 bit immediate value.
fn from_i_format(value: u32) -> Result<Instruction, Error> {
    let opcode = value >> 26;
    let rs = ((value >> 21) & MASK5).try_into()?;
    let rt = ((value >> 16) & MASK5).try_into()?;
    let immediate = (value & 0xFFFF) as i16;

    let instruction: Option<fn(Rs, Rt, Immediate) -> Instruction> = match opcode {
        0x08 => Some(Instruction::Addi),
        0x0C => Some(Instruction::Andi),
        0x04 => Some(Instruction::Beq),
        0x06 => Some(Instruction::Blez),
        0x05 => Some(Instruction::Bne),
        0x07 => Some(Instruction::Bgtz),
        0x09 => Some(Instruction::Addiu),
        0x20 => Some(Instruction::Lb),
        // TODO: saknas inte `Lh`? I guess many more.
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
        |i| Ok(i(rs, rt, immediate)),
    )
}
