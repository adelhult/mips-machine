use crate::{Error, Instruction, Register, DATA_BASE_ADDRESS, MEMORY_SIZE, TEXT_BASE_ADDRESS};
use num_traits::Num;
use std::{
    collections::HashMap,
    str::{Chars, FromStr},
};

pub type Labels = HashMap<String, usize>;

#[derive(Debug, Copy, Clone)]
enum Cursor {
    Data,
    Text,
}

#[derive(Debug)]
pub struct Parser<'a> {
    data_cursor: usize,
    text_cursor: usize,
    active_cursor: Cursor,
    source: Chars<'a>,
    in_string: bool,
    in_escape: bool,
    in_comment: bool,
    labels: Labels,
    memory: [u8; MEMORY_SIZE],
}

impl<'a> Parser<'a> {
    /// Create a new parser and assemble the provided source code
    pub fn new(source: &'a str) -> Self {
        Self {
            data_cursor: DATA_BASE_ADDRESS,
            text_cursor: TEXT_BASE_ADDRESS,
            active_cursor: Cursor::Text,
            in_string: false,
            in_escape: false,
            in_comment: false,
            source: source.chars(),
            labels: HashMap::new(),
            memory: [0; MEMORY_SIZE],
        }
    }

    /// Consumes the parsers and returns the memory of the assembled program
    /// as well as all of the labels defined in the program
    pub fn assemble(mut self) -> Result<([u8; MEMORY_SIZE], Labels), Error> {
        while let Some(token) = self.consume_token() {
            if let Some(directive) = token.strip_prefix('.') {
                // directives is written by using a period followed by a identifier
                self.parse_directive(directive.into())?;
            } else if let Some(label) = token.strip_suffix(':') {
                // labels are a single word identifier ending with a colon
                self.labels.insert(label.into(), self.current_cursor());
            } else {
                // all other tokens are assumed to be instructions
                let instructions = self.parse_instruction(token)?;
                for instruction in instructions {
                    self.place_word(instruction.into())?;
                }
            }
        }

        Ok((self.memory, self.labels))
    }

    /// Consume a token from the input text
    /// TODO: rewrite this in a cleaner way using seperate functions that take a peekable
    /// iterator, instead of having all these flags like I currently have (in_comment, in_string...)
    fn consume_token(&mut self) -> Option<String> {
        let mut token = String::new();

        for c in self.source.by_ref() {
            if self.in_comment {
                if c == '\n' {
                    self.in_comment = false;
                    // don't just to the next token so that the \n
                    // also can act as a token delim
                } else {
                    // ignore everything inside comments
                    continue;
                }
            }

            // start espace sequence
            if c == '\\' {
                self.in_escape = true;
                continue;
            }

            // end of escape sequence
            if self.in_escape {
                token.push(match c {
                    'n' => '\n',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '"',
                    c => c,
                });
                continue;
            }

            if c == '"' {
                self.in_string = !self.in_string;
                continue;
            }

            // comments start with a pound sign
            if c == '#' {
                self.in_comment = true;
                continue;
            }

            // tokens are delim. by whitespace or break lines, a parenthesis or a comma
            // FIXME: It is a really hacky solution to treat '(' and ')' as delimeters
            // instead of individual tokens. It works fine when parsing things like
            // `lb rt, offset(base)` but it also allows for `lb rt, offset, base`
            // or horrendous things like `addi(rs(rt))10`.
            if !self.in_string
                && !token.is_empty()
                && (c.is_whitespace() || c == ',' || c == '(' || c == ')')
            {
                return Some(token.trim().to_string());
            }

            // extra whitespaces characters should be skipped
            if c.is_whitespace() && !self.in_string {
                continue;
            }
            // finally, we can add the character to the current token
            token.push(c);
        }
        // return the final token if there is one
        if token.is_empty() {
            None
        } else {
            Some(token.trim().to_string())
        }
    }

    fn try_consume_operand<T: FromStr>(&mut self, error_msg: &str) -> Result<T, Error> {
        self.consume_token()
            .ok_or_else(|| Error::BadOperand(error_msg.into()))?
            .parse::<_>()
            .map_err(|_| Error::BadOperand(error_msg.into()))
    }

    /// Attempt to consume a number literal
    fn try_consume_number<T: Num + FromStr>(&mut self, error_msg: &str) -> Result<T, Error> {
        let token = self
            .consume_token()
            .ok_or_else(|| Error::BadOperand(error_msg.to_string()))?;

        // is it a literal written in decimal form?
        if let Ok(number) = token.parse::<T>() {
            return Ok(number);
        }

        // is it a hex literal 0x...?
        if let Some(number) = token.strip_prefix("0x") {
            return T::from_str_radix(number, 16)
                .map_err(|_| Error::BadOperand(error_msg.to_string()));
        }

        // is it a binary literal 0b...?
        if let Some(number) = token.strip_prefix("0b") {
            return T::from_str_radix(number, 2)
                .map_err(|_| Error::BadOperand(error_msg.to_string()));
        }

        todo!()
    }

    fn try_consume_register(&mut self, error_msg: &str) -> Result<Register, Error> {
        Register::try_from(self.try_consume_operand::<String>(error_msg)?.as_str())
    }

    /// returns the position of the cursor
    fn current_cursor(&self) -> usize {
        match self.active_cursor {
            Cursor::Data => self.data_cursor,
            Cursor::Text => self.text_cursor,
        }
    }

    // return the value of the cursor and increment it
    fn increment_cursor(&mut self) -> usize {
        let current = self.current_cursor();
        match self.active_cursor {
            Cursor::Data => self.data_cursor += 1,
            Cursor::Text => self.text_cursor += 1,
        }
        current
    }

    /// places a byte at the cursor location and then increment it
    fn place_byte(&mut self, byte: u8) -> Result<(), Error> {
        let current = self.current_cursor();
        if current >= self.memory.len() {
            return Err(Error::AddressOutOfRange(current));
        }
        self.memory[current] = byte;
        self.increment_cursor();
        Ok(())
    }

    fn place_word(&mut self, word: u32) -> Result<(), Error> {
        self.place_byte((word >> 24) as u8)?;
        self.place_byte((word >> 16) as u8)?;
        self.place_byte((word >> 8) as u8)?;
        self.place_byte(word as u8)?;
        Ok(())
    }

    fn parse_directive(&mut self, directive: String) -> Result<(), Error> {
        match directive.as_str() {
            "ascii" => {
                let string: String = self.try_consume_operand(".ascii takes a string")?;
                if !string.is_ascii() {
                    return Err(Error::BadOperand(
                        ".ascii only supports ascii characters".into(),
                    ));
                }

                for b in string.as_bytes().iter() {
                    self.place_byte(*b)?;
                }

                Ok(())
            }
            "asciiz" => {
                let string: String = self.try_consume_operand(".asciiz takes a string")?;
                if !string.is_ascii() {
                    return Err(Error::BadOperand(
                        ".ascii only supports ascii characters".into(),
                    ));
                }

                for b in string.as_bytes().iter() {
                    self.place_byte(*b)?;
                }
                self.place_byte(0)?;

                Ok(())
            }
            "byte" => {
                let byte: u8 =
                    self.try_consume_number::<i8>(".byte takes a byte as an operand")? as u8;
                self.place_byte(byte)?;
                Ok(())
            }
            "data" => {
                self.active_cursor = Cursor::Data;
                Ok(())
            }
            "globl" => Ok(()), // NOTE: this assembler has no notion of scope,
            "half" => {
                let half: u16 =
                    self.try_consume_number::<i16>(".half takes a halfword as an operand")? as u16;
                self.place_byte(half as u8)?;
                self.place_byte((half >> 8) as u8)?;
                Ok(())
            }
            "space" => {
                let n: u8 = self.try_consume_number(".space takes an integer as an operand")?;
                for _ in 0..n {
                    self.place_byte(0)?;
                }
                Ok(())
            }
            "text" => {
                self.active_cursor = Cursor::Text;
                Ok(())
            }
            "word" => {
                let word: u32 = self
                    .try_consume_number::<i32>(".word takes a 32-bit word as an operand")?
                    as u32;
                self.place_word(word)?;
                Ok(())
            }
            //TODO: "align" => (),
            _ => Err(Error::InvalidDirective(directive)),
        }
    }

    fn parse_instruction(&mut self, operation: String) -> Result<Vec<Instruction>, Error> {
        use Instruction::*;
        // FIXME: note that some instructions like mfhi and mult etc. are not included in the parsing
        // since they are currently just implemented as TODOS and throws an Error in the virtual machine.
        match operation.to_lowercase().as_str() {
            "add" => self.parse_r_format(Add, Some(0)),
            "addu" => self.parse_r_format(Addu, Some(0)),
            "and" => self.parse_r_format(And, Some(0)),
            "div" => self.parse_r_format(Div, Some(0)),
            "mod" => self.parse_r_format(Mod, Some(0)),
            "divu" => self.parse_r_format(Divu, Some(0)),
            "modu" => self.parse_r_format(Modu, Some(0)),
            "jalr" => {
                // jalr $rd, $rs
                let rd = Register::try_from(
                    self.try_consume_operand::<String>("jalr needs a register $rd")?
                        .as_str(),
                )?;
                let rs = Register::try_from(
                    self.try_consume_operand::<String>("jalr needs a register $rs")?
                        .as_str(),
                )?;
                Ok(vec![Instruction::Jalr(rs, Register::Unused, rd, 0)])
            }
            "jr" => {
                // jl $rs
                let rs = Register::try_from(
                    self.try_consume_operand::<String>("jl needs a register $rs")?
                        .as_str(),
                )?;
                Ok(vec![Instruction::Jr(
                    rs,
                    Register::Unused,
                    Register::Unused,
                    0,
                )])
            }
            "nor" => self.parse_r_format(Nor, Some(0)),
            "xor" => self.parse_r_format(Xor, Some(0)),
            "or" => self.parse_r_format(Or, Some(0)),
            "slt" => self.parse_r_format(Slt, Some(0)),
            "sltu" => self.parse_r_format(Sltu, Some(0)),
            "sll" => self.parse_r_format(Sll, None),
            "srl" => self.parse_r_format(Srl, None),
            "sra" => self.parse_r_format(Sra, None),
            "sub" => self.parse_r_format(Sub, Some(0)),
            "subu" => self.parse_r_format(Subu, Some(0)),
            "j" => {
                let target_label: String = self.try_consume_operand("J needs a label")?;
                let instr_index = self.calc_jump_offset(target_label)?;
                Ok(vec![Jal(instr_index as u32)])
            }
            "jal" => {
                let target_label: String = self.try_consume_operand("J needs a label")?;
                let instr_index = self.calc_jump_offset(target_label)?;
                Ok(vec![Jal(instr_index as u32)])
            }
            "addi" => self.parse_i_format(Addi),
            "addiu" => self.parse_i_format(Addiu),
            "andi" => self.parse_i_format(Andi),
            "beq" => {
                // beq rs, rt, label
                let rs = self.try_consume_register("beq needs a register \"rs\"")?;
                let rt = self.try_consume_register("beq needs a register \"rt\"")?;
                let label = self.try_consume_operand("beq needs a label")?;

                Ok(vec![Beq(rs, rt, self.calc_branch_offset(label)?)])
            }
            "blez" => {
                // blez rs, label
                let rs = self.try_consume_register("blez needs a register \"rs\"")?;
                let label = self.try_consume_operand("bne needs a label")?;

                Ok(vec![Blez(
                    rs,
                    Register::Unused,
                    self.calc_branch_offset(label)?,
                )])
            }
            "bne" => {
                // bne rs, rt, label
                let rs = self.try_consume_register("bne needs a register \"rs\"")?;
                let rt = self.try_consume_register("bne needs a register \"rt\"")?;
                let label = self.try_consume_operand("bne needs a label")?;

                Ok(vec![Bne(rs, rt, self.calc_branch_offset(label)?)])
            }
            "bgtz" => {
                // bgtz rs, label
                let rs = self.try_consume_register("bgtz needs a register \"rs\"")?;
                let label = self.try_consume_operand("bgtz needs a label")?;

                Ok(vec![Bgtz(
                    rs,
                    Register::Unused,
                    self.calc_branch_offset(label)?,
                )])
            }
            "lb" => self.parse_offset_format(Lb),
            "lbu" => self.parse_offset_format(Lbu),
            "lhu" => self.parse_offset_format(Lhu),
            "lw" => self.parse_offset_format(Lw),
            "lui" => {
                // lui rt, imm
                let rt = self.try_consume_register("lui needs a register \"rt\"")?;
                let imm = self.try_consume_number("lui needs a immediate value")?;
                Ok(vec![Instruction::Lui(Register::Unused, rt, imm)])
            }
            "ori" => self.parse_i_format(Ori),
            "sb" => self.parse_offset_format(Sb),
            "sh" => self.parse_offset_format(Sh),
            "slti" => self.parse_i_format(Slti),
            "sltiu" => self.parse_i_format(Sltiu),
            "sw" => self.parse_offset_format(Sw),
            "nop" => Ok(vec![Sll(Register::Zero, Register::Zero, Register::Zero, 0)]),
            "li" => {
                // li rs, imm
                let r = self.try_consume_register("li needs a register")?;
                let imm: u32 = self.try_consume_number::<i32>("li needs a imm value")? as u32;
                if let Ok(value) = u16::try_from(imm) {
                    Ok(vec![Addi(Register::Zero, r, value as i16)])
                } else {
                    // the imm value does not fit in the 16 bits of a single addi,
                    // use both lui and ori instead
                    Ok(vec![
                        Lui(Register::Zero, r, (imm >> 16) as i16),
                        Ori(r, r, (imm & 0xffff) as i16),
                    ])
                }
            }
            "la" => {
                // la rs, label
                let r = self.try_consume_register("la needs a register")?;
                let label: String = self.try_consume_operand("la needs a label")?;
                let addr = self.get_addr(&label)?;
                Ok(vec![
                    Lui(Register::Zero, r, (addr >> 16) as i16),
                    Ori(r, r, (addr & 0xffff) as i16),
                ])
            }
            token => Err(Error::Syntax(format!("Unknown token \"{token}\""))),
        }
    }

    fn get_addr(&self, label: &str) -> Result<usize, Error> {
        self.labels
            .get(label)
            .ok_or_else(|| Error::UndefinedLabel(label.to_string()))
            .map(|addr| *addr)
    }

    fn calc_jump_offset(&self, label: String) -> Result<usize, Error> {
        let mask_26bit = 0x3ffffff;
        let target_addr = self.get_addr(&label)?;
        // "The low 28 bits of the target address is the instr_index field shifted left 2bits.
        // The remaining upper bits are the corresponding bits of the
        // address of the instruction in the delay slot (not the branch itself).""

        Ok((target_addr >> 2) & mask_26bit)
    }

    fn calc_branch_offset(&self, label: String) -> Result<i16, Error> {
        let target_addr = self.get_addr(&label)?;
        let next_instr = self.current_cursor() + 4;
        // "An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) is added to the address of the instruction following
        // the branch (not the branch itself), in the branch delay slot, to form a PC-relative effective target address.""
        Ok(((target_addr - next_instr) >> 2) as i16)
    }

    /// Helper function to parse function like `lb rt, offset(base)`
    fn parse_offset_format(&mut self, op: IOperator) -> Result<Vec<Instruction>, Error> {
        let rt = self.try_consume_register("The instruction needs a register \"rt\"")?;
        let offset = self.try_consume_number("The instruction needs a register \"offset\"")?;
        let base = self.try_consume_register("The instruction needs a register \"base\"")?;
        Ok(vec![op(base, rt, offset)])
    }

    /// Helper function used to parse most of the r format instructions such as `add` and directly
    /// apply the operands values in the data type constructor.
    fn parse_r_format(
        &mut self,
        op: ROperator,
        default_shift: Option<u8>,
    ) -> Result<Vec<Instruction>, Error> {
        let rd = self.try_consume_register("The instruction needs a register \"rd\"")?;
        let rs = self.try_consume_register("The instruction needs a register \"rs\"")?;
        let rt = self.try_consume_register("The instruction needs a register \"rt\"")?;

        let shift = match default_shift {
            Some(default) => default,
            None => self.try_consume_number("The instruction needs a shift amount")?,
        };

        Ok(vec![op(rs, rt, rd, shift)])
    }

    /// Helper function used to parse common i format instructions such as `addi $rt $rs imm`
    fn parse_i_format(&mut self, op: IOperator) -> Result<Vec<Instruction>, Error> {
        let rt = self.try_consume_register("The instruction needs a register \"rt\"")?;
        let rs = self.try_consume_register("The instruction needs a register \"rs\"")?;

        let imm = self.try_consume_number("The instruction needs a immediate value")?;
        Ok(vec![op(rs, rt, imm)])
    }
}

type ROperator = fn(Register, Register, Register, u8) -> Instruction;
type IOperator = fn(Register, Register, i16) -> Instruction;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizer() {
        let mut parser = Parser::new("foo a, b, c");
        assert_eq!(
            [
                parser.consume_token(),
                parser.consume_token(),
                parser.consume_token(),
                parser.consume_token(),
                parser.consume_token()
            ],
            [
                Some("foo".to_string()),
                Some("a".to_string()),
                Some("b".to_string()),
                Some("c".to_string()),
                None,
            ]
        );
    }

    #[test]
    fn word_directive() {
        let source = ".byte 1\n.data\n.byte 0x2\n.byte 0b11";
        let (memory, _) = Parser::new(source).assemble().unwrap();
        assert_eq!(memory[TEXT_BASE_ADDRESS], 1);
        assert_eq!(memory[DATA_BASE_ADDRESS], 2);
        assert_eq!(memory[DATA_BASE_ADDRESS + 1], 3);
    }

    #[test]
    fn ascii_directive() {
        let source = ".data\n.ascii \"Abc\"";
        let ascii_string = [0x41, 0x62, 0x63];
        let (memory, _) = Parser::new(source).assemble().unwrap();
        assert_eq!(
            memory[DATA_BASE_ADDRESS..DATA_BASE_ADDRESS + 3],
            ascii_string
        );
    }
}
