mod error;
mod instructions;
mod machine;
pub use self::error::Error;
pub use self::instructions::*;
pub use self::machine::*;

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
