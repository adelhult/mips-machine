use clap::Parser as CliParser;
use colour::{blue, green_ln, red_ln};
use mips_machine::{Error, Instruction, Machine, Parser, Register, TEXT_BASE_ADDRESS};
use read_input::prelude::*;
use std::{fs, path::PathBuf};

/// A simple MIPS virtual machine
#[derive(CliParser, Debug)]
#[clap(version, long_about = None)]
struct Args {
    /// A file path to a assembler or memory file
    input_file: Option<PathBuf>,

    /// The address from which the given memory should be placed
    #[clap(short = 'o', long, default_value_t = 0, value_name = "ADDRESS")]
    memory_origin: usize,

    /// Used to specify the value of the pc register
    #[clap(long, value_name = "ADDRESS")]
    pc: Option<usize>,
}

fn main() {
    let args = Args::parse();
    let mut machine: Machine;

    // create a new machine based on if the user provided a memory or assembly file
    if let Some(filename) = args.input_file {
        let code = match fs::read_to_string(&filename) {
            Ok(content) => content,
            Err(_) => {
                red_ln!("Sorry, I failed to read the file!");
                return;
            }
        };

        match filename
            .extension()
            .expect("File has no extension")
            .to_str()
            .unwrap()
        {
            // assemble the file and create a machine
            "asm" => {
                let parsing_result = Parser::new(&code).assemble();

                if let Err(error) = parsing_result {
                    red_ln!("{}", explain_error(error));
                    return;
                }

                let (memory, labels) = parsing_result.unwrap();

                machine = Machine::from(memory);

                // set pc to the value given by the user otherwise the main label
                // and as a final fallback the  the text base addr will be used.
                let pc = args
                    .pc
                    .or_else(|| labels.get("main").cloned())
                    .unwrap_or(TEXT_BASE_ADDRESS);

                machine.set_pc(pc as u32);

                println!("Created a machine based on the given assembly file.");
            }

            // create a machine based on the memory file
            "mem" => {
                if let Some(memory) = parse_mem_file(&code) {
                    machine = Machine::new();
                    machine.set_memory_from(&memory, 0);

                    if let Some(pc) = args.pc {
                        machine.set_pc(pc as u32);
                    }
                    println!("Created a machine based on the given memory file.");
                } else {
                    red_ln!("Sorry, I failed to parse the memory file!");
                    return;
                }
            }
            _ => {
                red_ln!("Invalid file extension, please use .asm or .mem.");
                return;
            }
        }
    } else {
        println!("Created a machine with the memory (and registers) all set to 0.");
        machine = Machine::new();

        if let Some(pc) = args.pc {
            machine.set_pc(pc as u32);
        }
    }

    start_interact_mode(&mut machine);
}

/// Starts a interactive REPL style session using the given machine
fn start_interact_mode(machine: &mut Machine) {
    loop {
        let command = get_command("> ");
        match command
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<&str>>()
            .as_slice()
        {
            ["help"] => help(),
            ["step", n] => {
                if let Ok(n) = n.parse() {
                    step(machine, n);
                } else {
                    red_ln!("Invalid number of steps \"{}\"!", n);
                }
            }
            ["step"] => step(machine, 1),
            ["print", things @ ..] => print(machine, things),
            ["exit"] => break,
            _ => println!(
                "There is no \"{}\" command. Try \"help\".",
                command.join(" ")
            ),
        }
    }
}

/// Print a register or a memory address
fn print(machine: &mut Machine, things: &[&str]) {
    for thing in things {
        // do they want pc to be printed?
        if *thing == "pc" {
            blue!("pc");
            println!(" = {:#x}", machine.read_pc());
            continue;
        }

        // did the user provide a register?
        if let Ok(register) = Register::try_from(*thing) {
            blue!("{}", register);
            println!(" = {:#x}", machine.read_register(register));
            continue;
        }

        // if not, check if they provided a number in decimal form
        if let Ok(addr) = thing.parse() {
            print_addr(machine, addr);
            continue;
        }

        // or in hex "0x123" format
        if let Ok(addr) = usize::from_str_radix(thing.trim_start_matches("0x"), 16) {
            print_addr(machine, addr);
            continue;
        }

        red_ln!("\"{}\" is not a register name nor a memory address.", thing);
    }
}

fn print_addr(machine: &mut Machine, addr: usize) {
    let word = machine.read_word(addr);

    blue!("{:#06x}", addr);
    print!(" = {:#010x}", word);

    if let Ok(instruction) = Instruction::try_from(word) {
        println!("   {}", instruction);
    } else {
        println!();
    }
}

/// Prompt the user for input and collect a list of commands/args
fn get_command(prompt: &str) -> Vec<String> {
    input::<String>()
        .msg(prompt)
        .get()
        .as_str()
        .trim()
        .split_whitespace()
        .map(|s| s.to_string())
        .collect()
}

/// Get a vec of bytes given a filename to read
fn parse_mem_file(content: &str) -> Option<Vec<u8>> {
    let mut memory = Vec::new();
    for line in content.lines() {
        // every line should contain a word written in hex format
        let word = u32::from_str_radix(line.trim(), 16).ok()?;
        memory.push((word >> 24) as u8);
        memory.push(((word >> 16) & 0xFF) as u8);
        memory.push(((word >> 8) & 0xFF) as u8);
        memory.push((word & 0xFF) as u8);
    }
    Some(memory)
}

fn step(machine: &mut Machine, n: u32) {
    for _ in 0..n {
        match machine.step() {
            Err(error) => {
                let msg = explain_error(error);
                red_ln!("Error: {}.", msg);
            }
            Ok(instruction) => {
                green_ln!("Executed {}", instruction);
            }
        }
    }
}

fn help() {
    println!(include_str!("help_text.txt"));
}

fn explain_error(error: Error) -> String {
    match error {
        Error::Syntax(code) => format!("Error (syntax): {code}."),
        Error::InvalidRegisterNumber(n) => format!("Invalid register number {n}"),
        Error::InvalidInstruction(instr) => format!("Invalid instruction \"{instr:x}\""),
        Error::InvalidRegisterName(name) => format!("Invalid register name \"{name}\"."),
        Error::InvalidDirective(directive) => format!("No directive \"{directive}\" supported"),
        Error::AddressOutOfRange(n) => format!("Address {n} is out of range"),
        Error::BadOperand(s) => s,
        Error::UndefinedLabel(label) => format!("A undefined label \"{label}\" was used"),
        Error::InstructionNotImplemented(instr) => {
            format!("Instruction \"{instr}\" not implemented yet")
        }
    }
}
