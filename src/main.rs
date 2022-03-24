use clap::Parser;
use colour::red_ln;
use mips_machine::{Error, Machine};
use read_input::prelude::*;
use std::fs;

/// A simple MIPS virtual machine
#[derive(Parser, Debug)]
#[clap(version, long_about = None)]
struct Args {
    /// A file path to a text file containg memory data
    #[clap(short, long, value_name = "FILE")]
    memory_file: Option<String>,

    /// The address from which the given memory should be placed
    #[clap(short = 'o', long, default_value_t = 0, value_name = "ADDRESS")]
    memory_origin: usize,

    /// Used to specify the value of the pc register
    #[clap(long, default_value_t = 0, value_name = "ADDRESS")]
    pc: usize,
}

/// Get a vec of bytes given a filename to read
fn get_memory(file: String) -> Option<Vec<u8>> {
    let mut memory = Vec::new();
    for line in fs::read_to_string(&file).ok()?.lines() {
        // every line should contain a byte in binary form
        let byte = u8::from_str_radix(line, 2).ok()?;
        memory.push(byte);
    }
    Some(memory)
}

fn main() {
    let args = Args::parse();
    let mut machine = Machine::new();

    // load the memory of the machine
    let memory = args.memory_file.clone().and_then(get_memory);
    match memory {
        Some(m) => {
            machine.set_memory_from(&m, args.memory_origin);
            println!("The memory file was loaded into the machine.");
        }
        None => {
            if args.memory_file.is_some() {
                red_ln!("Error: Failed to parse memory file");
            }
            println!("Note: Memory and registers are all set to 0.")
        }
    }

    // set the pc register
    machine.set_pc(args.pc as u32);

    loop {
        match input::<String>().msg("Command: ").get().as_str().trim() {
            "help" => help(),
            "exit" => break,
            "step" => step(&mut machine),
            other => println!("There is no \"{other}\" command. Try \"help\"."),
        }
    }
}

fn step(machine: &mut Machine) {
    match machine.step() {
        Err(error) => {
            let msg = report_error(error);
            red_ln!("Error: {}", msg);
        }
        Ok(instruction) => println!("Executed {:?}", instruction),
    }
}

fn help() {
    println!(
        "Welcome to this simple demo CLI. Available commands:\n\
    help, step, exit"
    );
}

fn report_error(error: Error) -> String {
    match error {
        Error::InvalidRegisterNumber(n) => format!("Invalid register number {n}"),
        Error::InvalidInstruction(instr) => format!("Invalid instruction \"{instr:x}\""),
        Error::InstructionNotImplemented(instr) => {
            format!("Instruction \"{instr}\" not implemented yet")
        }
    }
}
