use std::fs;
use instructions::Instruction;

#[derive(Debug)]
struct LineOfCode {
    address: u16,
    instruction_byte: u8,
    instruction_string: Option<String>,
    comment: Option<String>,
    disassembled: bool,
}

impl LineOfCode {
    fn new(address: u16, line: u8) -> Self {
        LineOfCode { 
            address: address,
            instruction_byte: line, 
            instruction_string: None,
            comment: None,
            disassembled: false
        }
    }
}

pub fn disassemble() {
    let filename = "../../test/blargg-test-roms/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb";

    let file_contents = fs::read(filename).unwrap();
    let mut lines_of_code: Vec<LineOfCode> = file_contents
        .iter()
        .enumerate()
        .map(|(n, x)| LineOfCode::new(n as u16, *x))
        .collect();

    // Start iterating over code
    let mut stack: Vec<u16> = Vec::new();
    stack.push(0x0100);

    while !stack.is_empty() {
        let mut pc = stack.pop().unwrap() as usize;
        let mut line = &mut lines_of_code[pc];

        // Stop the current iteration
        if line.disassembled {
            continue;
        }

        // Handle prefixed instructions
        let mut prefixed = false;
        if line == 0xCB {
            prefix = true;
            
        }

        let instr_byte = line.instruction_byte;
        let instr = instructions::Instruction::from_byte(instr_byte, prefixed).unwrap();

        // If we have a jump instruction we branch the iteration
        if let Instruction::JP(target) = instr {
            
        }

        
        line.instruction_string = Some(instruction_string);

        dbg!(&line);

        pc += 1;
    }
}

fn instruction_lookup(byte: &u8) -> Option<Instruction> {
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        disassemble();
    }
}
