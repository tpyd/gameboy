use std::fs;

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
    let mut pc = 0x0100;
    let mut stack: Vec<u16> = Vec::new();

    for _ in 0..10 {
        let line = &mut lines_of_code[pc];
        let instruction_string = instruction_lookup(&line.instruction_byte);
        line.instruction_string = instruction_string;

        dbg!(&line);

        pc += 1;
    }
}

fn instruction_lookup(byte: &u8) -> Option<String> {
    match byte {
        0xc2 | 0xd2 | 0xc3 | 0xca | 0xda => Some(String::from("JP")),
        _ => None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        disassemble();
    }
}
