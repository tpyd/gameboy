use std::{fmt, fs};
use std::path::Path;

use crate::instruction::Instruction;

#[derive(Debug)]
struct TableRow {
    location: u16,
    raw: u8,
    instruction: Option<String>,
    comment: Option<String>,
}

impl TableRow {
    fn new(location: u16, raw: u8) -> Self {
        TableRow {
            location: location,
            raw: raw,
            instruction: None,
            comment: None,
        }
    }

    fn set_instruction(&mut self, instruction: String) {
        self.instruction = Some(instruction);
    }

    fn set_comment(&mut self, comment: String) {
        self.comment = Some(comment);
    }
}

pub struct Debugger {
    table: Vec<TableRow>,
}

impl Debugger {
    pub fn new(rom_path: &Path) -> Self {
        let mut table = Vec::new();
        let rom_data = fs::read(rom_path).unwrap();

        let mut prefixed = false;

        for (i, b) in rom_data.iter().enumerate() {
            let mut table_row = TableRow::new(i as u16, b.clone());

            // Handle prefixed instructions
            if b == &0xCB && !prefixed {
                prefixed = true;
                table_row.set_comment("prefix".into());
            } else {
                match Instruction::from_byte(*b, prefixed) {
                    Some(instruction) => table_row.set_instruction(instruction.to_string()),
                    None => {},
                };
            }

            table.push(table_row);
        }

        Debugger { 
            table: table
        }
    }
}