use std::fmt::Display;
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

impl Display for TableRow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut display = format!("{:#06X}    {:#04X}", self.location, self.raw);

        if let Some(instruction) = &self.instruction {
            display = format!("{}    {}", display, instruction);
        }

        if let Some(comment) = &self.comment {
            display = format!("{}    {}", display, comment);
        }

        write!(f, "{}", display)
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
                prefixed = false;
                match Instruction::from_byte(*b, prefixed) {
                    Some(instruction) => table_row.set_instruction(instruction.to_string()),
                    None => {},
                };
            }

            table.push(table_row);
        }

        for i in 0..100 {
            println!("{}", table[i]);
        }

        Debugger { 
            table: table
        }
    }
}
