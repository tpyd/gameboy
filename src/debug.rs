struct TableRow {
    location: u8,
    raw: u8,
    instruction: Option(str),
    comment: Option(str),
}

impl TableRow {
    fn new(location: u8, raw: u8) -> Self {
        TableRow {
            location: location,
            raw: raw,
            instruction: None,
            comment: None,
        }
    }
}

struct Debugger {
    table: Vec<TableRow>,
}

impl Debugger {
    fn new(rom_path: Into<Path>) -> Self {
        let rom_data = fs::read(rom_path).unwrap();

        for b in rom_data.iter() {
            let table_row = TableRow::new(b, b);
            table.push(table_row);
        }
    }
}