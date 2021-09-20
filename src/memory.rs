use std::cell::RefCell;
use std::fs;
use std::path::Path;
use std::rc::Rc;
use crate::utils;

/**
 * The Memory Bank Controller (MBC) type used in the cartridge
 */
enum MBCType {
    None,
    MBC1,
}

/*
    The memory of the gameboy
    0x0000 - 0x00FF: bootstrap, turns into interrupt table after boot
    0x0100 - 0x3FFF: cartridge

    0x8000 - 0x97FF: tile data
        0x8000 - 0x8FFF tile set 1
        0x8800 - 0x8FFF shared tile set
        0x8800 - 0x97FF tile set 2
*/
pub struct MemoryBus {
    pub base: Rc<RefCell<[u8; 0x10000]>>, // TODO make field private
    mbc_type: MBCType,
    banks: Vec<[u8; 0x4000]>,
}

impl MemoryBus {
    pub fn new(rom_path: Option<&Path>) -> Self {
        let rom = match rom_path {
            Some(p) => fs::read(p).unwrap(),
            None => vec![0; 32768],
        };
        let rom_slice = rom.as_slice();
        let rom_header = &rom[..0x014F]; // Header info starts at 0x0100, but easier this way

        // Load data into memory depending on cartridge configuration
        let mut memory_base = [0; 0x10000];
        let mut memory_banks = Vec::new();
        let mbc_type = MemoryBus::get_cartridge_type(rom_header);
        match mbc_type {
            // No MBC, loaded directly into memory
            MBCType::None => {
                for (i, b) in rom_slice.iter().enumerate() {
                    memory_base[i] = *b;
                }
            },
            // MBC1 uses 125 banks
            MBCType::MBC1 => {
                let rom_iter = rom_slice.chunks_exact(0x4000);
                for slice in rom_iter {
                    let mut bank = [0u8; 0x4000];
                    bank.copy_from_slice(slice);
                    memory_banks.push(bank);
                }
                // TODO check if rom_iter.remainder() contains anything
                while memory_banks.len() < 125 {
                    memory_banks.push([0; 0x4000]);
                }

                // Put first two banks directly in memory
                for (i, b) in memory_banks[0].iter().chain(memory_banks[1].iter()).enumerate() {
                    memory_base[i] = *b;
                }
            },
        }

        let mut mem = MemoryBus {
            base: Rc::new(RefCell::new(memory_base)),
            mbc_type: mbc_type,
            banks: memory_banks,
        };

        // Initial memory values. See https://gbdev.io/pandocs/#power-up-sequence
        // Memory defaults to 0 so some entries are removed
        // TODO read these from file along with the bootrom
        mem.write_byte(0xFF10, 0x80); // NR10
        mem.write_byte(0xFF11, 0xBF); // NR11
        mem.write_byte(0xFF12, 0xF3); // NR12
        mem.write_byte(0xFF14, 0xBF); // NR14
        mem.write_byte(0xFF16, 0x3F); // NR21
        mem.write_byte(0xFF19, 0xBF); // NR24
        mem.write_byte(0xFF1A, 0x7F); // NR30
        mem.write_byte(0xFF1B, 0xFF); // NR31
        mem.write_byte(0xFF1C, 0x9F); // NR32
        mem.write_byte(0xFF1E, 0xBF); // NR34
        mem.write_byte(0xFF20, 0xFF); // NR41
        mem.write_byte(0xFF23, 0xBF); // NR44
        mem.write_byte(0xFF24, 0x77); // NR50
        mem.write_byte(0xFF25, 0xF3); // NR51
        mem.write_byte(0xFF26, 0xF1); // NR52   ($F1 on GB, $F0 on SGB)
        mem.write_byte(0xFF40, 0x91); // LCDC
        mem.write_byte(0xFF47, 0xFC); // BGP
        mem.write_byte(0xFF48, 0xFF); // OBP0
        mem.write_byte(0xFF49, 0xFF); // OBP1

        mem
    }

    // Returns a Rc with the base memory of the gameboy. Used by PPU.
    pub fn get_mem_ref(&self) -> Rc<RefCell<[u8; 0x10000]>> {
        self.base.clone()
    }

    pub fn read_byte(&self, address: usize) -> u8 {
        self.base.borrow()[address]
    }

    pub fn write_byte(&mut self, address: usize, value: u8) {
        match address {
            0x0000..=0x1FFF => {},
            0x2000..=0x3FFF => {
                match self.mbc_type {
                    MBCType::MBC1 => {
                        let mut bank_number = value & 0x1F;
                        if vec!(0x00, 0x20, 0x40, 0x60).contains(&bank_number) {
                            bank_number += 1;
                        }

                        // Replace memory with bank data
                        let mut mem_ref = self.base.borrow_mut();
                        for (i, b) in self.banks[bank_number as usize].iter().enumerate() {
                            mem_ref[i] = *b;
                        }
                    },
                    _ => {},
                }
            },
            0x4000..=0x5FFF => {},
            0x6000..=0x7FFF => {},
            _ => {
                let mut mem_ref = self.base.borrow_mut();
                mem_ref[address] = value;
            },
        }
    }

    // Returns the type of the cartridge. What type of MBC it uses.
    fn get_cartridge_type(header: &[u8]) -> MBCType {
        let cartridge_type = &header[0x147];

        // Extract MBC type
        match cartridge_type {
            0x00 | 0x08..=0x09  => MBCType::None,
            0x01..=0x03         => MBCType::MBC1,
            _ => panic!("Cartridge type unsupported"),
        }
    }

    // Reads cartridge header data and prints it
    pub fn read_cartridge_header(&self) {
        let memory = self.base.borrow();

        // Titles
        let title_slice = &memory[0x134..=0x143];
        print!("Title: ");
        for c in title_slice {
            print!("{}", *c as char);
        }
        println!();

        // Licensee code
        let licensee_code = &memory[0x144..=0x145];
        let code = (licensee_code[0] << 1).wrapping_add(licensee_code[1]);
        println!("Publisher: {}", utils::get_licensee_name(code));

        // Cartridge type
        let cartridge_type = &memory[0x147];
        println!("Cartridge type: {}", utils::get_cartridge_type(*cartridge_type));

        // ROM size
        let rom_size = memory[0x148];
        let postfix = if rom_size < 0x5 {"KiB"} else {"MiB"};
        println!("ROM size: {} {}", 32 << rom_size, postfix);
    }
}
