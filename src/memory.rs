use std::fs;
use crate::utils;

/**
 * The Memory Bank Controller (MBC) type used in the cartridge
 */
enum MBCType {
    None,
    MBC1,
}

/*
    Graphic part of the gameboy.
    The gameboy doesn't actually have a GPU
*/
const VRAM_BEGIN: usize = 0x8000;
const VRAM_END: usize = 0x97FF;
const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;

/*
    The color of a pixel. Gameboy has four possible colors for each pixel (2 bits per pixel).
    1 1 => White
    0 1 => Light gray
    1 0 => Dark gray
    0 0 => Black
*/
#[derive(Clone, Copy)]
#[repr(u32)]
pub enum TilePixelValue {
    Zero = 0x00FFFFFF,
    One = 0x00AAAAAA,
    Two = 0x00555555,
    Three = 0x00000000,
}

/*
    A tile is 8x8 pixels.
    Since each pixel uses 2 bits, the size of one tile is 8*8*2/8 = 16 bytes
*/
type Tile = [[TilePixelValue; 8]; 8];
fn empty_tile() -> Tile {
    [[TilePixelValue::Zero; 8]; 8]
}

struct GPU {
    vram: [u8; VRAM_SIZE],
    pub tile_set: [Tile; 384],
}

impl GPU {
    fn new() -> Self {
        GPU {
            vram: [0; VRAM_SIZE],
            tile_set: [empty_tile(); 384],
        }
    }

    fn read_vram(&self, address: usize) -> u8 {
        self.vram[address]
    }

    // Writing to vram also manages tile sets
    fn write_vram(&mut self, address: usize, value: u8) {
        self.vram[address] = value;

        // Return if the address is outside vram, don't need to update tiles
        if address >= 0x1800 { // TODO change to VRAM_SIZE?
            return
        }

        // Tile rows are encoded such that the first byte is on the even address
        // ANDing with 0xFFFE effectively rounds down to an even address
        let normalized_address = address & 0xFFFE;

        // Get the two bytes containing the tile row
        let byte1 = self.vram[normalized_address];
        let byte2 = self.vram[normalized_address + 1]; // TODO check if wrapping_add() is better

        // One tile is 8x8 pixels. Every pixel is represented by 2 bits (4 colors).
        // 8*2 = 16bits per row, which is 2 bytes. One tile is 8*2 = 16bytes in size
        let tile_address = address / 16;
        let row_address = (address % 16) / 2;

        // Loop each row
        for pixel_address in 0..8 {
            // Create a mask to extract a single bit from each of the bytes.
            // This is done since pixels in one tile row is encoded to take one bit from each byte
            // Example:
            // 1 1 0 1 0 0 1 1 byte1
            // 1 0 0 1 1 1 0 1 byte2
            // 3 1 0 3 2 2 1 3 Result colors for the row
            let mask = 1 << (7 - pixel_address);
            let lsb = byte1 & mask;
            let msb = byte2 & mask;

            // Check the two bits of both bytes to determine the color of the pixel
            let value = match (lsb != 0, msb != 0) {
                (true, true) => TilePixelValue::Three,
                (false, true) => TilePixelValue::Two,
                (true, false) => TilePixelValue::One,
                (false, false) => TilePixelValue::Zero,
            };

            self.tile_set[tile_address][row_address][pixel_address] = value;
        }
    }

    // Returns the tile at the given index depending on whether the address mode is $8000 or not (set by LDLC bit 4)
    pub fn get_tile(&self, index: u8, address_mode: u16) -> Tile {
        if address_mode == 0x8000 {
            self.tile_set[index as usize]
        } else {
            // $8800 mode uses 0x9000 as base address and signed indexing
            self.tile_set[255u16.wrapping_add((index as i8 + 1) as u16) as usize]
        }
    }
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
    base: [u8; 0x10000],
    mbc_type: MBCType,
    banks: Vec<[u8; 0x4000]>,
    gpu: GPU,
}

impl MemoryBus {
    pub fn new() -> Self {
        let rom = fs::read("test/cpu_instrs/individual/06-ld r,r.gb").unwrap();
        let rom_slice = rom.as_slice();
        let rom_header = &rom[..0x014F]; // Header info starts at 0x0100, but easier this way

        // Load data into memory depending on cartridge configuration
        let mut memory_base = [0u8; 0x10000];
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
            base: memory_base,
            mbc_type: mbc_type,
            banks: memory_banks,
            gpu: GPU::new(),
        };

        read_cartridge_header(&rom_header);

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
        //[$FF26] = $F1-GB, $F0-SGB ; NR52
        mem.write_byte(0xFF40, 0x91); // LCDC
        mem.write_byte(0xFF47, 0xFC); // BGP
        mem.write_byte(0xFF48, 0xFF); // OBP0
        mem.write_byte(0xFF49, 0xFF); // OBP1

        mem
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        let address = address as usize;

        // Redirect VRAM address to GPU struct
        if (VRAM_BEGIN..=VRAM_END).contains(&address) {
            return self.gpu.read_vram(address - VRAM_BEGIN)
        }

        self.base[address]
    }

    pub fn write_byte(&mut self, address: u16, value: u8) {
        let address = address as usize;

        // Redirect VRAM address to GPU struct
        if (VRAM_BEGIN..=VRAM_END).contains(&address) {
            return self.gpu.write_vram(address - VRAM_BEGIN, value)
        }

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
                        for (i, b) in self.banks[bank_number as usize].iter().enumerate() {
                            self.base[i] = *b;
                        }
                    },
                    _ => {},
                }
            },
            0x4000..=0x5FFF => {},
            0x6000..=0x7FFF => {},
            _ => self.base[address] = value,
        }
    }

    // Returns the type of the cartridge. What type of MBC it uses.
    fn get_cartridge_type(header: &[u8]) -> MBCType {
        let cartridge_type = &header[0x147];

        // Extract MBC type
        let mbc_type = match cartridge_type {
            0x00 | 0x08..=0x09  => MBCType::None,
            0x01..=0x03         => MBCType::MBC1,
            _ => panic!("Cartridge uses unsupported MBC type"),
        };

        mbc_type
    }

    // Should be refactored in the future
    pub fn get_gpu_tile(&self, index: u8, address_mode: u16) -> Tile {
        self.gpu.get_tile(index, address_mode)
    }

}

/**
 * Reads cartridge header data and prints it
 */
fn read_cartridge_header(memory: &[u8]) {
    // Title
    let title_slice = &memory[0x134..=0x143];
    print!("Title: ");
    for c in title_slice {
        print!("{}", *c as char);
    }
    println!();

    // Licensee code
    let licensee_code = &memory[0x144..=0x145];
    let code = licensee_code[0] << 1 + licensee_code[1];
    println!("Publisher: {}", utils::get_licensee_name(code));

    // Cartridge type
    let cartridge_type = &memory[0x147];
    println!("Cartridge type: {}", utils::get_cartridge_type(*cartridge_type));

    // ROM size
    let rom_size = *&memory[0x148];
    let postfix = if rom_size < 0x5 {"KiB"} else {"MiB"};
    println!("ROM size: {} {}", 32 << rom_size, postfix);
}