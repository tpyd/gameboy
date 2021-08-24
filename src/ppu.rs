use std::{cell::RefCell, rc::Rc};

use crate::{HEIGHT, WIDTH};
const FULL_WIDTH: usize = 256;
const FULL_HEIGHT: usize = 256;

// The four different color values of a pixel
// 1 1 => White
// 0 1 => Light gray
// 1 0 => Dark gray
// 0 0 => Black
#[derive(Clone, Copy)]
#[repr(u32)]
pub enum TilePixelValue {
    Zero = 0x00FFFFFF,
    One = 0x00AAAAAA,
    Two = 0x00555555,
    Three = 0x00000000,
}

// A tile is 8x8 pixels.
// Since each pixel uses 2 bits, the size of one tile is 8*8*2/8 = 16 bytes
type Tile = [[TilePixelValue; 8]; 8];
fn empty_tile() -> Tile {
    [[TilePixelValue::Zero; 8]; 8]
}

pub struct Ppu {
    pub mem: Rc<RefCell<Box<[u8; 0x10000]>>>,
    screen_buffer: [TilePixelValue; FULL_HEIGHT * FULL_WIDTH],
    tile_set: [Tile; 384],
    current_row_tiles: Vec<Tile>,
}

impl Ppu {
    pub fn new(mem_ref: Rc<RefCell<Box<[u8; 0x10000]>>>) -> Self {
        Ppu {
            mem: mem_ref,
            screen_buffer: [TilePixelValue::Zero; FULL_HEIGHT * FULL_WIDTH],
            tile_set: [empty_tile(); 384],
            current_row_tiles: Vec::new(),
        }
    }

    // Updates the PPUs internal data like the tilemap. The address and value is required to
    // only make the PPU update the necessary data instead of updating everything.
    pub fn update(&mut self, address: usize) {
        match address {
            0x8000..=0x97FF => self.update_tiledata(address),
            _ => {},
        }
    }

    // Updates the internal tiledata
    fn update_tiledata(&mut self, address: usize) {
        // Tile rows are encoded such that the first byte is on the even address
        // ANDing with 0xFFFE effectively rounds down to an even address
        let normalized_address = address & 0xFFFE;

        // Get the two bytes containing the tile row
        let mem = self.mem.borrow();
        let byte1 = mem[normalized_address];
        let byte2 = mem[normalized_address + 1];

        // One tile is 8x8 pixels. Every pixel is represented by 2 bits (4 colors).
        // 8*2 = 16bits per row, which is 2 bytes. One tile is 8*2 = 16bytes in size
        let tile_address = (address - 0x8000) / 16;
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

    pub fn oam_search(&mut self, ly: u8) {
        let ldlc = self.mem.borrow()[0xFF40];
        let address_mode = if ldlc & 0x04 == 0 { 0x8000 } else { 0x8800 };

        let tile_index_start = (ly / 8) * 32; // 32 is row offset
        self.current_row_tiles.clear();
        for i in tile_index_start..tile_index_start + 32 {
            self.current_row_tiles.push(self.get_tile(i, address_mode));
        }
    }

    pub fn pixel_transfer(&mut self, ly: u8) {
        for x in 0..32*8 {
            let current_tile = self.current_row_tiles[x / 8];
            self.screen_buffer[32 * 8 * ly as usize + x] = current_tile[x % 8][ly as usize % 8];
        }
    }

    pub fn get_screen_buffer(&self) -> &[TilePixelValue] {
        &self.screen_buffer
    }

    // TODO remove this
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