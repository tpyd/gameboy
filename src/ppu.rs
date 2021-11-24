use std::{cell::RefCell, rc::Rc};
use crate::utils;
use crate::constants::*;

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

impl Default for TilePixelValue {
    fn default() -> Self {
        TilePixelValue::Zero
    }
}

// A tile is 8x8 pixels.
// Since each pixel uses 2 bits, the size of one tile is 8*8*2/8 = 16 bytes
type Tile = [[TilePixelValue; 8]; 8];
fn empty_tile() -> Tile {
    [[TilePixelValue::Zero; 8]; 8]
}

pub struct Ppu {
    pub mem: Rc<RefCell<[u8; 0x10000]>>,
    screen_buffer: Box<[TilePixelValue; BG_SIZE]>,
    tile_set: Vec<Tile>, // 384 tiles in the tileset
    tile_set_buffer: Box<[TilePixelValue; TILESET_SIZE]>,
    current_row_tiles: Vec<Tile>,
}

impl Ppu {
    pub fn new(mem_ref: Rc<RefCell<[u8; 0x10000]>>) -> Self {
        let screen_buffer = utils::alloc_boxed_array();
        let tile_set_buffer = utils::alloc_boxed_array();

        Ppu {
            mem: mem_ref,
            screen_buffer: screen_buffer,
            tile_set: vec![empty_tile(); 384],
            tile_set_buffer: tile_set_buffer,
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

        self.update_tiledata_buffer();
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

    // Updates the internal tiledata rendering buffer
    fn update_tiledata_buffer(&mut self) {
        // Do one row at a time
        for y in 0..TILESET_HEIGHT {

            // Get all tiles belonging to the current row.
            // This is just grabbing 24 tiles at a time.
            let grid_row = y / 8;
            let tile_row = y % 8;
            let tiles = &self.tile_set[grid_row..grid_row + 8];
            let mut row_pixels = [TilePixelValue::Zero; TILESET_WIDTH];

            let mut x = 0;
            for tile in tiles {
                
            }
        }
    }

    // Gets the tiles on the current scanline and stores them for the pixel transfer
    pub fn oam_search(&mut self, ly: u8) {
        let mem_ref = self.mem.borrow();

        let ldlc = mem_ref[0xFF40];
        let address_mode = if ldlc & 0x04 == 0 { 0x8000 } else { 0x8800 };

        let scy = mem_ref[0xFF42];
        //let scx = mem_ref[0xFF43];

        let tile_y_start = (scy as u16 + ly as u16) % 256 / 16;
        //let tile_x_start = scx as u16 / 16;
        let tile_start = 0x9800 + tile_y_start * 32;// + tile_x_start;

        self.current_row_tiles.clear();
        for i in tile_start..tile_start + 32 { // Does not take scx into account atm
            self.current_row_tiles.push(self.get_tile(i, address_mode));
        }
    }

    // Uses values from OAM search and puts them onto the screen
    pub fn pixel_transfer(&mut self, ly: u8) {
        for x in 0..32*8 {
            let current_tile = self.current_row_tiles[x / 8];
            self.screen_buffer[32 * 8 * ly as usize + x] = current_tile[x % 8][ly as usize % 8];
        }
    }

    pub fn get_screen_buffer(&self) -> &[TilePixelValue] {
        &self.screen_buffer[0..SCREEN_SIZE]
    }

    pub fn get_tileset_buffer(&self) -> &[TilePixelValue] {
        &self.tile_set_buffer[0..TILESET_SIZE]
    }

    // Returns the tile at the given index depending on whether the address mode is $8000 or not (set by LDLC bit 4)
    pub fn get_tile(&self, index: u16, address_mode: u16) -> Tile {
        let mem_ref = self.mem.borrow();
        let tile_index = mem_ref[index as usize] as usize;

        if address_mode == 0x8000 {
            // $8000 mode uses 0x8000 as base pointer and unsigned adressing
            self.tile_set[tile_index]
        } else {
            // $8800 mode uses 0x9000 as base address and signed indexing
            self.tile_set[255u16.wrapping_add((tile_index as i8 + 1) as u16) as usize]
        }
    }
}