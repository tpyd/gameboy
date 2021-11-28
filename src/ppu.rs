use std::{cell::RefCell, rc::Rc};
use crate::utils;
use crate::constants::*;

// The four different color values of a pixel
// 1 1 => White
// 0 1 => Light gray
// 1 0 => Dark gray
// 0 0 => Black
#[derive(Clone, Copy, Debug)]
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

type TilesetBuffer = [[TilePixelValue; TILESET_WIDTH]; TILESET_HEIGHT];
fn empty_tileset_buffer() -> TilesetBuffer {
    [[TilePixelValue::Zero; TILESET_WIDTH]; TILESET_HEIGHT]
}

pub struct Ppu {
    pub mem: Rc<RefCell<[u8; 0x10000]>>,

    // Internal buffers for rendering the tilemap and the background
    screen_buffer: Box<[TilePixelValue; SCREEN_SIZE]>,
    background_buffer: Box<[TilePixelValue; BG_SIZE]>,
    tileset_buffer: TilesetBuffer,

    // Current scanline from oam search
    current_row_tiles: [TilePixelValue; WIDTH],
}

impl Ppu {
    pub fn new(mem_ref: Rc<RefCell<[u8; 0x10000]>>) -> Self {
        Ppu {
            mem: mem_ref,
            screen_buffer: utils::alloc_boxed_array(),
            background_buffer: utils::alloc_boxed_array(),
            tileset_buffer: empty_tileset_buffer(),
            current_row_tiles: [TilePixelValue::Zero; WIDTH],
        }
    }

    // Updates the PPUs internal data like the tilemap. The address and value is required to
    // only make the PPU update the necessary data instead of updating everything.
    pub fn update(&mut self, address: usize) {
        match address {
            0x8000..=0x97FF => {
                self.update_tiledata(address);
                //self.update_background(address);
            },
            0x9800..=0x9FFF => self.update_background(address),
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
        let tile_index = (address - 0x8000) / 16;

        let tile_row = (address % 16) / 2; // Row of the tile (0-7)
        let tileset_row = (tile_index / 24) * 8 + tile_row; // Row of the whole tilemap (0-16*8)

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

            let tileset_col = (tile_index % 24) * 8 + pixel_address;
            self.tileset_buffer[tileset_row][tileset_col] = value;
        }
    }

    // Updates the internal background buffer.
    fn update_background(&mut self, address: usize) {
        let mem_ref = self.mem.borrow();

        let background_index = address - 0x9800;

        let tile_index = mem_ref[address] as u16;
        let tile_address = (0x8000 + tile_index * 16) as usize; // A tile is 16 bytes

        // Get the whole tile and render it to the background buffer
        let tile_data = &mem_ref[tile_address..tile_address + 16];
        for (row, tile_row) in tile_data.chunks_exact(2).enumerate() {
            let byte1 = tile_row[0];
            let byte2 = tile_row[1];

            for pixel_address in 0..8 {
                let mask = 1 << (7 - pixel_address);
                let lsb = byte1 & mask;
                let msb = byte2 & mask;

                let value = match (lsb != 0, msb != 0) {
                    (true, true) => TilePixelValue::Three,
                    (false, true) => TilePixelValue::Two,
                    (true, false) => TilePixelValue::One,
                    (false, false) => TilePixelValue::Zero,
                };

                let bg_y = (background_index / 32) * 8 + row;
                let bg_x = (background_index % 32) * 8 + pixel_address;
                self.background_buffer[bg_y * 256 + bg_x] = value;
            }
        }
        
    }

    // Gets the tiles on the current scanline and stores them for the pixel transfer
    pub fn oam_search(&mut self, ly: u8) {
        let mem_ref = self.mem.borrow();

        let ldlc = mem_ref[0xFF40];
        let address_mode = if ldlc & 0x04 == 0 { 0x8000 } else { 0x8800 };

        // Get the scroll of the window
        let scy = mem_ref[0xFF42];
        let scx = mem_ref[0xFF43];

        // Add the current scanline to find out which background line to render
        let y = scy.wrapping_add(ly);
        let x = scx;

        // This function only processes one scanline so only a subset is needed.
        // First we have to find out which tiles to get. These two lines will
        // figure out exactly which row we are on in the background.
        let tilemap_row = (y / 8) as usize;
        let tile_row = (y % 8) as usize;

        // Grab the tile indicies for the current row
        let tile_index_start = 0x9800 + 32 * tilemap_row;
        let tile_index_end = 0x9800 + 32 * tilemap_row + 32;
        let tile_indicies = &mem_ref[tile_index_start..tile_index_end];

        // Get the memory locations for the actual tile data
        let mut tile_data = Vec::new();
        for tile_index in tile_indicies {
            let tile_data_location = (*tile_index as u16 * 16 + 0x8000) as usize;
            let row_offset = tile_row * 2; // 2 bytes per row
            tile_data.push(mem_ref[tile_data_location + row_offset]);
        }

        // Only need to process the relevant tile data
        let iter = tile_data.as_slice().chunks_exact(2).enumerate();
        for (i, tile_bytes) in iter {
            let byte1 = tile_bytes[0];
            let byte2 = tile_bytes[1];

            for pixel_address in 0..8 {
                let mask = 1 << (7 - pixel_address);
                let lsb = byte1 & mask;
                let msb = byte2 & mask;

                let value = match (lsb != 0, msb != 0) {
                    (true, true) => TilePixelValue::Three,
                    (false, true) => TilePixelValue::Two,
                    (true, false) => TilePixelValue::One,
                    (false, false) => TilePixelValue::Zero,
                };

                self.current_row_tiles[i*8 + pixel_address] = value;
            }
        }
    }

    // Uses values from OAM search and puts them onto the screen
    pub fn pixel_transfer(&mut self, ly: u8) {
        for (x, v) in self.current_row_tiles.iter().enumerate() {
            self.screen_buffer[WIDTH * ly as usize + x] = *v;
        }
    }

    pub fn get_screen_buffer(&self) -> &[TilePixelValue] {
        &self.screen_buffer[0..SCREEN_SIZE]
    }

    pub fn get_tileset_buffer(&self) -> &TilesetBuffer {
        &self.tileset_buffer
    }

    pub fn get_background_buffer(&self) -> &[TilePixelValue] {
        &self.background_buffer[0..BG_SIZE]
    }
}