use std::rc::Rc;

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

pub struct Ppu {
    mem: Rc<[u8; 0x10000]>,
}

impl Ppu {
    pub fn new(mem_ref: Rc<[u8; 0x10000]>) -> Self {
        Ppu {
            mem: mem_ref,
        }
    }
    
    pub fn oam_search(&self) {

    }

    pub fn pixel_transfer(&self) {

    }
}