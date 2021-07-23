const VRAM_BEGIN: usize = 0x8000;
const VRAM_END: usize = 0x97FF;
const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;

struct Ppu {
    VRAM: [u8; VRAM_SIZE],

}

impl Ppu {
    fn 
}