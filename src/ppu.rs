pub struct Ppu {
    mem: &[u8; 0x10000],
}

impl Ppu {
    pub fn new(&mut mem_ref: [u8; 0x10000]) -> Self {
        Ppu {
            mem: mem_ref,
        }
    }
}