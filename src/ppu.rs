use std::rc::Rc;

pub struct Ppu {
    mem: Rc<[u8; 0x10000]>,
}

impl Ppu {
    pub fn new(mem_ref: Rc<[u8; 0x10000]>) -> Self {
        Ppu {
            mem: mem_ref,
        }
    }

    pub fn test(&self) {
        println!("Hello from PPU! Random value from memory: {}", self.mem[0xFF24]);
    }
}