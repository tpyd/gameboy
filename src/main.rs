use minifb::{Key, Window, WindowOptions};
use std::time::{Duration, Instant};
use std::thread::sleep;
use std::fs;

mod instruction;
use instruction::*;

const WIDTH: usize = 160;
const HEIGHT: usize = 144;
const TILESET_WIDTH: usize = 24*8; // 8x8 pixels
const TILESET_HEIGHT: usize = 16*8;

const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBTRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

/*
    FlagsRegister is an abstraction of the f register on the CPU.
    bits 0-3 are unused and bits 4-7 are set following these rules:

    z, set if an operation is zero
    n, if the previous instruction was a subtraction
    h, if the lower 4 bits resulted in an overflow. (0xF)
    c, set if   8-bit addition is larger than 0xFF (8-bit overflow)
                16-bit larger than 0xFFFF (16-bit overflow)
                Subtraction or comparison lower than zero
                When rotate/shift operation shifts out a "1" bit
*/
#[derive(Clone, Copy)]
struct FlagsRegister {
    zero: bool,         // z
    subtract: bool,     // n
    half_carry: bool,   // h
    carry: bool,        // c
}

// Implement methods for easy conversion between u8 and FlagsRegister
impl std::convert::From<FlagsRegister> for u8 {
    fn from(flag: FlagsRegister) -> u8 {
        (if flag.zero       { 1 } else { 0 }) << ZERO_FLAG_BYTE_POSITION |
        (if flag.subtract   { 1 } else { 0 }) << SUBTRACT_FLAG_BYTE_POSITION |
        (if flag.half_carry { 1 } else { 0 }) << HALF_CARRY_FLAG_BYTE_POSITION |
        (if flag.carry      { 1 } else { 0 }) << CARRY_FLAG_BYTE_POSITION
    }
}

impl std::convert::From<u8> for FlagsRegister {
    fn from(byte: u8) -> Self {
        let zero = ((byte >> ZERO_FLAG_BYTE_POSITION) & 0b1) != 0;
        let subtract = ((byte >> SUBTRACT_FLAG_BYTE_POSITION) & 0b1) != 0;
        let half_carry = ((byte >> HALF_CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;
        let carry = ((byte >> CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;

        FlagsRegister {
            zero,
            subtract,
            half_carry,
            carry,
        }
    }
}

/*
    CPU Registers a,b,c,d,e,f,h,l
    f register is flags represented by FlagsRegister struct

    All registers are set using 16-bit values in combinations 'af', 'bc', 'de' and 'hl'
*/
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: FlagsRegister,
    h: u8,
    l: u8,
}

impl Registers {
    fn new() -> Self {
        let mut reg = Registers {
            a: 0x00,
            b: 0x00,
            c: 0x00,
            d: 0x00,
            e: 0x00,
            f: FlagsRegister::from(0x00),
            h: 0x00,
            l: 0x00,
        };

        // See https://gbdev.io/pandocs/#power-up-sequence
        reg.set_af(0x01b0);
        reg.set_bc(0x0013);
        reg.set_de(0x00d8);
        reg.set_hl(0x014d);
        reg
    }

    fn get_af(&self) -> u16 {
        (self.a as u16) << 8 | u8::from(self.f) as u16
    }

    fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.f = FlagsRegister::from((value & 0x00FF) as u8);
    }

    fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }

    fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0x00FF) as u8;
    }

    fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | self.e as u16
    }

    fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0x00FF) as u8;
    }

    fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }

    fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0x00FF) as u8;
    }
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
enum TilePixelValue {
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
    tile_set: [Tile; 384],
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

    fn get_tile(&self, index: u8) -> Tile {
        self.tile_set[index as usize]
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
struct MemoryBus {
    memory: [u8; 0xFFFF],
    gpu: GPU,
}

impl MemoryBus {
    fn new() -> Self {
        let mut mem = MemoryBus {
            memory: [0; 0xFFFF],
            gpu: GPU::new(),
        };

        // Load in Bootstrap into memory
        let bootstrap = fs::read("resources/DMG_ROM.bin").unwrap();
        for (i, byte) in bootstrap.iter().enumerate() {
            mem.write_byte(i as u16, *byte);
        }

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

    fn read_byte(&self, address: u16) -> u8 {
        let address = address as usize;
        match address {
            // Redirect vram address to GPU
            VRAM_BEGIN ..= VRAM_END => {
                self.gpu.read_vram(address - VRAM_BEGIN)
            },
            _ => self.memory[address]
        }
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        let address = address as usize;
        match address {
            VRAM_BEGIN ..= VRAM_END => {
                self.gpu.write_vram(address - VRAM_BEGIN, value)
            },
            _ => self.memory[address as usize] = value,
        };
    }
}

/*
    Implementation of the CPU LR35902
*/
struct CPU {
    registers: Registers,
    pc: u16, // Program counter, tells which instruction its currently running
    sp: u16, // Stack pointer, points to the top of the stack
    bus: MemoryBus,
    ei_called: bool, // Whether to enable interrupt after instruction
    is_halted: bool,
    ime: bool,
    last_instruction: Instruction,
}

impl CPU {
    fn new() -> Self {
        CPU {
            registers: Registers::new(),
            pc: 0x0000,
            sp: 0xFFFE, // See https://gbdev.io/pandocs/#power-up-sequence
            bus: MemoryBus::new(),
            ei_called: false,
            is_halted: false,
            ime: true,
            last_instruction: Instruction::STOP,
        }
    }

    // Runs a specified amount of cycles depending on the time that passed since last frame.
    // Returns the number of cycles run
    fn run(&mut self, time_delta: u32) -> usize {
        // Calculate how many times to run step depending on time_delta
        let cycles_per_nano = CLOCK as f32 / 1e9;
        let cycles_to_run = (cycles_per_nano * time_delta as f32) as usize;

        let mut cycles = 0;
        while cycles < cycles_to_run {
            cycles += self.step()
        }

        // Return how many cycles have passed in those steps
        cycles
    }

    // Executes one CPU cycle. Reads and executes an instruction from the position of the pc and updates the pc
    // Returns the number of cycles performed
    fn step(&mut self) -> usize {
        let mut instruction_byte = self.read_next_byte();
        let prefixed = instruction_byte == 0xCB;
        if prefixed {
            instruction_byte = self.read_next_byte();
        }

        // Lookup and execute next instruction and return size of instruction in bytes and how many cycles it took to execute
        let cycles = if let Some(instruction) = Instruction::from_byte(instruction_byte, prefixed) {
            let c = self.execute(instruction);
            self.last_instruction = instruction;

            // Enable interrupts if previous instruction call was EI
            if self.ei_called {
                match Instruction::from_byte(instruction_byte, prefixed).unwrap() {
                    Instruction::EI => {}, // Makes repeated calls to EI work as one call
                    _ => { self.ime = true; self.ei_called = false; },
                }
            }

            c
        } else {
            let description = format!("0x{}{:x}", if prefixed { "cb" } else { "" }, instruction_byte);
            panic!("Unknown instruction found for: {}\nLast instruction: {:?}",
                description, self.last_instruction);
        };

        // Read serial port, used to debug with Blargg's test rom
        //print!("{}", self.bus.read_byte(0xFF01).to_ascii_lowercase()); // Maybe change to little endianess

        cycles
    }

    // Creates and returns a pixel buffer containing what should be drawn on screen
    fn pixel_buffer(&mut self) -> Vec<u32> {
        let ldlc = self.bus.read_byte(0xFF40);
        let mut tile_map: Vec<u32> = Vec::new();
        tile_map.resize(256*256, 0); // tilemap is 32x32 tiles, each 8x8 pixels

        // If Bit 7 is not set, LCD is disabled. Return all white
        if ldlc & 0x80 == 0 {
            tile_map.resize(256*256, 0x00FFFFFF);
            return tile_map
        }

        let tile_set_start = if ldlc & 0x04 == 0 { 0x9800 } else { 0x9C00 };

        // Loop through all tiles in BG
        for y in 0..32 {
            for x in 0..32 {
                // Get tile at this position
                let tile_index = self.bus.read_byte(tile_set_start + 32*y + x);
                let tile = self.bus.gpu.get_tile(tile_index);

                // Map tile into pixel buffer
                for tile_y in 0..8 {
                    for tile_x in 0..8 {
                        tile_map[(256*(y*8 + tile_y) + x*8 + tile_x) as usize] = tile[tile_x as usize][tile_y as usize] as u32;
                    }
                }
            }
        }

        // Find out where the screen is in the tilemap
        let scroll_y = self.bus.read_byte(0xFF42);
        let scroll_x = self.bus.read_byte(0xFF43);

        // Crop buffer to screen
        // TODO add so screen view wraps to other side
        let mut screen: Vec<u32> = Vec::new();
        screen.resize(WIDTH*HEIGHT, 0x00FFFFFF);
        for (y, y_offset) in (scroll_y..(scroll_y + HEIGHT as u8)).enumerate() {
            for (x, x_offset) in (scroll_x..(scroll_x + WIDTH as u8)).enumerate() {
                let p = tile_map[(y_offset as u32*256 + x_offset as u32) as usize];
                screen[(y*WIDTH + x) as usize] = p;
            }
        }

        screen
    }

    // Creates and returns a buffer with all tiles in the tileset in all three blocks
    fn tileset_buffer(&mut self) -> Vec<u32> {
        let mut tile_set: Vec<u32> = Vec::new();
        tile_set.resize(384*8*8, 0x00FFFFFF); // tileset is 384 tiles, each 8x8 pixels

        // Loop through tiles in tileset window
        for x in 0..24 {
            for y in 0..16 {
                // Get the tile at this position
                let tile = self.bus.gpu.tile_set[x*(y+1)];

                // Map tile to tile_set buffer
                for tile_y in 0..8 {
                    for tile_x in 0..8 {
                        tile_set[y*24*64 + tile_y*24*8 + x*8 + tile_x] = tile[tile_x][tile_y] as u32;
                    }
                }
            }
        }

        tile_set
    }

    //Executes a given instruction on the CPU. Every instruction execution controls pc movement by itself.
    fn execute(&mut self, instruction: Instruction) -> usize {
        if self.is_halted {
            return 4
        }

        match instruction {
            Instruction::ADD(target) => self.add(target, false),
            Instruction::ADC(target) => self.add(target, true),
            Instruction::SUB(target) => self.sub(target, false),
            Instruction::SBC(target) => self.sub(target, true),
            Instruction::INC(target) => self.inc(target),
            Instruction::DEC(target) => self.dec(target),

            Instruction::AND(target) => self.and(target),
            Instruction::XOR(target) => self.xor(target),
            Instruction::OR(target) => self.or(target),
            Instruction::CP(target) => self.cp(target),

            // These instructions are not prefixed and only takes 4 cycles, easier to return cycles here and send all to rotate method
            Instruction::RLA => { self.rotate(ByteTarget::A, true, true); 4 },
            Instruction::RLCA => { self.rotate(ByteTarget::A, false, true); 4 },
            Instruction::RRA => { self.rotate(ByteTarget::A, true, false); 4 },
            Instruction::RRCA => { self.rotate(ByteTarget::A, false, false); 4 },

            Instruction::RLC(target) => self.rotate(target, false, true),
            Instruction::RL(target) => self.rotate(target, true, true),
            Instruction::RRC(target) => self.rotate(target, false, false),
            Instruction::RR(target) => self.rotate(target, true, false),
            Instruction::SLA(target) => self.shift(target, true, false),
            Instruction::SRA(target) => self.shift(target, false, false),
            Instruction::SWAP(target) => self.swap(target),
            Instruction::SRL(target) => self.shift(target, false, true),
            Instruction::BIT(n, target) => self.bit(n, target),
            Instruction::RES(n, target) => self.setres(n, target, false),
            Instruction::SET(n, target) => self.setres(n, target, true),

            Instruction::JP(condition) => self.jump(condition),
            Instruction::JR(condition) => self.jump_relative(condition),

            Instruction::LD(load_type) => self.load(load_type),

            Instruction::PUSH(target) => self.push(target),
            Instruction::POP(target) => self.pop(target),

            Instruction::CALL(condition) => self.call(condition),
            Instruction::RET(condition) => self.ret(condition, false),
            Instruction::RETI => self.ret(JumpCondition::Always, true),
            Instruction::RST(vec) => self.rst(vec),

            Instruction::DAA => self.daa(),
            Instruction::SCF => self.scf(),
            Instruction::CPL => self.cpl(),
            Instruction::CCF => self.ccf(),
            Instruction::DI => self.di(),
            Instruction::EI => self.ei(),
            Instruction::NOP => { self.read_next_byte(); 4 },
            Instruction::HALT => { self.is_halted = true; 4 },
            Instruction::STOP => self.stop(),
        }
    }

    /*
        ADD and ADC instruction.
        Reads the current value from target (register or memory value). Adds the
        value to register A or HL depending on instruction overflowing if necessary.
    */
    fn add(&mut self, target: ArithmeticType, carry: bool) -> usize {
        let mut cycles = 4; // Default number of cycles for ADD instruction
        match target {
            ArithmeticType::Byte(byte_target) => {
                let mut value: u8;
                match byte_target {
                    ByteTarget::A => { value = self.registers.a; },
                    ByteTarget::B => { value = self.registers.b; },
                    ByteTarget::C => { value = self.registers.c; },
                    ByteTarget::D => { value = self.registers.d; },
                    ByteTarget::E => { value = self.registers.e; },
                    ByteTarget::H => { value = self.registers.h; },
                    ByteTarget::L => { value = self.registers.l; },
                    ByteTarget::D8 => { value = self.read_next_byte(); cycles = 8 },
                    ByteTarget::HLI => { value = self.bus.read_byte(self.registers.get_hl()); cycles = 8 },
                }
                if carry { value += self.registers.f.carry as u8; }
                let (new_value, did_overflow) = self.registers.a.overflowing_add(value);

                // Half carry is set if bit 3 overflows
                self.registers.f.zero = new_value == 0;
                self.registers.f.carry = did_overflow;
                self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

                self.registers.a = new_value;

            },
            ArithmeticType::Word(word_target) => {
                cycles = 8;
                let mut value: u16;
                match word_target {
                    WordTarget::BC => { value = self.registers.get_bc() },
                    WordTarget::DE => { value = self.registers.get_de() },
                    WordTarget::HL => { value = self.registers.get_hl() },
                    WordTarget::SP => { value = self.registers.get_af() },
                }
                if carry { value += self.registers.f.carry as u16; }
                let (new_value, did_overflow) = self.registers.get_hl().overflowing_add(value);

                // Half-carry is set if bit 11 overflows
                self.registers.f.carry = did_overflow;
                self.registers.f.half_carry = (self.registers.get_hl() & 0x0FFF) + (value & 0x0FFF) > 0x0FFF;

                self.registers.set_hl(new_value)
            },
            ArithmeticType::SP => {
                cycles = 16;
                let mut value = self.read_next_byte() as i8 as u16;
                if carry { value += self.registers.f.carry as u16; }
                let (new_value, _) = self.sp.overflowing_add(value);

                // Instruction sets carry bit if overflow occured on bit 7, thus did_overflow can't be used here
                self.registers.f.carry = (self.sp & 0x00FF) + (value & 0x00FF) > 0x00FF;
                self.registers.f.half_carry = (self.sp & 0x000F) + (value & 0x000F) > 0x000F;

                self.sp = new_value;
            },
        }

        // Last flag common for all cases
        self.registers.f.subtract = false;

        cycles
    }

    /*
        SUB and SBC instruction
        Subtracts the target value (and carry flag for SBC) from register A or HL depending on instruction.
    */
    fn sub(&mut self, target: ByteTarget, carry: bool) -> usize {
        let mut cycles = 4;
        let mut value: u8;
        match target {
            ByteTarget::A => { value = self.registers.a; },
            ByteTarget::B => { value = self.registers.b; },
            ByteTarget::C => { value = self.registers.c; },
            ByteTarget::D => { value = self.registers.d; },
            ByteTarget::E => { value = self.registers.e; },
            ByteTarget::H => { value = self.registers.h; },
            ByteTarget::L => { value = self.registers.l; },
            ByteTarget::D8 => { value = self.read_next_byte(); cycles = 8 },
            ByteTarget::HLI => { value = self.bus.read_byte(self.registers.get_hl()); cycles = 8 },
        }
        if carry { value += self.registers.f.carry as u8; }
        let new_value = self.registers.a.wrapping_sub(value);

        // Carry bit is set if subtraction has to borrow
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = value > self.registers.a;
        self.registers.f.half_carry = (value & 0x0F) > (self.registers.a & 0x0F);

        self.registers.a = new_value;

        cycles
    }

    /*
        INC instruction
        Increments target register by 1
    */
    fn inc(&mut self, target: ArithmeticType) -> usize {
        let cycles = 4;
        match target {
            ArithmeticType::Byte(byte_target) => {
                let new_value = match byte_target {
                    ByteTarget::A => { self.registers.a = self.registers.a.wrapping_add(1); self.registers.a },
                    ByteTarget::B => { self.registers.b = self.registers.b.wrapping_add(1); self.registers.b },
                    ByteTarget::C => { self.registers.c = self.registers.c.wrapping_add(1); self.registers.c },
                    ByteTarget::D => { self.registers.d = self.registers.d.wrapping_add(1); self.registers.d },
                    ByteTarget::E => { self.registers.e = self.registers.e.wrapping_add(1); self.registers.e },
                    ByteTarget::H => { self.registers.h = self.registers.h.wrapping_add(1); self.registers.h },
                    ByteTarget::L => { self.registers.l = self.registers.l.wrapping_add(1); self.registers.l },
                    ByteTarget::HLI => {
                        let value = self.bus.read_byte(self.registers.get_hl()).wrapping_add(1);
                        self.bus.write_byte(self.registers.get_hl(), value);
                        value
                    },
                    _ => panic!("Got invalid INC instruction.")
                };

                self.registers.f.zero = new_value == 0;
                self.registers.f.subtract = false;
                self.registers.f.half_carry = (new_value.wrapping_sub(1) & 0x0F) + (new_value & 0x0F) > 0x0F;
            },
            ArithmeticType::Word(word_target) => {
                match word_target {
                    WordTarget::BC => self.registers.set_bc(self.registers.get_bc().wrapping_add(1)),
                    WordTarget::DE => self.registers.set_de(self.registers.get_de().wrapping_add(1)),
                    WordTarget::HL => self.registers.set_hl(self.registers.get_hl().wrapping_add(1)),
                    WordTarget::SP => self.sp = self.sp.wrapping_add(1),
                }
            },
            _ => panic!("Got invalid INC instruction"),
        }

        cycles
    }

    /*
        DEC instruction
        Decrements target register by 1
    */
    fn dec(&mut self, target: ArithmeticType) -> usize {
        let cycles = 4;
        match target {
            ArithmeticType::Byte(byte_target) => {
                let new_value = match byte_target {
                    ByteTarget::A => { self.registers.a = self.registers.a.wrapping_sub(1); self.registers.a },
                    ByteTarget::B => { self.registers.b = self.registers.b.wrapping_sub(1); self.registers.b },
                    ByteTarget::C => { self.registers.c = self.registers.c.wrapping_sub(1); self.registers.c },
                    ByteTarget::D => { self.registers.d = self.registers.d.wrapping_sub(1); self.registers.d },
                    ByteTarget::E => { self.registers.e = self.registers.e.wrapping_sub(1); self.registers.e },
                    ByteTarget::H => { self.registers.h = self.registers.h.wrapping_sub(1); self.registers.h },
                    ByteTarget::L => { self.registers.l = self.registers.l.wrapping_sub(1); self.registers.l },
                    ByteTarget::HLI => {
                        let value = self.bus.read_byte(self.registers.get_hl()).wrapping_sub(1);
                        self.bus.write_byte(self.registers.get_hl(), value);
                        value
                    },
                    _ => panic!("Got invalid DEC instruction")
                };

                self.registers.f.zero = new_value == 0;
                self.registers.f.subtract = true;
                self.registers.f.half_carry = (new_value & 0x0F) > (new_value.wrapping_add(1) & 0x0F);
            },
            ArithmeticType::Word(word_target) => {
                match word_target {
                    WordTarget::BC => self.registers.set_bc(self.registers.get_bc().wrapping_sub(1)),
                    WordTarget::DE => self.registers.set_de(self.registers.get_de().wrapping_sub(1)),
                    WordTarget::HL => self.registers.set_hl(self.registers.get_hl().wrapping_sub(1)),
                    WordTarget::SP => self.sp = self.sp.wrapping_sub(1),
                }
            },
            _ => panic!("Got invalid DEC instruction"),
        }

        cycles
    }

    /*
        AND instruction
        Performs bitwise AND between target value and register A and stores the result in A
    */
    fn and(&mut self, target: ByteTarget) -> usize {
        let mut cycles = 4;
        let value: u8;
        match target {
            ByteTarget::A => { value = self.registers.a; },
            ByteTarget::B => { value = self.registers.b; },
            ByteTarget::C => { value = self.registers.c; },
            ByteTarget::D => { value = self.registers.d; },
            ByteTarget::E => { value = self.registers.e; },
            ByteTarget::H => { value = self.registers.h; },
            ByteTarget::L => { value = self.registers.l; },
            ByteTarget::D8 => { value = self.read_next_byte(); cycles = 8 },
            ByteTarget::HLI => { value = self.bus.read_byte(self.registers.get_hl()); cycles = 8 },
        };
        self.registers.a = self.registers.a & value;

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = true;

        cycles
    }

    /*
        XOR instruction
        Performs bitwise XOR between target value and register A and stores the result in A
    */
    fn xor(&mut self, target: ByteTarget) -> usize {
        let mut cycles = 4;
        let value: u8;
        match target {
            ByteTarget::A => { value = self.registers.a; },
            ByteTarget::B => { value = self.registers.b; },
            ByteTarget::C => { value = self.registers.c; },
            ByteTarget::D => { value = self.registers.d; },
            ByteTarget::E => { value = self.registers.e; },
            ByteTarget::H => { value = self.registers.h; },
            ByteTarget::L => { value = self.registers.l; },
            ByteTarget::D8 => { value = self.read_next_byte(); cycles = 8 },
            ByteTarget::HLI => { value = self.bus.read_byte(self.registers.get_hl()); cycles = 8 },
        };
        self.registers.a = self.registers.a ^ value;

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;

        cycles
    }

    /*
        OR instruction
        Performs bitwise OR between target value and register A and stores the result in A
    */
    fn or(&mut self, target: ByteTarget) -> usize {
        let mut cycles = 4;
        let value: u8;
        match target {
            ByteTarget::A => { value = self.registers.a; },
            ByteTarget::B => { value = self.registers.b; },
            ByteTarget::C => { value = self.registers.c; },
            ByteTarget::D => { value = self.registers.d; },
            ByteTarget::E => { value = self.registers.e; },
            ByteTarget::H => { value = self.registers.h; },
            ByteTarget::L => { value = self.registers.l; },
            ByteTarget::D8 => { value = self.read_next_byte(); cycles = 8 },
            ByteTarget::HLI => { value = self.bus.read_byte(self.registers.get_hl()); cycles = 8 },
        };
        self.registers.a = self.registers.a | value;

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;

        cycles
    }

    /*
        CP instruction
        Subtracts the target value from A and sets flags. Doesn't store the result. Used for comparison.
    */
    fn cp(&mut self, target: ByteTarget) -> usize {
        let mut cycles = 4;
        let value: u8;
        match target {
            ByteTarget::A => { value = self.registers.a; },
            ByteTarget::B => { value = self.registers.b; },
            ByteTarget::C => { value = self.registers.c; },
            ByteTarget::D => { value = self.registers.d; },
            ByteTarget::E => { value = self.registers.e; },
            ByteTarget::H => { value = self.registers.h; },
            ByteTarget::L => { value = self.registers.l; },
            ByteTarget::D8 => { value = self.read_next_byte(); cycles = 8 },
            ByteTarget::HLI => { value = self.bus.read_byte(self.registers.get_hl()); cycles = 8 },
        }
        let new_value = self.registers.a.wrapping_sub(value);

        // Carry bit is set if subtraction has to borrow
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = value > self.registers.a;
        self.registers.f.half_carry = (value & 0x0F) > (self.registers.a & 0x0F);

        cycles
    }

    /**
     * Rotate instruction
     * Contains RL, RR, RLC, RLCA, RLA, RRC, RRCA and RRA
     * Rotates register left. Carry parameter specifies whether to rotate through carry or not
     */
    fn rotate(&mut self, target: ByteTarget, carry: bool, left: bool) -> usize {
        let mut cycles = 8;
        let mut source_value = match target {
            ByteTarget::A => { cycles = 4; self.registers.a },
            ByteTarget::B => self.registers.b, // TODO definately make this into a method
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::HLI => { cycles = 16; self.bus.read_byte(self.registers.get_hl()) },
            ByteTarget::D8 => panic!("Got invalid enum ByteTarget::D8 in rotate instruction"),
        };

        let c: u8;
        if left {
            source_value = source_value.rotate_left(1);
            c = source_value & 0x01;
        } else {
            source_value = source_value.rotate_right(1);
            c = source_value & 0x80;
        }

        // Set wrapped bit to carry if instruction is 'through carry'
        if carry {
            // Toggle the bit only if it's different from the carry flag
            if left && source_value & 0x01 != self.registers.f.carry as u8 {
                source_value ^= 0x01;
            }
            if !left && source_value & 0x80 != (self.registers.f.carry as u8) << 7 {
                source_value ^= 0x80;
            }
        }

        match target {
            ByteTarget::A => self.registers.a = source_value, // TODO definately make this into a method
            ByteTarget::B => self.registers.b = source_value,
            ByteTarget::C => self.registers.c = source_value,
            ByteTarget::D => self.registers.d = source_value,
            ByteTarget::E => self.registers.e = source_value,
            ByteTarget::H => self.registers.h = source_value,
            ByteTarget::L => self.registers.l = source_value,
            ByteTarget::HLI => self.bus.write_byte(self.registers.get_hl(), source_value),
            _ => {},
        }

        self.registers.f.zero = source_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = c != 0;
        self.registers.f.half_carry = false;

        cycles
    }

    /**
     * Shift instruction
     * Contains SLA and SRA.
     * Shift the target left or right. Logic bool indicates whether to do a logical shift or an arithmetic shift.
     */
    fn shift(&mut self, target: ByteTarget, left: bool, logic: bool) -> usize {
        let mut cycles = 8;
        let mut source_value = match target {
            ByteTarget::A => self.registers.a,
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::HLI => { cycles = 16; self.bus.read_byte(self.registers.get_hl()) },
            ByteTarget::D8 => panic!("Got invalid enum ByteTarget::D8 in shift instruction"),
        };

        let msb = source_value & 0x80;
        let lsb = source_value & 0x01;
        let c = if left { msb >> 7 } else { lsb };
        source_value = if left { source_value << 1 } else { source_value >> 1 };

        // Right arithmetic shift inserts msb insted of 0
        if !logic && !left {
            source_value = source_value | msb;
        }

        match target {
            ByteTarget::A => self.registers.a = source_value,
            ByteTarget::B => self.registers.b = source_value,
            ByteTarget::C => self.registers.c = source_value,
            ByteTarget::D => self.registers.d = source_value,
            ByteTarget::E => self.registers.e = source_value,
            ByteTarget::H => self.registers.h = source_value,
            ByteTarget::L => self.registers.l = source_value,
            ByteTarget::HLI => self.bus.write_byte(self.registers.get_hl(), source_value),
            _ => {},
        }

        self.registers.f.zero = source_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = c != 0;

        cycles
    }

    /**
     * SWAP instruction
     * Swaps the lower 4 bits and upper 4 bits
     */
    fn swap(&mut self, target: ByteTarget) -> usize {
        let mut cycles = 8;
        let source_value = match target {
            ByteTarget::A => self.registers.a,
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::HLI => { cycles = 16; self.bus.read_byte(self.registers.get_hl()) },
            ByteTarget::D8 => panic!("Got invalid enum ByteTarget::D8 in SWAP instruction"),
        };

        let lower = source_value & 0x0F;
        let upper = source_value & 0xF0;
        let source_value = lower << 4 | upper >> 4;

        match target {
            ByteTarget::A => self.registers.a = source_value,
            ByteTarget::B => self.registers.b = source_value,
            ByteTarget::C => self.registers.c = source_value,
            ByteTarget::D => self.registers.d = source_value,
            ByteTarget::E => self.registers.e = source_value,
            ByteTarget::H => self.registers.h = source_value,
            ByteTarget::L => self.registers.l = source_value,
            ByteTarget::HLI => self.bus.write_byte(self.registers.get_hl(), source_value),
            _ => {},
        }

        self.registers.f.zero = source_value == 0; // TODO create method for resetting all flags
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;

        cycles
    }

    /**
     * BIT instruction
     * Tests bit n in target and sets zero flag if the bit is not set
     */
    fn bit(&mut self, n: u8, target: ByteTarget) -> usize {
        let mut cycles = 8;
        let source_value = match target {
            ByteTarget::A => self.registers.a,
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::HLI => { cycles = 12; self.bus.read_byte(self.registers.get_hl()) },
            ByteTarget::D8 => panic!("Got invalid enum ByteTarget::D8 in BIT instruction"),
        };

        let bit = source_value >> n & 0x01;
        self.registers.f.zero = bit == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = true;

        cycles
    }

    /**
     * SET and RES instruction
     * Sets or resets bit n in target
     */
    fn setres(&mut self, n: u8, target: ByteTarget, set: bool) -> usize {
        let mut cycles = 8;
        let mut source_value = match target {
            ByteTarget::A => self.registers.a,
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::HLI => { cycles = 16; self.bus.read_byte(self.registers.get_hl()) },
            ByteTarget::D8 => panic!("Got invalid enum ByteTarget::D8 in RES instruction"),
        };

        let bit = source_value >> n & 0x01;
        if (set && bit == 0) || (!set && bit != 0) {
            source_value ^= 1 << n
        }

        match target {
            ByteTarget::A => self.registers.a = source_value,
            ByteTarget::B => self.registers.b = source_value,
            ByteTarget::C => self.registers.c = source_value,
            ByteTarget::D => self.registers.d = source_value,
            ByteTarget::E => self.registers.e = source_value,
            ByteTarget::H => self.registers.h = source_value,
            ByteTarget::L => self.registers.l = source_value,
            ByteTarget::HLI => self.bus.write_byte(self.registers.get_hl(), source_value),
            _ => {},
        }

        cycles
    }

    /*
        JMP instruction
        Jumps to a location given by the next 2 bytes if one of the following conditions are met
        flag zero is set, flag carry is set or always jump.
    */
    fn jump(&mut self, jump_type: JumpType) -> usize {
        let mut cycles = 4;
        let jump_condition;
        match jump_type {
            JumpType::Word(condition_check) => {
                jump_condition = match condition_check {
                    JumpCondition::NotZero => !self.registers.f.zero,
                    JumpCondition::NotCarry => !self.registers.f.carry,
                    JumpCondition::Zero => self.registers.f.zero,
                    JumpCondition::Carry => self.registers.f.carry,
                    JumpCondition::Always => true,
                };

                if jump_condition {
                    cycles = 16;
                    self.pc = self.read_next_word();
                } else {
                    cycles = 12;
                    self.read_next_word();
                }
            },
            JumpType::Address => {
                self.pc = self.registers.get_hl();
            }
        }

        cycles
    }

    /*
        JR instruction
        Reads the next byte as i8 and jumps to pc + i8 if jump condition is set.
        NOTE: Pc needs to be added to the next instructions' pc, which is automatically done in this case.
    */
    fn jump_relative(&mut self, jump_condition: JumpCondition) -> usize {
        let mut cycles = 8;
        let relative_val = self.read_next_byte() as i8 as u16;

        let jump = match jump_condition {
            JumpCondition::NotZero => !self.registers.f.zero,
            JumpCondition::NotCarry => !self.registers.f.carry,
            JumpCondition::Zero => self.registers.f.zero,
            JumpCondition::Carry => self.registers.f.carry,
            JumpCondition::Always => true,
        };

        if jump {
            self.pc = self.pc.wrapping_add(relative_val);
            cycles = 12;
        }

        cycles
    }

    /*
        LD instruction
        Loads either from memory into registers or vice versa
    */
    fn load(&mut self, load_type: LoadType) -> usize {
        let mut cycles = 4;
        match load_type {
            LoadType::Byte(target, source) => {
                let source_value = match source {
                    LoadByteSource::A => self.registers.a,
                    LoadByteSource::B => self.registers.b,
                    LoadByteSource::C => self.registers.c,
                    LoadByteSource::D => self.registers.d,
                    LoadByteSource::E => self.registers.e,
                    LoadByteSource::H => self.registers.h,
                    LoadByteSource::L => self.registers.l,
                    LoadByteSource::D8 => { cycles += 4; self.read_next_byte() },
                    LoadByteSource::HLI => { cycles += 4; self.bus.read_byte(self.registers.get_hl()) },
                };
                match target {
                    LoadByteTarget::A => self.registers.a = source_value,
                    LoadByteTarget::B => self.registers.b = source_value,
                    LoadByteTarget::C => self.registers.c = source_value,
                    LoadByteTarget::D => self.registers.d = source_value,
                    LoadByteTarget::E => self.registers.e = source_value,
                    LoadByteTarget::H => self.registers.h = source_value,
                    LoadByteTarget::L => self.registers.l = source_value,
                    LoadByteTarget::HLI => { cycles += 4; self.bus.write_byte(self.registers.get_hl(), source_value) },
                };
            },
            LoadType::Word(target, source) => {
                cycles = 12;
                let source_value = match source {
                    LoadWordSource::D16 => self.read_next_word(),
                    LoadWordSource::SP => self.sp,
                };
                match target {
                    LoadWordTarget::BC => self.registers.set_bc(source_value),
                    LoadWordTarget::DE => self.registers.set_de(source_value),
                    LoadWordTarget::HL => self.registers.set_hl(source_value),
                    LoadWordTarget::SP => self.sp = source_value,
                    LoadWordTarget::D16 => {
                        let word = self.read_next_word();
                        self.bus.write_byte(word, (source_value & 0x00FF) as u8);
                        cycles = 20
                    }
                }
            },
            LoadType::AFromIndirect(source) => {
                cycles = 8;
                let source_value = match source {
                    LoadMemoryLocation::BC => self.bus.read_byte(self.registers.get_bc()),
                    LoadMemoryLocation::DE => self.bus.read_byte(self.registers.get_de()),
                    LoadMemoryLocation::HLpostinc => {
                        let value = self.bus.read_byte(self.registers.get_hl());
                        self.registers.set_hl(self.registers.get_hl().wrapping_add(1));
                        value
                    },
                    LoadMemoryLocation::HLpredec => {
                        self.registers.set_hl(self.registers.get_hl().wrapping_sub(1));
                        self.bus.read_byte(self.registers.get_hl())
                    }
                };
                self.registers.a = source_value;
            },
            LoadType::IndirectFromA(target) => {
                cycles = 8;
                let source_value = self.registers.a;
                match target {
                    LoadMemoryLocation::BC => self.bus.write_byte(self.registers.get_bc(), source_value),
                    LoadMemoryLocation::DE => self.bus.write_byte(self.registers.get_de(), source_value),
                    LoadMemoryLocation::HLpostinc => {
                        self.bus.write_byte(self.registers.get_hl(), source_value);
                        self.registers.set_hl(self.registers.get_hl().wrapping_add(1));
                    },
                    LoadMemoryLocation::HLpredec => {
                        self.registers.set_hl(self.registers.get_hl().wrapping_sub(1));
                        self.bus.write_byte(self.registers.get_hl(), source_value);
                    },
                }
            },
            LoadType::AFromByteAddress(source) => {
                cycles = 8;
                let source_value = match source {
                    ByteAddress::D8 => {
                        cycles = 12;
                        let byte = self.read_next_byte();
                        self.bus.read_byte(0xFF00 + byte as u16)
                    },
                    ByteAddress::C => self.bus.read_byte(0xFF00 + self.registers.c as u16),
                    ByteAddress::D16 => { let word = self.read_next_word(); self.bus.read_byte(word) },
                };
                self.registers.a = source_value;
            },
            LoadType::ByteAddressFromA(target) => {
                cycles = 8;
                let source_value = self.registers.a;
                match target {
                    ByteAddress::D8 => {
                        cycles = 12;
                        let byte = self.read_next_byte();
                        self.bus.write_byte(0xFF00 + byte as u16, source_value)
                    },
                    ByteAddress::C => self.bus.write_byte(0xFF00 + self.registers.c as u16, source_value),
                    ByteAddress::D16 => { let word = self.read_next_word(); self.bus.write_byte(word, source_value) },
                }
            },
            LoadType::HLFromSP => {
                cycles = 12;
                let next_byte = self.read_next_byte() as i8 as u16;
                let source_value = self.sp + next_byte;
                self.registers.set_hl(source_value);

                self.registers.f.zero = false;
                self.registers.f.subtract = false;
                self.registers.f.carry = (self.sp & 0x00FF) + (next_byte & 0x00FF) > 0x00FF;
                self.registers.f.half_carry = (self.sp & 0x000F) + (next_byte & 0x000F) > 0x000F;
            },
            LoadType::SPFromHL => {
                cycles = 8;
                self.sp = self.registers.get_hl();
            }
        }

        cycles
    }

    /*
        PUSH instruction.
        Pushes a value from the target register onto the stack.
    */
    fn push(&mut self, target: StackTarget) -> usize {
        let value = match target {
            StackTarget::AF => self.registers.get_af(),
            StackTarget::BC => self.registers.get_bc(),
            StackTarget::DE => self.registers.get_de(),
            StackTarget::HL => self.registers.get_hl(),
        };
        self.push_value(value);

        16
    }

    // Separate method for pushing a value to the stack
    fn push_value(&mut self, value: u16) {
        self.sp = self.sp.wrapping_sub(1);
        self.bus.write_byte(self.sp, ((value & 0xFF00) >> 8) as u8);

        self.sp = self.sp.wrapping_sub(1);
        self.bus.write_byte(self.sp, (value & 0x00FF) as u8);
    }

    /*
        POP instruction
        Pops a value from the stack onto the target register
    */
    fn pop(&mut self, target: StackTarget) -> usize {
        let value = self.pop_value();
        match target {
            StackTarget::AF => {
                self.registers.set_af(value);

                // AF sets flags for some reason
                self.registers.f.zero = value & 0x80 != 0;
                self.registers.f.subtract = value & 0x40 != 0;
                self.registers.f.carry = value & 0x20 != 0;
                self.registers.f.half_carry = value & 0x10 != 0;
            }
            StackTarget::BC => self.registers.set_bc(value),
            StackTarget::DE => self.registers.set_de(value),
            StackTarget::HL => self.registers.set_hl(value),
        };

        12
    }

    // Separate function for popping a value from the stack
    fn pop_value(&mut self) -> u16 {
        let lsb = self.bus.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        let msb = self.bus.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        (msb << 8) | lsb
    }

    /*
        CALL instruction
        Calls a function by setting the pc to the address given by the next 2 bytes
    */
    fn call(&mut self, test: JumpCondition) -> usize {
        let mut cycles = 12;
        let jump_condition = match test {
            JumpCondition::NotZero => !self.registers.f.zero,
            JumpCondition::NotCarry => !self.registers.f.carry,
            JumpCondition::Zero => self.registers.f.zero,
            JumpCondition::Carry => self.registers.f.carry,
            JumpCondition::Always => true,
        };

        if jump_condition {
            cycles = 24;
            let new_pc = self.read_next_word();
            self.push_value(self.pc);
            self.pc = new_pc;
        }

        cycles
    }

    /*
        RET instruction
        Returns from the function call.
    */
    fn ret(&mut self, test: JumpCondition, enable_interrupts: bool) -> usize {
        let mut cycles = 8;
        let jump_condition = match test {
            JumpCondition::NotZero => !self.registers.f.zero,
            JumpCondition::NotCarry => !self.registers.f.carry,
            JumpCondition::Zero => self.registers.f.zero,
            JumpCondition::Carry => self.registers.f.carry,
            JumpCondition::Always => true,
        }; // TODO make this into a method on the JumpCondition enum maybe? not sure how due to self.register

        if jump_condition {
            cycles = 20;
            self.pc = self.pop_value();
        }
        if let JumpCondition::Always = test {cycles = 16}

        // Simpler than calling EI before RET
        if enable_interrupts {
            self.ime = true;
        }

        cycles
    }

    /*
        RST instruction
        Call to address vec. Faster than CALL, but for a limited amount of locations. Also called reset
    */
    fn rst(&mut self, vec: RSTVec) -> usize {
        self.push_value(self.pc);
        self.pc = 0x0000 + vec as u16;

        16
    }

    /*
        DAA instruction
        Changes register A to contain a decimal number after an arithmetic operation.
        For example: 15+27 = 3C, running DAA after changes it to 42.
    */
    fn daa(&mut self) -> usize {
        let mut reg_a = self.registers.a;

        if !self.registers.f.subtract {
            // Lower nibble
            if (reg_a & 0x0F) > 9 || self.registers.f.half_carry {
                // larger than 9 means that it contains hex values (A, B, C...).
                // Also have to add if lower nibble overflow occured.
                // Add 6 to offset
                reg_a += 0x06;
            }

            // Upper nibble
            if (reg_a & 0xF0) > 0x90 || self.registers.f.carry {
                reg_a += 0x60;
                self.registers.f.carry = true;
            }
        } else {
            // Same for subtraction, except we subtract 6
            // Lower nibble
            if (reg_a & 0x0F) > 0x09 || self.registers.f.half_carry {
                reg_a -= 0x06;
                if (reg_a & 0xF0) == 0xF0 {
                    // Set carry flag if underflow occured
                    self.registers.f.carry = true;
                }
            }

            // Upper nibble
            if (reg_a & 0xF0) > 0x90 || self.registers.f.carry {
                reg_a -= 0x60;
                self.registers.f.carry = true;
            }
        }

        if reg_a == 0 {
            self.registers.f.zero = true;
        } else {
            self.registers.f.zero = false;
        }

        self.registers.f.half_carry = false;
        self.registers.a = reg_a;

        4
    }

    /*
        SCF instruction
        Sets the carry flag. Also resets N and H flags.
    */
    fn scf(&mut self) -> usize {
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = true;

        4
    }

    /**
     * CPL instruction
     * Complements accumulator (A = ~A)
     */
    fn cpl(&mut self) -> usize {
        self.registers.a = self.registers.a.wrapping_neg();

        4
    }

    /**
     * CCF instruction
     * Complements carry flag. Also resets N and H flags
     */
    fn ccf(&mut self) -> usize {
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = !self.registers.f.carry;

        4
    }

    /**
     * DI instruction
     * Disables interrupt by clearing IME flag.
     */
    fn di(&mut self) -> usize {
        self.ime = false;

        4
    }

    /**
     * EI instruction
     * Enables interrupt by setting IME flag. The flag is only set AFTER the instruction following EI
     */
    fn ei(&mut self) -> usize {
        self.ei_called = true;

        4
    }

    /**
     * STOP instruction
     * Enters CPU very low power mode.
     */
    fn stop(&mut self) -> usize{
        // TODO find out if this does anything at all

        4
    }

    // Reads the next byte and increments the pc
    fn read_next_byte(&mut self) -> u8 {
        let next_byte = self.bus.read_byte(self.pc);
        self.pc = self.pc.wrapping_add(1);
        next_byte
    }

    // Reads the next word  and increments the pc
    fn read_next_word(&mut self) -> u16 {
        let right = self.bus.read_byte(self.pc) as u16;
        let left = (self.bus.read_byte(self.pc.wrapping_add(1)) as u16) << 8;
        self.pc = self.pc.wrapping_add(2);
        left | right
    }
}

const CLOCK: usize = 4194304; // in Hz
const FPS: usize = 30;
const ONE_FRAME_IN_CYCLES: usize = CLOCK / ( 4 * FPS );

fn run(mut cpu: CPU, mut window: Window, mut tileset_window: Window) {
    let mut window_buffer = [0; WIDTH * HEIGHT];
    let mut tileset_window_buffer = [0; TILESET_WIDTH * TILESET_HEIGHT];
    let mut cycles_elapsed_in_frame = 0usize;
    let mut now = Instant::now();

    while window.is_open() && tileset_window.is_open() &&
            !window.is_key_down(Key::Escape) && !tileset_window.is_key_down(Key::Escape) {
        // Check how much time (in nanoseconds) has passed
        let time_delta = now.elapsed().subsec_nanos();
        now = Instant::now();

        // Gameboy knows how many times to call step in this time
        // Can't just run ONE_FRAME_IN_CYCLES steps as the emulator can run quicker than the actual gameboy
        cycles_elapsed_in_frame += cpu.run(time_delta);

        // If the gameboy should update the screen
        if cycles_elapsed_in_frame >= ONE_FRAME_IN_CYCLES {
            for (i, pixel) in cpu.pixel_buffer().iter().enumerate() {
                window_buffer[i] = *pixel;
            }
            for (i, pixel) in cpu.tileset_buffer().iter().enumerate() {
                tileset_window_buffer[i] = *pixel;
            }
            window.update_with_buffer(&window_buffer, WIDTH, HEIGHT).unwrap();
            tileset_window.update_with_buffer(&tileset_window_buffer, TILESET_WIDTH, TILESET_HEIGHT).unwrap();
            cycles_elapsed_in_frame = 0;
        } else {
            sleep(Duration::from_nanos(2))
        }
    }
}

fn main() {
    let mut window = Window::new(
        "Gameboy",
        WIDTH,
        HEIGHT,
        WindowOptions::default()
    )
    .unwrap_or_else(|e| {
        panic!("{}", e);
    });

    let mut tileset_window = Window::new(
        "Tileset",
        TILESET_WIDTH,
        TILESET_HEIGHT,
        WindowOptions::default()
    )
    .unwrap_or_else(|e| {
        panic!("{}", e);
    });


    // gameboy framerate is 59.727500569606 Hz
    window.limit_update_rate(Some(std::time::Duration::from_micros(1667)));
    tileset_window.limit_update_rate(Some(std::time::Duration::from_micros(1667)));

    let cpu = CPU::new();

    run(cpu, window, tileset_window);
}
