use minifb::{Key, Window, WindowOptions};
use std::time::{Duration, Instant};
use std::thread::sleep;

const WIDTH: usize = 160;
const HEIGHT: usize = 144;

const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBTRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

/*
    FlagsRegister is an abstraction of the f register on the CPU.
    bits 0-3 are unused and bits 4-7 are described by constants.
*/
#[derive(Clone, Copy)]
struct FlagsRegister {
    zero: bool,
    subtract: bool,
    half_carry: bool,
    carry: bool,
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
enum TilePixelValue {
    Zero,
    One,
    Two,
    Three,
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
}

/*
    All CPU instructions for LR35902
*/
enum Instruction {
    ADD(ArithmeticTarget),
    ADDHL(ArithmeticTarget),
    SUB(ArithmeticTarget),
    SBC,
    AND,
    OR,
    XOR,
    CP,
    INC,
    DEC,
    CCF,
    SCF,
    RRA,
    RLA,
    RRCA,
    RRLA,
    CPL,
    BIT,
    RESET,
    SET,
    SRL,
    RR,
    RL,
    RRC,
    RLC,
    SRA,
    SLA,
    SWAP,
    // Jump instructions
    JP(JumpTest),
    // Memory instructions
    LD(LoadType),
    // Stack instructions
    PUSH(StackTarget),
    POP(StackTarget),
    // Function call
    CALL(JumpTest),
    RET(JumpTest),
    // Other
    NOP,
    HALT,
}

enum ArithmeticTarget {
    A, B, C, D, E, H, L,
}

enum JumpTest {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}

enum LoadByteTarget {
    A, B, C, D, E, H, L, HLI,
}

enum LoadByteSource {
    A, B, C, D, E, H, L, D8, HLI,
}

enum LoadWordTarget {
    BC, 
}

enum LoadWordSource {
    A, B, C, D, E, H, L, D8, HLI,
}

enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget, LoadWordSource),
    /*
    TODO add these instructions:
    Word - Byte except 16-bit values
    AFromIndirect
    IndirectFromA
    AFromByteAddress
    ByteAddressFromA
    */
}

enum StackTarget {
    AF,
    BC,
    DE,
    HL,
}

impl Instruction {
    // Converts a byte into an instruction
    // Full opcode table at https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
    fn from_byte(byte: u8, prefixed: bool) -> Option<Instruction> {
        if prefixed {
            Instruction::from_byte_prefixed(byte)
        } else {
            Instruction::from_byte_not_prefixed(byte)
        }
    }

    // TODO should return how many cycles it requires as well as the instruction
    fn from_byte_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x00 => Some(Instruction::NOP),
            0x01 => Some(Instruction::LD)
            _ => None // TODO add all prefix instructions
        }
    }

    fn from_byte_not_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x02 => Some(Instruction::INC(IncDecTarget::BC)),
            0x13 => Some(Instruction::INC(IncDecTarget::DE)),
            _ => None // TODO add all instructions
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
struct MemoryBus {
    memory: [u8; 0xFFFF],
    gpu: GPU,
}

impl MemoryBus {
    fn new() -> Self {
        let mem = MemoryBus {
            memory: [0; 0xFFFF],
            gpu: GPU::new(),
        };

        // Initial memory values. See https://gbdev.io/pandocs/#power-up-sequence
        // Memory defaults to 0 so some entries are removed
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
    is_halted: bool,
}

impl CPU {
    fn new() -> Self {
        CPU {
            registers: Registers::new(),
            pc: 0x0000,
            sp: 0xFFFE, // See https://gbdev.io/pandocs/#power-up-sequence
            bus: MemoryBus::new(),
            is_halted: false,
        }
    }

    // Runs a specified amount of cycles depending on the time that passed since last frame.
    // Returns the number of cycles run
    fn run(&self, time_delta: u32) -> u32 {

    }

    // Executes one CPU cycle. Reads and executes an instruction from the position of the pc and updates the pc
    fn step(&mut self) {
        let mut instruction_byte = self.bus.read_byte(self.pc);
        let prefixed = instruction_byte == 0xCB;
        if prefixed {
            instruction_byte = self.bus.read_byte(self.pc + 1); // TODO add wrap add and see if pc needs to be incremented
        }

        let next_pc = if let Some(instruction) = Instruction::from_byte(instruction_byte, prefixed) {
            self.execute(instruction)
        } else {
            let description = format!("0x{}{:x}", if prefixed { "cb" } else { "" }, instruction_byte);
            panic!("Unknown instruction found for: {}", description);
        };

        self.pc = next_pc;
    }

    /*
        Executes a given instruction on the CPU.
        Every instruction implementation is self responsible for returning the new pc location.
        This is done because some instructions like JMP will change the location arbitrarily.
        NOTE: pc increments should use wrapping_add()
    */
    fn execute(&mut self, instruction: Instruction) -> u16 {
        if self.is_halted {
            return self.pc
        }

        match instruction {
            Instruction::ADD(target) => self.add(target),
            Instruction::JP(test) => self.jump(test),
            Instruction::LD(load_type) => self.load(load_type),
            Instruction::PUSH(target) => self.push(target),
            Instruction::POP(target) => self.pop(target),
            Instruction::CALL(test) => self.call(test),
            Instruction::RET(test) => self.ret(test),
            Instruction::NOP => self.pc.wrapping_add(1),
            Instruction::HALT => { self.is_halted = true; self.pc },
            _ => {panic!("Instruction not found")} // TODO add more instructions
        }
    }

    /*
        ADD instruction.
        Reads the current value from target register. Adds the value to register A overflowing if necessary.
        Updates the flags register and writer the new value to register A.
    */
    fn add(&mut self, target: ArithmeticTarget) -> u16 {
        let value: u8;
        match target {
            ArithmeticTarget::A => { let value = self.registers.a; },
            ArithmeticTarget::B => { let value = self.registers.b; },
            ArithmeticTarget::C => { let value = self.registers.c; },
            ArithmeticTarget::D => { let value = self.registers.d; },
            ArithmeticTarget::E => { let value = self.registers.e; },
            ArithmeticTarget::H => { let value = self.registers.h; },
            ArithmeticTarget::L => { let value = self.registers.l; },
        }
        let (new_value, did_overflow) = self.registers.a.overflowing_add(value);
        /*
            Flags have to be set according to the following rules:
            Zero: true if the result of the operation is 0
            Subtract: true if the operation was a subtraction
            Carry: true if operation resulted in overflow
            Half Carry: true if the operation resulted in overflow from lower nibble to upper nibble
        */
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        // Check if half carry needs to be set by adding both lower nibbles together and see if the result is greater than 1111 (0xF)
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        self.registers.a = new_value;

        self.pc.wrapping_add(1)
    }

    /*
        JMP instruction
        Jumps to a location given by the next 2 bytes if one of the following conditions are met
        flag zero is set, flag carry is set or always jump.
    */
    fn jump(&self, test: JumpTest) -> u16 {
        let jump_condition = match test {
            JumpTest::NotZero => !self.registers.f.zero,
            JumpTest::NotCarry => !self.registers.f.carry,
            JumpTest::Zero => self.registers.f.zero,
            JumpTest::Carry => self.registers.f.carry,
            JumpTest::Always => true,
        };

        if jump_condition {
            // If the jump condition is set, read the next 2 bytes and return it (location of the jump)
            let least_significant_byte = self.bus.read_byte(self.pc + 1) as u16;
            let most_significant_byte = self.bus.read_byte(self.pc + 2) as u16;
            (most_significant_byte << 8) | least_significant_byte
        } else {
            // Jump condition not set, increment pc by 3 bytes (1 for instruction and 2 for jump location)
            self.pc.wrapping_add(3)
        }
    }

    /*
        LD instruction
        Loads either from memory into registers or vice versa
    */
    fn load(&mut self, load_type: LoadType) -> u16 {
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
                    LoadByteSource::D8 => self.read_next_byte(),
                    LoadByteSource::HLI => self.bus.read_byte(self.registers.get_hl()),
                };
                match target {
                    LoadByteTarget::A => self.registers.a = source_value,
                    LoadByteTarget::B => self.registers.b = source_value,
                    LoadByteTarget::C => self.registers.c = source_value,
                    LoadByteTarget::D => self.registers.d = source_value,
                    LoadByteTarget::E => self.registers.e = source_value,
                    LoadByteTarget::H => self.registers.h = source_value,
                    LoadByteTarget::L => self.registers.l = source_value,
                    LoadByteTarget::HLI => self.bus.write_byte(self.registers.get_hl(), source_value),
                };
                match source {
                    // D8 reads next byte, so pc needs to skip it
                    LoadByteSource::D8 => self.pc.wrapping_add(2),
                    _                  => self.pc.wrapping_add(1)
                }
            }
        }
    }

    /*
        PUSH instruction.
        Pushes a value from the target register onto the stack.
    */
    fn push(&mut self, target: StackTarget) -> u16 {
        let value = match target {
            StackTarget::AF => self.registers.get_af(),
            StackTarget::BC => self.registers.get_bc(),
            StackTarget::DE => self.registers.get_de(),
            StackTarget::HL => self.registers.get_hl(),
        };
        self.push_value(value);
        self.pc.wrapping_add(1)
    }

    // Separate function for pushing a value to the stack
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
    fn pop(&mut self, target: StackTarget) -> u16 {
        let value = self.pop_value();
        match target {
            StackTarget::AF => self.registers.set_af(value),
            StackTarget::BC => self.registers.set_bc(value),
            StackTarget::DE => self.registers.set_de(value),
            StackTarget::HL => self.registers.set_hl(value),
        };

        self.pc.wrapping_add(1)
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
    fn call(&self, test: JumpTest) -> u16 {
        let jump_condition = match test {
            JumpTest::NotZero => !self.registers.f.zero,
            JumpTest::NotCarry => !self.registers.f.carry,
            JumpTest::Zero => self.registers.f.zero,
            JumpTest::Carry => self.registers.f.carry,
            JumpTest::Always => true,
        };

        let next_pc = self.pc.wrapping_add(3); // Jump past address when returning from call
        if jump_condition {
            self.push_value(next_pc);
            self.read_next_word()
        } else {
            next_pc
        }
    }

    /*
        RET instruction
        Returns from the function call.
    */
    fn ret(&mut self, test: JumpTest) -> u16 {
        let jump_condition = match test {
            JumpTest::NotZero => !self.registers.f.zero,
            JumpTest::NotCarry => !self.registers.f.carry,
            JumpTest::Zero => self.registers.f.zero,
            JumpTest::Carry => self.registers.f.carry,
            JumpTest::Always => true,
        }; // TODO make this into a method on the JumpTest enum maybe? not sure how due to self.register

        if jump_condition {
            self.pop_value()
        } else {
            self.pc.wrapping_add(1)
        }
    }

    // Reads the next byte from where the pc is
    fn read_next_byte(&self) -> u8 {
        self.bus.read_byte(self.pc.wrapping_add(1))
    }

    // Reads the next 2 bytes from where the pc is
    fn read_next_word(&self) -> u16 {
        (self.bus.read_byte(self.pc.wrapping_add(1)) as u16) << 8 | self.bus.read_byte(self.pc.wrapping_add(2)) as u16
    }
}

fn run(mut cpu: CPU, mut window: Window) {
    let mut window_buffer = [0; WIDTH * HEIGHT];
    let mut cycles_elapsed_in_frame = 0usize;
    let mut now = Instant::now();

    while window.is_open() && !window.is_key_down(Key::Escape) {
        // Check how much time (in nanoseconds) has passed
        let time_delta = now.elapsed().subsec_nanos();
        now = Instant::now();

        cycles_elapsed_in_frame += cpu.run(time_delta); // Gameboy knows how many times to call step in this time
        // If the gameboy should update the screen
        if cycles_elapsed_in_frame >= ONE_FRAME_IN_CYCLES { // 30 times per second, should update
            for (i, pixel) in cpu.pixel_buffer().enumerate() {
                window_buffer[i] = pixel
            }
            window.update_with_buffer(&window_buffer, WIDTH, HEIGHT);
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

    window.limit_update_rate(Some(std::time::Duration::from_micros(8300)));

    let mut cpu = CPU{};

    run(cpu, window);
}
