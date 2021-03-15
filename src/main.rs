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
    // Arithmetic instructions
    ADD(ArithmeticType),  // Add target to register A
    ADC(ArithmeticType),  // Add the target plus carry flag to register A
    SUB(ArithmeticType),  // Subtract target from register A
    SBC(ArithmeticType),  // Subtract target and carry flag from register A
    INC(ArithmeticType),  // Increment target
    DEC(ArithmeticType),  // Decrement target
    
    // Logic instructions
    AND(ByteTarget),  // Bitwise AND with target and register A into A
    XOR(ByteTarget),  // Bitwise XOR with target and register A into A
    OR(ByteTarget),   // Bitwise OR with target and register A into A
    CP(ByteTarget), // Subtract target from A but don't store value. Used to do comparisons by reading flags after
    

    // Bit shift instructions, NOTE: none of these uses D8 as ByteTarget value
    RLA,  // Rotate A left through carry
    RLCA, // Rotate A left
    RRA,  // Rotate A right through carry
    RRCA, // Rotate A right

    RLC(ByteTarget),  // Rotate register left
    RRC(ByteTarget),  // Rotate register right
    RL(ByteTarget),   // Rotate register left through carry
    RR(ByteTarget),   // Rotate register right through carry
    SLA(ByteTarget),  // Shift arithmetic register left
    SRA(ByteTarget),  // Shift arithmetic register right
    SWAP(ByteTarget), // Swap upper 4 bits and lower 4 bits
    SRL(ByteTarget),  // Shift logic register right
    BIT(u8, ByteTarget),  // Test bit n at given register and set zero flag if not set
    RES(u8, ByteTarget),  // Set bit n at given register to 0
    SET(u8, ByteTarget),  // Set bit n at given register to 1

    // Jump instructions
    JP(JumpTest),
    JR(JumpTest), // Relative jump

    // Memory instructions
    LD(LoadType),

    // Stack instructions
    PUSH(StackTarget),
    POP(StackTarget),

    // Function call
    CALL(JumpTest),
    RET(JumpTest),
    RETI,
    RST(RSTVec),

    // Other
    DAA,
    SCF, // Set carry flag
    CPL, // Complement accumulator
    CCF, // Complement carry flag
    DI,  // Disable interrupts
    EI,  // Enable interrupts
    NOP,
    HALT,
    STOP,
}

// Targets for operations, both 8-bit and 16-bit
// HLI means value at the address pointed to by HL registers, D8 means next byte
enum ArithmeticType { Byte(ByteTarget), Word(WordTarget), SP }

enum ByteTarget { A, B, C, D, E, H, L, D8, HLI }
enum WordTarget { BC, DE, HL, SP }


enum JumpTest {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
    AlwaysHL, // For instruction 0xE9 where it jumps to address in register HL
}

enum LoadByteTarget {
    A, B, C, D, E, H, L, HLI, 
}

enum LoadByteSource {
    A, B, C, D, E, H, L, D8, HLI, // D8 means next byte
}

enum LoadWordTarget {
    BC, DE, HL, SP, D16, HLI, // D16 means next word
}

enum LoadWordSource {
    BC, DE, HL, SP, D16, HLI, SPi8
}

enum LoadMemoryLocation {
    BC, DE, HLpostinc, HLpredec, 
}

enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget, LoadWordSource),
    AFromIndirect(LoadMemoryLocation), // Load memory location into A
    IndirectFromA(LoadMemoryLocation), // Load A into memory location
    AFromByteAddress(ByteAddress), // Address FF00+u8
    ByteAddressFromA(ByteAddress),
}

enum ByteAddress {
    C, D8
}

enum StackTarget {
    AF,
    BC,
    DE,
    HL,
}

enum RSTVec {
    h00,
    h10,
    h20,
    h30,
    h08,
    h18,
    h28,
    h38,
}

impl Instruction {
    // Converts a byte into an instruction
    // Full opcode table at https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
    fn from_byte(byte: u8, prefixed: bool) -> Option<(Instruction, usize, usize)> {
        if prefixed {
            Instruction::from_byte_prefixed(byte)
        } else {
            Instruction::from_byte_not_prefixed(byte)
        }
    }
    
    fn from_byte_not_prefixed(byte: u8) -> Option<(Instruction, usize, usize)> {
        match byte {
            // Load instructions 8-bit
            0x02 => Some((Instruction::LD(LoadType::IndirectFromA(LoadMemoryLocation::BC)), 1, 8)),
            0x12 => Some((Instruction::LD(LoadType::IndirectFromA(LoadMemoryLocation::DE)), 1, 8)),
            0x22 => Some((Instruction::LD(LoadType::IndirectFromA(LoadMemoryLocation::HLpostinc)), 1, 8)),
            0x32 => Some((Instruction::LD(LoadType::IndirectFromA(LoadMemoryLocation::HLpredec)), 1, 8)),

            0x06 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::D8)), 2, 8)),
            0x16 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::D8)), 2, 8)),
            0x26 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::D8)), 2, 8)),
            0x36 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::D8)), 2, 12)),

            0x0A => Some((Instruction::LD(LoadType::AFromIndirect(LoadMemoryLocation::BC)), 1, 8)),
            0x1A => Some((Instruction::LD(LoadType::AFromIndirect(LoadMemoryLocation::DE)), 1, 8)),
            0x2A => Some((Instruction::LD(LoadType::AFromIndirect(LoadMemoryLocation::HLpostinc)), 1, 8)),
            0x3A => Some((Instruction::LD(LoadType::AFromIndirect(LoadMemoryLocation::HLpredec)), 1, 8)),

            0x0E => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::D8)), 2, 8)),
            0x1E => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::D8)), 2, 8)),
            0x2E => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::D8)), 2, 8)),
            0x3E => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::D8)), 2, 8)),

            0x40 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::B)), 1, 4)),
            0x41 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::C)), 1, 4)),
            0x42 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::D)), 1, 4)),
            0x43 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::E)), 1, 4)),
            0x44 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::H)), 1, 4)),
            0x45 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::L)), 1, 4)),
            0x46 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::HLI)), 1, 8)),
            0x47 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::A)), 1, 4)),
            0x48 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::B)), 1, 4)),
            0x49 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::C)), 1, 4)),
            0x4A => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::D)), 1, 4)),
            0x4B => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::E)), 1, 4)),
            0x4C => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::H)), 1, 4)),
            0x4D => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::L)), 1, 4)),
            0x4E => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::HLI)), 1, 8)),
            0x4F => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::A)), 1, 4)),

            0x50 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::B)), 1, 4)),
            0x51 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::C)), 1, 4)),
            0x52 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::D)), 1, 4)),
            0x53 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::E)), 1, 4)),
            0x54 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::H)), 1, 4)),
            0x55 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::L)), 1, 4)),
            0x56 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::HLI)), 1, 8)),
            0x57 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::A)), 1, 4)),
            0x58 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::B)), 1, 4)),
            0x59 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::C)), 1, 4)),
            0x5A => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::D)), 1, 4)),
            0x5B => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::E)), 1, 4)),
            0x5C => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::H)), 1, 4)),
            0x5D => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::L)), 1, 4)),
            0x5E => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::HLI)), 1, 8)),
            0x5F => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::A)), 1, 4)),

            0x60 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::B)), 1, 4)),
            0x61 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::C)), 1, 4)),
            0x62 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::D)), 1, 4)),
            0x63 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::E)), 1, 4)),
            0x64 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::H)), 1, 4)),
            0x65 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::L)), 1, 4)),
            0x66 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::HLI)), 1, 8)),
            0x67 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::A)), 1, 4)),
            0x68 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::B)), 1, 4)),
            0x69 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::C)), 1, 4)),
            0x6A => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::D)), 1, 4)),
            0x6B => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::E)), 1, 4)),
            0x6C => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::H)), 1, 4)),
            0x6D => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::L)), 1, 4)),
            0x6E => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::HLI)), 1, 8)),
            0x6F => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::A)), 1, 4)),

            0x70 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::B)), 1, 8)),
            0x71 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::C)), 1, 8)),
            0x72 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::D)), 1, 8)),
            0x73 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::E)), 1, 8)),
            0x78 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::H)), 1, 8)),
            0x75 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::L)), 1, 8)),
            0x76 => Some((Instruction::HALT, 1, 4)),
            0x77 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::A)), 1, 8)),
            0x78 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::B)), 1, 4)),
            0x79 => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::C)), 1, 4)),
            0x7A => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::D)), 1, 4)),
            0x7B => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::E)), 1, 4)),
            0x7C => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::H)), 1, 4)),
            0x7D => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::L)), 1, 4)),
            0x7E => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::HLI)), 1, 8)),
            0x7F => Some((Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::A)), 1, 4)),

            // Load instructions 16-bit
            0x01 => Some((Instruction::LD(LoadType::Word(LoadWordTarget::BC, LoadWordSource::D16)), 3, 12)),
            0x11 => Some((Instruction::LD(LoadType::Word(LoadWordTarget::DE, LoadWordSource::D16)), 3, 12)),
            0x21 => Some((Instruction::LD(LoadType::Word(LoadWordTarget::HL, LoadWordSource::D16)), 3, 12)),
            0x31 => Some((Instruction::LD(LoadType::Word(LoadWordTarget::SP, LoadWordSource::D16)), 3, 12)),
            0x08 => Some((Instruction::LD(LoadType::Word(LoadWordTarget::D16, LoadWordSource::SP)), 3, 20)),
            0xF9 => Some((Instruction::LD(LoadType::Word(LoadWordTarget::SP, LoadWordSource::HL)), 1, 8)),

            0xEA => Some((Instruction::LD(LoadType::Word(LoadWordTarget::D16, LoadWordSource::HLI)), 3, 16)),
            0xFA => Some((Instruction::LD(LoadType::Word(LoadWordTarget::HLI, LoadWordSource::D16)), 3, 16)),

            0xE0 => Some((Instruction::LD(LoadType::ByteAddressFromA(ByteAddress::D8)), 2, 12)),
            0xF0 => Some((Instruction::LD(LoadType::AFromByteAddress(ByteAddress::D8)), 2, 12)),
            0xE2 => Some((Instruction::LD(LoadType::ByteAddressFromA(ByteAddress::C)), 1, 8)),
            0xF2 => Some((Instruction::LD(LoadType::AFromByteAddress(ByteAddress::C)), 1, 8)),

            0xF8 => Some((Instruction::LD(LoadType::Word(LoadWordTarget::HL, LoadWordSource::SPi8)), 2, 12)),

            // Increment and decrement instructions
            0x04 => Some((Instruction::INC(ArithmeticType::Byte(ByteTarget::B)), 1, 4)),
            0x14 => Some((Instruction::INC(ArithmeticType::Byte(ByteTarget::D)), 1, 4)),
            0x24 => Some((Instruction::INC(ArithmeticType::Byte(ByteTarget::H)), 1, 4)),
            0x34 => Some((Instruction::INC(ArithmeticType::Byte(ByteTarget::HLI)), 1, 12)),
            
            0x0C => Some((Instruction::INC(ArithmeticType::Byte(ByteTarget::C)), 1, 4)),
            0x1C => Some((Instruction::INC(ArithmeticType::Byte(ByteTarget::E)), 1, 4)),
            0x2C => Some((Instruction::INC(ArithmeticType::Byte(ByteTarget::L)), 1, 4)),
            0x3C => Some((Instruction::INC(ArithmeticType::Byte(ByteTarget::A)), 1, 4)),
            
            0x05 => Some((Instruction::DEC(ArithmeticType::Byte(ByteTarget::B)), 1, 4)),
            0x15 => Some((Instruction::DEC(ArithmeticType::Byte(ByteTarget::D)), 1, 4)),
            0x25 => Some((Instruction::DEC(ArithmeticType::Byte(ByteTarget::H)), 1, 4)),
            0x35 => Some((Instruction::DEC(ArithmeticType::Byte(ByteTarget::HLI)), 1, 12)),
            
            0x0D => Some((Instruction::DEC(ArithmeticType::Byte(ByteTarget::C)), 1, 4)),
            0x1D => Some((Instruction::DEC(ArithmeticType::Byte(ByteTarget::E)), 1, 4)),
            0x2D => Some((Instruction::DEC(ArithmeticType::Byte(ByteTarget::L)), 1, 4)),
            0x3D => Some((Instruction::DEC(ArithmeticType::Byte(ByteTarget::A)), 1, 4)),
            
            0x03 => Some((Instruction::INC(ArithmeticType::Word(WordTarget::BC)), 1, 8)),
            0x13 => Some((Instruction::INC(ArithmeticType::Word(WordTarget::DE)), 1, 8)),
            0x23 => Some((Instruction::INC(ArithmeticType::Word(WordTarget::HL)), 1, 8)),
            0x33 => Some((Instruction::INC(ArithmeticType::Word(WordTarget::SP)), 1, 8)),
            
            0x0B => Some((Instruction::DEC(ArithmeticType::Word(WordTarget::BC)), 1, 8)),
            0x1B => Some((Instruction::DEC(ArithmeticType::Word(WordTarget::DE)), 1, 8)),
            0x2B => Some((Instruction::DEC(ArithmeticType::Word(WordTarget::HL)), 1, 8)),
            0x3B => Some((Instruction::DEC(ArithmeticType::Word(WordTarget::SP)), 1, 8)),

            // Bit shift instructions
            0x07 => Some((Instruction::RLCA, 1, 4)),
            0x0F => Some((Instruction::RRCA, 1, 4)),
            0x17 => Some((Instruction::RLA, 1, 4)),
            0x1F => Some((Instruction::RRA, 1, 4)),

            // Aritmetic add instructions
            0x80 => Some((Instruction::ADD(ArithmeticType::Byte(ByteTarget::B)), 1, 4)),
            0x81 => Some((Instruction::ADD(ArithmeticType::Byte(ByteTarget::C)), 1, 4)),
            0x82 => Some((Instruction::ADD(ArithmeticType::Byte(ByteTarget::D)), 1, 4)),
            0x83 => Some((Instruction::ADD(ArithmeticType::Byte(ByteTarget::E)), 1, 4)),
            0x84 => Some((Instruction::ADD(ArithmeticType::Byte(ByteTarget::H)), 1, 4)),
            0x85 => Some((Instruction::ADD(ArithmeticType::Byte(ByteTarget::L)), 1, 4)),
            0x86 => Some((Instruction::ADD(ArithmeticType::Byte(ByteTarget::HLI)), 1, 8)),
            0x87 => Some((Instruction::ADD(ArithmeticType::Byte(ByteTarget::A)), 1, 4)),
            0xC6 => Some((Instruction::ADD(ArithmeticType::Byte(ByteTarget::D8)), 2, 8)),

            0x88 => Some((Instruction::ADC(ArithmeticType::Byte(ByteTarget::B)), 1, 4)),
            0x89 => Some((Instruction::ADC(ArithmeticType::Byte(ByteTarget::C)), 1, 4)),
            0x8A => Some((Instruction::ADC(ArithmeticType::Byte(ByteTarget::D)), 1, 4)),
            0x8B => Some((Instruction::ADC(ArithmeticType::Byte(ByteTarget::E)), 1, 4)),
            0x8C => Some((Instruction::ADC(ArithmeticType::Byte(ByteTarget::H)), 1, 4)),
            0x8D => Some((Instruction::ADC(ArithmeticType::Byte(ByteTarget::L)), 1, 4)),
            0x8E => Some((Instruction::ADC(ArithmeticType::Byte(ByteTarget::HLI)), 1, 8)),
            0x8F => Some((Instruction::ADC(ArithmeticType::Byte(ByteTarget::A)), 1, 4)),
            0xCE => Some((Instruction::ADC(ArithmeticType::Byte(ByteTarget::D8)), 2, 8)),

            0x09 => Some((Instruction::ADD(ArithmeticType::Word(WordTarget::BC)), 1, 8)),
            0x19 => Some((Instruction::ADD(ArithmeticType::Word(WordTarget::DE)), 1, 8)),
            0x29 => Some((Instruction::ADD(ArithmeticType::Word(WordTarget::HL)), 1, 8)),
            0x39 => Some((Instruction::ADD(ArithmeticType::Word(WordTarget::SP)), 1, 8)),
            0xE8 => Some((Instruction::ADD(ArithmeticType::SP), 2, 16)),

            // Arithmetic subtract
            0x90 => Some((Instruction::SUB(ArithmeticType::Byte(ByteTarget::B)), 1, 4)),
            0x91 => Some((Instruction::SUB(ArithmeticType::Byte(ByteTarget::C)), 1, 4)),
            0x92 => Some((Instruction::SUB(ArithmeticType::Byte(ByteTarget::D)), 1, 4)),
            0x93 => Some((Instruction::SUB(ArithmeticType::Byte(ByteTarget::E)), 1, 4)),
            0x94 => Some((Instruction::SUB(ArithmeticType::Byte(ByteTarget::H)), 1, 4)),
            0x95 => Some((Instruction::SUB(ArithmeticType::Byte(ByteTarget::L)), 1, 4)),
            0x96 => Some((Instruction::SUB(ArithmeticType::Byte(ByteTarget::HLI)), 1, 8)),
            0x97 => Some((Instruction::SUB(ArithmeticType::Byte(ByteTarget::A)), 1, 4)),
            0xD6 => Some((Instruction::SUB(ArithmeticType::Byte(ByteTarget::D8)), 2, 8)),

            0x98 => Some((Instruction::SBC(ArithmeticType::Byte(ByteTarget::B)), 1, 4)),
            0x99 => Some((Instruction::SBC(ArithmeticType::Byte(ByteTarget::C)), 1, 4)),
            0x9A => Some((Instruction::SBC(ArithmeticType::Byte(ByteTarget::D)), 1, 4)),
            0x9B => Some((Instruction::SBC(ArithmeticType::Byte(ByteTarget::E)), 1, 4)),
            0x9C => Some((Instruction::SBC(ArithmeticType::Byte(ByteTarget::H)), 1, 4)),
            0x9D => Some((Instruction::SBC(ArithmeticType::Byte(ByteTarget::L)), 1, 4)),
            0x9E => Some((Instruction::SBC(ArithmeticType::Byte(ByteTarget::HLI)), 1, 8)),
            0x9F => Some((Instruction::SBC(ArithmeticType::Byte(ByteTarget::A)), 1, 4)),
            0xDE => Some((Instruction::SBC(ArithmeticType::Byte(ByteTarget::D8)), 2, 8)),

            // Logic AND
            0xA0 => Some((Instruction::AND(ByteTarget::B), 1, 4)),
            0xA1 => Some((Instruction::AND(ByteTarget::C), 1, 4)),
            0xA2 => Some((Instruction::AND(ByteTarget::D), 1, 4)),
            0xA3 => Some((Instruction::AND(ByteTarget::E), 1, 4)),
            0xA4 => Some((Instruction::AND(ByteTarget::H), 1, 4)),
            0xA5 => Some((Instruction::AND(ByteTarget::L), 1, 4)),
            0xA6 => Some((Instruction::AND(ByteTarget::HLI), 1, 8)),
            0xA7 => Some((Instruction::AND(ByteTarget::A), 1, 4)),
            0xE6 => Some((Instruction::AND(ByteTarget::D8), 2, 8)),

            // Logic XOR
            0xA8 => Some((Instruction::XOR(ByteTarget::B), 1, 4)),
            0xA9 => Some((Instruction::XOR(ByteTarget::C), 1, 4)),
            0xAA => Some((Instruction::XOR(ByteTarget::D), 1, 4)),
            0xAB => Some((Instruction::XOR(ByteTarget::E), 1, 4)),
            0xAC => Some((Instruction::XOR(ByteTarget::H), 1, 4)),
            0xAD => Some((Instruction::XOR(ByteTarget::L), 1, 4)),
            0xAE => Some((Instruction::XOR(ByteTarget::HLI), 1, 8)),
            0xAF => Some((Instruction::XOR(ByteTarget::A), 1, 4)),
            0xEE => Some((Instruction::XOR(ByteTarget::D8), 2, 8)),

            // Logic OR
            0xB0 => Some((Instruction::OR(ByteTarget::B), 1, 4)),
            0xB1 => Some((Instruction::OR(ByteTarget::C), 1, 4)),
            0xB2 => Some((Instruction::OR(ByteTarget::D), 1, 4)),
            0xB3 => Some((Instruction::OR(ByteTarget::E), 1, 4)),
            0xB4 => Some((Instruction::OR(ByteTarget::H), 1, 4)),
            0xB5 => Some((Instruction::OR(ByteTarget::L), 1, 4)),
            0xB6 => Some((Instruction::OR(ByteTarget::HLI), 1, 8)),
            0xB7 => Some((Instruction::OR(ByteTarget::A), 1, 4)),
            0xF6 => Some((Instruction::OR(ByteTarget::D8), 2, 8)),

            // Logic CP
            0xB8 => Some((Instruction::CP(ByteTarget::B), 1, 4)),
            0xB9 => Some((Instruction::CP(ByteTarget::C), 1, 4)),
            0xBA => Some((Instruction::CP(ByteTarget::D), 1, 4)),
            0xBB => Some((Instruction::CP(ByteTarget::E), 1, 4)),
            0xBC => Some((Instruction::CP(ByteTarget::H), 1, 4)),
            0xBD => Some((Instruction::CP(ByteTarget::L), 1, 4)),
            0xBE => Some((Instruction::CP(ByteTarget::HLI), 1, 8)),
            0xBF => Some((Instruction::CP(ByteTarget::A), 1, 4)),
            0xFE => Some((Instruction::CP(ByteTarget::D8), 2, 8)),

            // Jump instructions
            0x18 => Some((Instruction::JR(JumpTest::Always), 2, 12)),
            0x20 => Some((Instruction::JR(JumpTest::NotZero), 2, 8)),   // TODO number of cycles varies from 8-12
            0x30 => Some((Instruction::JR(JumpTest::NotCarry), 2, 8)),  // TODO number of cycles varies from 8-12
            0x28 => Some((Instruction::JR(JumpTest::Zero), 2, 8)),      // TODO number of cycles varies from 8-12
            0x38 => Some((Instruction::JR(JumpTest::Carry), 2, 8)),     // TODO number of cycles varies from 8-12

            0xC2 => Some((Instruction::JP(JumpTest::NotZero), 3, 12)),  // TODO number of cycles varies from 12-16
            0xD2 => Some((Instruction::JP(JumpTest::NotCarry), 3, 12)), // TODO number of cycles varies from 12-16
            0xC3 => Some((Instruction::JP(JumpTest::Always), 3, 16)),
            0xE9 => Some((Instruction::JP(JumpTest::AlwaysHL), 1, 4)),
            0xCA => Some((Instruction::JP(JumpTest::Zero), 3, 12)),     // TODO number of cycles varies from 12-16
            0xDA => Some((Instruction::JP(JumpTest::Carry), 3, 12)),    // TODO number of cycles varies from 12-16

            0xC4 => Some((Instruction::CALL(JumpTest::NotZero), 3, 12)),    // TODO number of cycles varies from 12-24
            0xD4 => Some((Instruction::CALL(JumpTest::NotCarry), 3, 12)),   // TODO number of cycles varies from 12-24
            0xCC => Some((Instruction::CALL(JumpTest::Zero), 3, 12)),       // TODO number of cycles varies from 12-24
            0xDC => Some((Instruction::CALL(JumpTest::Carry), 3, 12)),      // TODO number of cycles varies from 12-24
            0xCD => Some((Instruction::CALL(JumpTest::Always), 3, 24)),

            0xC0 => Some((Instruction::RET(JumpTest::NotZero), 1, 8)),  // TODO number of cycles varies from 8-20
            0xD0 => Some((Instruction::RET(JumpTest::NotCarry), 1, 8)), // TODO number of cycles varies from 8-20
            0xC8 => Some((Instruction::RET(JumpTest::Zero), 1, 8)),     // TODO number of cycles varies from 8-20
            0xD8 => Some((Instruction::RET(JumpTest::Carry), 1, 8)),    // TODO number of cycles varies from 8-20
            0xC9 => Some((Instruction::RET(JumpTest::Always), 1, 16)),

            0xD9 => Some((Instruction::RETI, 1, 16)),

            0xC7 => Some((Instruction::RST(RSTVec::h00), 1, 16)),
            0xD7 => Some((Instruction::RST(RSTVec::h10), 1, 16)),
            0xE7 => Some((Instruction::RST(RSTVec::h20), 1, 16)),
            0xF7 => Some((Instruction::RST(RSTVec::h30), 1, 16)),
            0xFF => Some((Instruction::RST(RSTVec::h08), 1, 16)),
            0xDF => Some((Instruction::RST(RSTVec::h18), 1, 16)),
            0xEF => Some((Instruction::RST(RSTVec::h28), 1, 16)),
            0xFF => Some((Instruction::RST(RSTVec::h38), 1, 16)),

            // Stack instructions
            0xC1 => Some((Instruction::POP(StackTarget::BC), 1, 12)),
            0xD1 => Some((Instruction::POP(StackTarget::DE), 1, 12)),
            0xE1 => Some((Instruction::POP(StackTarget::HL), 1, 12)),
            0xF1 => Some((Instruction::POP(StackTarget::AF), 1, 12)),

            0xC5 => Some((Instruction::PUSH(StackTarget::BC), 1, 16)),
            0xD5 => Some((Instruction::PUSH(StackTarget::DE), 1, 16)),
            0xE5 => Some((Instruction::PUSH(StackTarget::HL), 1, 16)),
            0xF5 => Some((Instruction::PUSH(StackTarget::AF), 1, 16)),

            // Misc 
            0x00 => Some((Instruction::NOP, 1, 4)),
            0x10 => Some((Instruction::STOP, 2, 4)),
            0x27 => Some((Instruction::DAA, 1, 4)),
            0x37 => Some((Instruction::SCF, 1, 4)),
            0x2F => Some((Instruction::CPL, 1, 4)),
            0x3F => Some((Instruction::CCF, 1, 4)),
            0xF3 => Some((Instruction::DI, 1, 4)),
            0xFB => Some((Instruction::EI, 1, 4)),
            0xCB => None, // Prefixed instruction should not land in this method
            _ => None,
        }
    }

    // Prefixed instructions are instructions starting with 0xCB
    fn from_byte_prefixed(byte: u8) -> Option<(Instruction, usize, usize)> {
        match byte {
            0x00 => Some((Instruction::RLC(ByteTarget::B), 2, 8)),
            0x01 => Some((Instruction::RLC(ByteTarget::C), 2, 8)),
            0x02 => Some((Instruction::RLC(ByteTarget::D), 2, 8)),
            0x03 => Some((Instruction::RLC(ByteTarget::E), 2, 8)),
            0x04 => Some((Instruction::RLC(ByteTarget::H), 2, 8)),
            0x05 => Some((Instruction::RLC(ByteTarget::L), 2, 8)),
            0x06 => Some((Instruction::RLC(ByteTarget::HLI), 2, 16)),
            0x07 => Some((Instruction::RLC(ByteTarget::A), 2, 8)),

            0x08 => Some((Instruction::RRC(ByteTarget::B), 2, 8)),
            0x09 => Some((Instruction::RRC(ByteTarget::C), 2, 8)),
            0x0A => Some((Instruction::RRC(ByteTarget::D), 2, 8)),
            0x0B => Some((Instruction::RRC(ByteTarget::E), 2, 8)),
            0x0C => Some((Instruction::RRC(ByteTarget::H), 2, 8)),
            0x0D => Some((Instruction::RRC(ByteTarget::L), 2, 8)),
            0x0E => Some((Instruction::RRC(ByteTarget::HLI), 2, 16)),
            0x0F => Some((Instruction::RRC(ByteTarget::A), 2, 8)),

            0x10 => Some((Instruction::RL(ByteTarget::B), 2, 8)),
            0x11 => Some((Instruction::RL(ByteTarget::C), 2, 8)),
            0x12 => Some((Instruction::RL(ByteTarget::D), 2, 8)),
            0x13 => Some((Instruction::RL(ByteTarget::E), 2, 8)),
            0x14 => Some((Instruction::RL(ByteTarget::H), 2, 8)),
            0x15 => Some((Instruction::RL(ByteTarget::L), 2, 8)),
            0x16 => Some((Instruction::RL(ByteTarget::HLI), 2, 16)),
            0x17 => Some((Instruction::RL(ByteTarget::A), 2, 8)),

            0x18 => Some((Instruction::RR(ByteTarget::B), 2, 8)),
            0x19 => Some((Instruction::RR(ByteTarget::C), 2, 8)),
            0x1A => Some((Instruction::RR(ByteTarget::D), 2, 8)),
            0x1B => Some((Instruction::RR(ByteTarget::E), 2, 8)),
            0x1C => Some((Instruction::RR(ByteTarget::H), 2, 8)),
            0x1D => Some((Instruction::RR(ByteTarget::L), 2, 8)),
            0x1E => Some((Instruction::RR(ByteTarget::HLI), 2, 16)),
            0x1F => Some((Instruction::RR(ByteTarget::A), 2, 8)),

            0x20 => Some((Instruction::SLA(ByteTarget::B), 2, 8)),
            0x21 => Some((Instruction::SLA(ByteTarget::C), 2, 8)),
            0x22 => Some((Instruction::SLA(ByteTarget::D), 2, 8)),
            0x23 => Some((Instruction::SLA(ByteTarget::E), 2, 8)),
            0x24 => Some((Instruction::SLA(ByteTarget::H), 2, 8)),
            0x25 => Some((Instruction::SLA(ByteTarget::L), 2, 8)),
            0x26 => Some((Instruction::SLA(ByteTarget::HLI), 2, 16)),
            0x27 => Some((Instruction::SLA(ByteTarget::A), 2, 8)),

            0x28 => Some((Instruction::SRA(ByteTarget::B), 2, 8)),
            0x29 => Some((Instruction::SRA(ByteTarget::C), 2, 8)),
            0x2A => Some((Instruction::SRA(ByteTarget::D), 2, 8)),
            0x2B => Some((Instruction::SRA(ByteTarget::E), 2, 8)),
            0x2C => Some((Instruction::SRA(ByteTarget::H), 2, 8)),
            0x2D => Some((Instruction::SRA(ByteTarget::L), 2, 8)),
            0x2E => Some((Instruction::SRA(ByteTarget::HLI), 2, 16)),
            0x2F => Some((Instruction::SRA(ByteTarget::A), 2, 8)),
            
            0x30 => Some((Instruction::SWAP(ByteTarget::B), 2, 8)),
            0x31 => Some((Instruction::SWAP(ByteTarget::C), 2, 8)),
            0x32 => Some((Instruction::SWAP(ByteTarget::D), 2, 8)),
            0x33 => Some((Instruction::SWAP(ByteTarget::E), 2, 8)),
            0x34 => Some((Instruction::SWAP(ByteTarget::H), 2, 8)),
            0x35 => Some((Instruction::SWAP(ByteTarget::L), 2, 8)),
            0x36 => Some((Instruction::SWAP(ByteTarget::HLI), 2, 16)),
            0x37 => Some((Instruction::SWAP(ByteTarget::A), 2, 8)),

            0x38 => Some((Instruction::SRL(ByteTarget::B), 2, 8)),
            0x39 => Some((Instruction::SRL(ByteTarget::C), 2, 8)),
            0x3A => Some((Instruction::SRL(ByteTarget::D), 2, 8)),
            0x3B => Some((Instruction::SRL(ByteTarget::E), 2, 8)),
            0x3C => Some((Instruction::SRL(ByteTarget::H), 2, 8)),
            0x3D => Some((Instruction::SRL(ByteTarget::L), 2, 8)),
            0x3E => Some((Instruction::SRL(ByteTarget::HLI), 2, 16)),
            0x3F => Some((Instruction::SRL(ByteTarget::A), 2, 8)),

            // Bit instructions
            0x40 => Some((Instruction::BIT(0, ByteTarget::B), 2, 8)),
            0x41 => Some((Instruction::BIT(0, ByteTarget::C), 2, 8)),
            0x42 => Some((Instruction::BIT(0, ByteTarget::D), 2, 8)),
            0x43 => Some((Instruction::BIT(0, ByteTarget::E), 2, 8)),
            0x44 => Some((Instruction::BIT(0, ByteTarget::H), 2, 8)),
            0x45 => Some((Instruction::BIT(0, ByteTarget::L), 2, 8)),
            0x46 => Some((Instruction::BIT(0, ByteTarget::HLI), 2, 12)),
            0x47 => Some((Instruction::BIT(0, ByteTarget::A), 2, 8)),
            0x48 => Some((Instruction::BIT(1, ByteTarget::B), 2, 8)),
            0x49 => Some((Instruction::BIT(1, ByteTarget::C), 2, 8)),
            0x4A => Some((Instruction::BIT(1, ByteTarget::D), 2, 8)),
            0x4B => Some((Instruction::BIT(1, ByteTarget::E), 2, 8)),
            0x4C => Some((Instruction::BIT(1, ByteTarget::H), 2, 8)),
            0x4D => Some((Instruction::BIT(1, ByteTarget::L), 2, 8)),
            0x4E => Some((Instruction::BIT(1, ByteTarget::HLI), 2, 12)),
            0x4F => Some((Instruction::BIT(1, ByteTarget::A), 2, 8)),
            0x50 => Some((Instruction::BIT(2, ByteTarget::B), 2, 8)),
            0x51 => Some((Instruction::BIT(2, ByteTarget::C), 2, 8)),
            0x52 => Some((Instruction::BIT(2, ByteTarget::D), 2, 8)),
            0x53 => Some((Instruction::BIT(2, ByteTarget::E), 2, 8)),
            0x54 => Some((Instruction::BIT(2, ByteTarget::H), 2, 8)),
            0x55 => Some((Instruction::BIT(2, ByteTarget::L), 2, 8)),
            0x56 => Some((Instruction::BIT(2, ByteTarget::HLI), 2, 12)),
            0x57 => Some((Instruction::BIT(2, ByteTarget::A), 2, 8)),
            0x58 => Some((Instruction::BIT(3, ByteTarget::B), 2, 8)),
            0x59 => Some((Instruction::BIT(3, ByteTarget::C), 2, 8)),
            0x5A => Some((Instruction::BIT(3, ByteTarget::D), 2, 8)),
            0x5B => Some((Instruction::BIT(3, ByteTarget::E), 2, 8)),
            0x5C => Some((Instruction::BIT(3, ByteTarget::H), 2, 8)),
            0x5D => Some((Instruction::BIT(3, ByteTarget::L), 2, 8)),
            0x5E => Some((Instruction::BIT(3, ByteTarget::HLI), 2, 12)),
            0x5F => Some((Instruction::BIT(3, ByteTarget::A), 2, 8)),
            0x60 => Some((Instruction::BIT(4, ByteTarget::B), 2, 8)),
            0x61 => Some((Instruction::BIT(4, ByteTarget::C), 2, 8)),
            0x62 => Some((Instruction::BIT(4, ByteTarget::D), 2, 8)),
            0x63 => Some((Instruction::BIT(4, ByteTarget::E), 2, 8)),
            0x64 => Some((Instruction::BIT(4, ByteTarget::H), 2, 8)),
            0x65 => Some((Instruction::BIT(4, ByteTarget::L), 2, 8)),
            0x66 => Some((Instruction::BIT(4, ByteTarget::HLI), 2, 12)),
            0x67 => Some((Instruction::BIT(4, ByteTarget::A), 2, 8)),
            0x68 => Some((Instruction::BIT(5, ByteTarget::B), 2, 8)),
            0x69 => Some((Instruction::BIT(5, ByteTarget::C), 2, 8)),
            0x6A => Some((Instruction::BIT(5, ByteTarget::D), 2, 8)),
            0x6B => Some((Instruction::BIT(5, ByteTarget::E), 2, 8)),
            0x6C => Some((Instruction::BIT(5, ByteTarget::H), 2, 8)),
            0x6D => Some((Instruction::BIT(5, ByteTarget::L), 2, 8)),
            0x6E => Some((Instruction::BIT(5, ByteTarget::HLI), 2, 12)),
            0x6F => Some((Instruction::BIT(5, ByteTarget::A), 2, 8)),
            0x70 => Some((Instruction::BIT(6, ByteTarget::B), 2, 8)),
            0x71 => Some((Instruction::BIT(6, ByteTarget::C), 2, 8)),
            0x72 => Some((Instruction::BIT(6, ByteTarget::D), 2, 8)),
            0x73 => Some((Instruction::BIT(6, ByteTarget::E), 2, 8)),
            0x74 => Some((Instruction::BIT(6, ByteTarget::H), 2, 8)),
            0x75 => Some((Instruction::BIT(6, ByteTarget::L), 2, 8)),
            0x76 => Some((Instruction::BIT(6, ByteTarget::HLI), 2, 12)),
            0x77 => Some((Instruction::BIT(6, ByteTarget::A), 2, 8)),
            0x78 => Some((Instruction::BIT(7, ByteTarget::B), 2, 8)),
            0x79 => Some((Instruction::BIT(7, ByteTarget::C), 2, 8)),
            0x7A => Some((Instruction::BIT(7, ByteTarget::D), 2, 8)),
            0x7B => Some((Instruction::BIT(7, ByteTarget::E), 2, 8)),
            0x7C => Some((Instruction::BIT(7, ByteTarget::H), 2, 8)),
            0x7D => Some((Instruction::BIT(7, ByteTarget::L), 2, 8)),
            0x7E => Some((Instruction::BIT(7, ByteTarget::HLI), 2, 12)),
            0x7F => Some((Instruction::BIT(7, ByteTarget::A), 2, 8)),

            // RES instructions
            0x80 => Some((Instruction::RES(0, ByteTarget::B), 2, 8)),
            0x81 => Some((Instruction::RES(0, ByteTarget::C), 2, 8)),
            0x82 => Some((Instruction::RES(0, ByteTarget::D), 2, 8)),
            0x83 => Some((Instruction::RES(0, ByteTarget::E), 2, 8)),
            0x84 => Some((Instruction::RES(0, ByteTarget::H), 2, 8)),
            0x85 => Some((Instruction::RES(0, ByteTarget::L), 2, 8)),
            0x86 => Some((Instruction::RES(0, ByteTarget::HLI), 2, 12)),
            0x87 => Some((Instruction::RES(0, ByteTarget::A), 2, 8)),
            0x88 => Some((Instruction::RES(1, ByteTarget::B), 2, 8)),
            0x89 => Some((Instruction::RES(1, ByteTarget::C), 2, 8)),
            0x8A => Some((Instruction::RES(1, ByteTarget::D), 2, 8)),
            0x8B => Some((Instruction::RES(1, ByteTarget::E), 2, 8)),
            0x8C => Some((Instruction::RES(1, ByteTarget::H), 2, 8)),
            0x8D => Some((Instruction::RES(1, ByteTarget::L), 2, 8)),
            0x8E => Some((Instruction::RES(1, ByteTarget::HLI), 2, 12)),
            0x8F => Some((Instruction::RES(1, ByteTarget::A), 2, 8)),
            0x90 => Some((Instruction::RES(2, ByteTarget::B), 2, 8)),
            0x91 => Some((Instruction::RES(2, ByteTarget::C), 2, 8)),
            0x92 => Some((Instruction::RES(2, ByteTarget::D), 2, 8)),
            0x93 => Some((Instruction::RES(2, ByteTarget::E), 2, 8)),
            0x94 => Some((Instruction::RES(2, ByteTarget::H), 2, 8)),
            0x95 => Some((Instruction::RES(2, ByteTarget::L), 2, 8)),
            0x96 => Some((Instruction::RES(2, ByteTarget::HLI), 2, 12)),
            0x97 => Some((Instruction::RES(2, ByteTarget::A), 2, 8)),
            0x98 => Some((Instruction::RES(3, ByteTarget::B), 2, 8)),
            0x99 => Some((Instruction::RES(3, ByteTarget::C), 2, 8)),
            0x9A => Some((Instruction::RES(3, ByteTarget::D), 2, 8)),
            0x9B => Some((Instruction::RES(3, ByteTarget::E), 2, 8)),
            0x9C => Some((Instruction::RES(3, ByteTarget::H), 2, 8)),
            0x9D => Some((Instruction::RES(3, ByteTarget::L), 2, 8)),
            0x9E => Some((Instruction::RES(3, ByteTarget::HLI), 2, 12)),
            0x9F => Some((Instruction::RES(3, ByteTarget::A), 2, 8)),
            0xA0 => Some((Instruction::RES(4, ByteTarget::B), 2, 8)),
            0xA1 => Some((Instruction::RES(4, ByteTarget::C), 2, 8)),
            0xA2 => Some((Instruction::RES(4, ByteTarget::D), 2, 8)),
            0xA3 => Some((Instruction::RES(4, ByteTarget::E), 2, 8)),
            0xA4 => Some((Instruction::RES(4, ByteTarget::H), 2, 8)),
            0xA5 => Some((Instruction::RES(4, ByteTarget::L), 2, 8)),
            0xA6 => Some((Instruction::RES(4, ByteTarget::HLI), 2, 12)),
            0xA7 => Some((Instruction::RES(4, ByteTarget::A), 2, 8)),
            0xA8 => Some((Instruction::RES(5, ByteTarget::B), 2, 8)),
            0xA9 => Some((Instruction::RES(5, ByteTarget::C), 2, 8)),
            0xAA => Some((Instruction::RES(5, ByteTarget::D), 2, 8)),
            0xAB => Some((Instruction::RES(5, ByteTarget::E), 2, 8)),
            0xAC => Some((Instruction::RES(5, ByteTarget::H), 2, 8)),
            0xAD => Some((Instruction::RES(5, ByteTarget::L), 2, 8)),
            0xAE => Some((Instruction::RES(5, ByteTarget::HLI), 2, 12)),
            0xAF => Some((Instruction::RES(5, ByteTarget::A), 2, 8)),
            0xB0 => Some((Instruction::RES(6, ByteTarget::B), 2, 8)),
            0xB1 => Some((Instruction::RES(6, ByteTarget::C), 2, 8)),
            0xB2 => Some((Instruction::RES(6, ByteTarget::D), 2, 8)),
            0xB3 => Some((Instruction::RES(6, ByteTarget::E), 2, 8)),
            0xB4 => Some((Instruction::RES(6, ByteTarget::H), 2, 8)),
            0xB5 => Some((Instruction::RES(6, ByteTarget::L), 2, 8)),
            0xB6 => Some((Instruction::RES(6, ByteTarget::HLI), 2, 12)),
            0xB7 => Some((Instruction::RES(6, ByteTarget::A), 2, 8)),
            0xB8 => Some((Instruction::RES(7, ByteTarget::B), 2, 8)),
            0xB9 => Some((Instruction::RES(7, ByteTarget::C), 2, 8)),
            0xBA => Some((Instruction::RES(7, ByteTarget::D), 2, 8)),
            0xBB => Some((Instruction::RES(7, ByteTarget::E), 2, 8)),
            0xBC => Some((Instruction::RES(7, ByteTarget::H), 2, 8)),
            0xBD => Some((Instruction::RES(7, ByteTarget::L), 2, 8)),
            0xBE => Some((Instruction::RES(7, ByteTarget::HLI), 2, 12)),
            0xBF => Some((Instruction::RES(7, ByteTarget::A), 2, 8)),

            // SET instructions
            0xC0 => Some((Instruction::SET(0, ByteTarget::B), 2, 8)),
            0xC1 => Some((Instruction::SET(0, ByteTarget::C), 2, 8)),
            0xC2 => Some((Instruction::SET(0, ByteTarget::D), 2, 8)),
            0xC3 => Some((Instruction::SET(0, ByteTarget::E), 2, 8)),
            0xC4 => Some((Instruction::SET(0, ByteTarget::H), 2, 8)),
            0xC5 => Some((Instruction::SET(0, ByteTarget::L), 2, 8)),
            0xC6 => Some((Instruction::SET(0, ByteTarget::HLI), 2, 12)),
            0xC7 => Some((Instruction::SET(0, ByteTarget::A), 2, 8)),
            0xC8 => Some((Instruction::SET(1, ByteTarget::B), 2, 8)),
            0xC9 => Some((Instruction::SET(1, ByteTarget::C), 2, 8)),
            0xCA => Some((Instruction::SET(1, ByteTarget::D), 2, 8)),
            0xCB => Some((Instruction::SET(1, ByteTarget::E), 2, 8)),
            0xCC => Some((Instruction::SET(1, ByteTarget::H), 2, 8)),
            0xCD => Some((Instruction::SET(1, ByteTarget::L), 2, 8)),
            0xCE => Some((Instruction::SET(1, ByteTarget::HLI), 2, 12)),
            0xCF => Some((Instruction::SET(1, ByteTarget::A), 2, 8)),
            0xD0 => Some((Instruction::SET(2, ByteTarget::B), 2, 8)),
            0xD1 => Some((Instruction::SET(2, ByteTarget::C), 2, 8)),
            0xD2 => Some((Instruction::SET(2, ByteTarget::D), 2, 8)),
            0xD3 => Some((Instruction::SET(2, ByteTarget::E), 2, 8)),
            0xD4 => Some((Instruction::SET(2, ByteTarget::H), 2, 8)),
            0xD5 => Some((Instruction::SET(2, ByteTarget::L), 2, 8)),
            0xD6 => Some((Instruction::SET(2, ByteTarget::HLI), 2, 12)),
            0xD7 => Some((Instruction::SET(2, ByteTarget::A), 2, 8)),
            0xD8 => Some((Instruction::SET(3, ByteTarget::B), 2, 8)),
            0xD9 => Some((Instruction::SET(3, ByteTarget::C), 2, 8)),
            0xDA => Some((Instruction::SET(3, ByteTarget::D), 2, 8)),
            0xDB => Some((Instruction::SET(3, ByteTarget::E), 2, 8)),
            0xDC => Some((Instruction::SET(3, ByteTarget::H), 2, 8)),
            0xDD => Some((Instruction::SET(3, ByteTarget::L), 2, 8)),
            0xDE => Some((Instruction::SET(3, ByteTarget::HLI), 2, 12)),
            0xDF => Some((Instruction::SET(3, ByteTarget::A), 2, 8)),
            0xE0 => Some((Instruction::SET(4, ByteTarget::B), 2, 8)),
            0xE1 => Some((Instruction::SET(4, ByteTarget::C), 2, 8)),
            0xE2 => Some((Instruction::SET(4, ByteTarget::D), 2, 8)),
            0xE3 => Some((Instruction::SET(4, ByteTarget::E), 2, 8)),
            0xE4 => Some((Instruction::SET(4, ByteTarget::H), 2, 8)),
            0xE5 => Some((Instruction::SET(4, ByteTarget::L), 2, 8)),
            0xE6 => Some((Instruction::SET(4, ByteTarget::HLI), 2, 12)),
            0xE7 => Some((Instruction::SET(4, ByteTarget::A), 2, 8)),
            0xE8 => Some((Instruction::SET(5, ByteTarget::B), 2, 8)),
            0xE9 => Some((Instruction::SET(5, ByteTarget::C), 2, 8)),
            0xEA => Some((Instruction::SET(5, ByteTarget::D), 2, 8)),
            0xEB => Some((Instruction::SET(5, ByteTarget::E), 2, 8)),
            0xEC => Some((Instruction::SET(5, ByteTarget::H), 2, 8)),
            0xED => Some((Instruction::SET(5, ByteTarget::L), 2, 8)),
            0xEE => Some((Instruction::SET(5, ByteTarget::HLI), 2, 12)),
            0xEF => Some((Instruction::SET(5, ByteTarget::A), 2, 8)),
            0xF0 => Some((Instruction::SET(6, ByteTarget::B), 2, 8)),
            0xF1 => Some((Instruction::SET(6, ByteTarget::C), 2, 8)),
            0xF2 => Some((Instruction::SET(6, ByteTarget::D), 2, 8)),
            0xF3 => Some((Instruction::SET(6, ByteTarget::E), 2, 8)),
            0xF4 => Some((Instruction::SET(6, ByteTarget::H), 2, 8)),
            0xF5 => Some((Instruction::SET(6, ByteTarget::L), 2, 8)),
            0xF6 => Some((Instruction::SET(6, ByteTarget::HLI), 2, 12)),
            0xF7 => Some((Instruction::SET(6, ByteTarget::A), 2, 8)),
            0xF8 => Some((Instruction::SET(7, ByteTarget::B), 2, 8)),
            0xF9 => Some((Instruction::SET(7, ByteTarget::C), 2, 8)),
            0xFA => Some((Instruction::SET(7, ByteTarget::D), 2, 8)),
            0xFB => Some((Instruction::SET(7, ByteTarget::E), 2, 8)),
            0xFC => Some((Instruction::SET(7, ByteTarget::H), 2, 8)),
            0xFD => Some((Instruction::SET(7, ByteTarget::L), 2, 8)),
            0xFE => Some((Instruction::SET(7, ByteTarget::HLI), 2, 12)),
            0xFF => Some((Instruction::SET(7, ByteTarget::A), 2, 8)),

            _ => None // TODO add all prefix instructions
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
        let mut mem = MemoryBus {
            memory: [0; 0xFFFF],
            gpu: GPU::new(),
        };

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
        let cycles = if let Some((instruction, _, cycles)) = Instruction::from_byte(instruction_byte, prefixed) {
            self.execute(instruction);
            cycles
        } else {
            let description = format!("0x{}{:x}", if prefixed { "cb" } else { "" }, instruction_byte);
            panic!("Unknown instruction found for: {}", description);
        };

        cycles
    }

    // Returns a slice to the pixel buffer 
    fn pixel_buffer(&self) -> &[u32] {
        let LCDC = self.bus.read_byte(0xFF40);

        // If Bit 7 is not set, LCD is disabled.
        if LCDC & 0x80 == 0x00 {
            return &[0x00FFFFFF as u32; WIDTH * HEIGHT] // Return all white
        }

        &[0; WIDTH * HEIGHT]
    }

    /*
        Executes a given instruction on the CPU.
        Every instruction implementation is self responsible for returning the new pc location.
        This is done because some instructions like JMP will change the location arbitrarily.
        NOTE: pc increments should use wrapping_add()
    */
    fn execute(&mut self, instruction: Instruction) {
        if self.is_halted {
            return
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
            Instruction::HALT => { self.is_halted = true },
            _ => {panic!("Instruction not found")} // TODO add more instructions
        }
    }

    /*
        ADD instruction.
        Reads the current value from target register. Adds the value to register A overflowing if necessary.
        Updates the flags register and writer the new value to register A.
    */
    fn add(&mut self, target: ArithmeticType) {
        match target {
            ArithmeticType::Byte(byte_target) => {
                let value: u8;
                match byte_target {
                    ByteTarget::A => { value = self.registers.a; },
                    ByteTarget::B => { value = self.registers.b; },
                    ByteTarget::C => { value = self.registers.c; },
                    ByteTarget::D => { value = self.registers.d; },
                    ByteTarget::E => { value = self.registers.e; },
                    ByteTarget::H => { value = self.registers.h; },
                    ByteTarget::L => { value = self.registers.l; },
                    ByteTarget::D8 => { value = self.read_next_byte() },
                    ByteTarget::HLI => { value = self.bus.read_byte(self.registers.get_hl()) },
                }
                let (new_value, did_overflow) = self.registers.a.overflowing_add(value);

                // Check if half carry needs to be set by adding both lower nibbles together and see if the result is greater than 1111 (0xF)
                self.registers.f.zero = new_value == 0;
                self.registers.f.carry = did_overflow;
                self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;
                
                self.registers.a = new_value;

            },
            ArithmeticType::Word(word_target) => {
                let value: u16;
                match word_target {
                    WordTarget::BC => { value = self.registers.get_bc() }, 
                    WordTarget::DE => { value = self.registers.get_de() }, 
                    WordTarget::HL => { value = self.registers.get_hl() }, 
                    WordTarget::SP => { value = self.registers.get_af() },
                }
                let (new_value, did_overflow) = self.registers.get_hl().overflowing_add(value);
                
                // Half-carry is set if overflow from bit 11
                self.registers.f.carry = did_overflow;
                self.registers.f.half_carry = (self.registers.get_hl() & 0x0FFF) + (value & 0x0FFF) > 0x0FFF;

                self.registers.set_hl(new_value)
            },
            ArithmeticType::SP => {
                let value = self.read_next_byte() as i8 as u16;
                let (new_value, _) = self.sp.overflowing_add(value);

                // Instruction sets carry bit if overflow occured on bit 7, thus did_overflow can't be used here
                self.registers.f.carry = (self.sp & 0x00FF) + (value & 0x00FF) > 0x00FF;
                self.registers.f.half_carry = (self.sp & 0x000F) + (value & 0x000F) > 0x000F;

                self.sp = new_value;
            },
        }

        // Last flag common for all cases
        self.registers.f.subtract = false;
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
            _ => unimplemented!(),
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
            },
            _ => unimplemented!(),
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
    fn call(&mut self, test: JumpTest) -> u16 {
        let jump_condition = match test {
            JumpTest::NotZero => !self.registers.f.zero,
            JumpTest::NotCarry => !self.registers.f.carry,
            JumpTest::Zero => self.registers.f.zero,
            JumpTest::Carry => self.registers.f.carry,
            JumpTest::Always => true,
            _ => panic!("Invalid JP call 'JumpTest::AlwaysHL'"),
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
            _ => unimplemented!(),
        }; // TODO make this into a method on the JumpTest enum maybe? not sure how due to self.register

        if jump_condition {
            self.pop_value()
        } else {
            self.pc.wrapping_add(1)
        }
    }

    // Increments the pc and reads the next byte
    fn read_next_byte(&mut self) -> u8 {
        self.pc = self.pc.wrapping_add(1);
        self.bus.read_byte(self.pc)
    }

    // Increments the pc and reads the next word
    fn read_next_word(&mut self) -> u16 {
        self.pc = self.pc.wrapping_add(1);
        (self.bus.read_byte(self.pc) as u16) << 8 | self.bus.read_byte(self.pc.wrapping_add(2)) as u16
    }
}

const CLOCK: usize = 4194304; // in Hz
const FPS: usize = 30;
const ONE_FRAME_IN_CYCLES: usize = CLOCK / ( 4 * FPS );

fn run(mut cpu: CPU, mut window: Window) {
    let mut window_buffer = [0; WIDTH * HEIGHT];
    let mut cycles_elapsed_in_frame = 0usize;
    let mut now = Instant::now();

    while window.is_open() && !window.is_key_down(Key::Escape) {
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

    // TODO gameboy framerate is 59.727500569606 Hz
    window.limit_update_rate(Some(std::time::Duration::from_micros(8300)));

    let mut cpu = CPU::new();

    run(cpu, window);
}
