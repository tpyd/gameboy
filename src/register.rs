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
pub struct FlagsRegister {
    pub zero: bool,         // z // TODO make fields private
    pub subtract: bool,     // n
    pub half_carry: bool,   // h
    pub carry: bool,        // c
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
pub struct Registers {
    pub a: u8, // TODO make fields private
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: FlagsRegister,
    pub h: u8,
    pub l: u8,
}

impl Registers {
    pub fn new() -> Self {
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

    pub fn get_af(&self) -> u16 {
        (self.a as u16) << 8 | u8::from(self.f) as u16
    }

    pub fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.f = FlagsRegister::from((value & 0x00FF) as u8);
    }

    pub fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }

    pub fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0x00FF) as u8;
    }

    pub fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | self.e as u16
    }

    pub fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0x00FF) as u8;
    }

    pub fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }

    pub fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0x00FF) as u8;
    }
}
