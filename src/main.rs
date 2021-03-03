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
}

enum ArithmeticTarget {
    A, B, C, D, E, H, L,
}

/*
    Implementation of the CPU LR35902
*/
struct CPU {
    registers: Registers,
}

impl CPU {
    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::ADD(target) => { self.add(target) }
            _ => {} // TODO add more instructions
        }
    }

    /*
        ADD instruction.
        Reads the current value from target register. Adds the value to register A overflowing if necessary.
        Updates the flags register and writer the new value to register A.
    */
    fn add(&mut self, target: ArithmeticTarget) {
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

        self.registers.a = new_value
    }
}

fn main() {
    println!("Hello, world!");
}
