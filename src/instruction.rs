use core::fmt;
use std::fmt::Display;

/*
    All CPU instructions for LR35902
*/
#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // Arithmetic instructions
    ADD(ArithmeticType),  // Add target to register A
    ADC(ArithmeticType),  // Add the target plus carry flag to register A
    SUB(ByteTarget),      // Subtract target from register A
    SBC(ByteTarget),      // Subtract target and carry flag from register A
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
    RL(ByteTarget),   // Rotate register left through carry
    RRC(ByteTarget),  // Rotate register right
    RR(ByteTarget),   // Rotate register right through carry
    SLA(ByteTarget),  // Shift arithmetic register left
    SRA(ByteTarget),  // Shift arithmetic register right
    SWAP(ByteTarget), // Swap upper 4 bits and lower 4 bits
    SRL(ByteTarget),  // Shift logic register right
    BIT(u8, ByteTarget),  // Test bit n at given register and set zero flag if not set
    RES(u8, ByteTarget),  // Set bit n at given register to 0
    SET(u8, ByteTarget),  // Set bit n at given register to 1

    // Jump instructions
    JP(JumpType),
    JR(JumpCondition), // Relative jump

    // Memory instructions
    LD(LoadType),

    // Stack instructions
    PUSH(StackTarget),
    POP(StackTarget),

    // Function call
    CALL(JumpCondition),
    RET(JumpCondition),
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
#[derive(Debug, Clone, Copy)] pub enum ArithmeticType { Byte(ByteTarget), Word(WordTarget), SP }

#[derive(Debug, Clone, Copy)] pub enum ByteTarget { A, B, C, D, E, H, L, D8, HLI }
#[derive(Debug, Clone, Copy)] pub enum WordTarget { BC, DE, HL, SP }

#[derive(Debug, Clone, Copy)] pub enum JumpType { Word(JumpCondition), Address }
#[derive(Debug, Clone, Copy)] pub enum JumpCondition { NotZero, Zero, NotCarry, Carry, Always }

#[derive(Debug, Clone, Copy)]
pub enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget, LoadWordSource),
    AFromIndirect(LoadMemoryLocation), // Load memory location into A
    IndirectFromA(LoadMemoryLocation), // Load A into memory location
    AFromByteAddress(ByteAddress), // Load IO location into A
    ByteAddressFromA(ByteAddress), // Load A into IO location
    SPFromHL, // Load HL into SP
    HLFromSP, // Load SP+i8 into HL
}

#[derive(Debug, Clone, Copy)] pub enum LoadByteTarget { A, B, C, D, E, H, L, HLI }
#[derive(Debug, Clone, Copy)] pub enum LoadByteSource { A, B, C, D, E, H, L, D8, HLI }
#[derive(Debug, Clone, Copy)] pub enum LoadWordTarget { BC, DE, HL, SP, D16 }
#[derive(Debug, Clone, Copy)] pub enum LoadWordSource { SP, D16 }
#[derive(Debug, Clone, Copy)] pub enum LoadMemoryLocation { BC, DE, HLpostinc, HLpredec }
#[derive(Debug, Clone, Copy)] pub enum ByteAddress { C, D8, D16 }

#[derive(Debug, Clone, Copy)] pub enum StackTarget { AF, BC, DE, HL }

#[derive(Debug, Clone, Copy)]
pub enum RSTVec {
    // Instruction vectors, called by ROMS
    H00 = 0x00,
    H10 = 0x10,
    H20 = 0x20,
    H30 = 0x30,
    H08 = 0x08,
    H18 = 0x18,
    H28 = 0x28,
    H38 = 0x38,
    // Interrupt vectors, only called internally
    I40 = 0x40,
    I48 = 0x48,
    I50 = 0x50,
    I58 = 0x58,
    I60 = 0x60,
}

impl Instruction {
    // Converts a byte into an instruction
    // Full opcode table at https://izik1.github.io/gbops/
    pub fn from_byte(byte: u8, prefixed: bool) -> Option<Instruction> {
        if prefixed {
            Instruction::from_byte_prefixed(byte)
        } else {
            Instruction::from_byte_not_prefixed(byte)
        }
    }

    fn from_byte_not_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            // Load instructions 8-bit
            0x02 => Some(Instruction::LD(LoadType::IndirectFromA(LoadMemoryLocation::BC))),
            0x12 => Some(Instruction::LD(LoadType::IndirectFromA(LoadMemoryLocation::DE))),
            0x22 => Some(Instruction::LD(LoadType::IndirectFromA(LoadMemoryLocation::HLpostinc))),
            0x32 => Some(Instruction::LD(LoadType::IndirectFromA(LoadMemoryLocation::HLpredec))),

            0x06 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::D8))),
            0x16 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::D8))),
            0x26 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::D8))),
            0x36 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::D8))),

            0x0A => Some(Instruction::LD(LoadType::AFromIndirect(LoadMemoryLocation::BC))),
            0x1A => Some(Instruction::LD(LoadType::AFromIndirect(LoadMemoryLocation::DE))),
            0x2A => Some(Instruction::LD(LoadType::AFromIndirect(LoadMemoryLocation::HLpostinc))),
            0x3A => Some(Instruction::LD(LoadType::AFromIndirect(LoadMemoryLocation::HLpredec))),

            0x0E => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::D8))),
            0x1E => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::D8))),
            0x2E => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::D8))),
            0x3E => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::D8))),

            0x40 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::B))),
            0x41 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::C))),
            0x42 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::D))),
            0x43 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::E))),
            0x44 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::H))),
            0x45 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::L))),
            0x46 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::HLI))),
            0x47 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::A))),
            0x48 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::B))),
            0x49 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::C))),
            0x4A => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::D))),
            0x4B => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::E))),
            0x4C => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::H))),
            0x4D => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::L))),
            0x4E => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::HLI))),
            0x4F => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::A))),

            0x50 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::B))),
            0x51 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::C))),
            0x52 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::D))),
            0x53 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::E))),
            0x54 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::H))),
            0x55 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::L))),
            0x56 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::HLI))),
            0x57 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::D, LoadByteSource::A))),
            0x58 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::B))),
            0x59 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::C))),
            0x5A => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::D))),
            0x5B => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::E))),
            0x5C => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::H))),
            0x5D => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::L))),
            0x5E => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::HLI))),
            0x5F => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::E, LoadByteSource::A))),

            0x60 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::B))),
            0x61 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::C))),
            0x62 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::D))),
            0x63 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::E))),
            0x64 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::H))),
            0x65 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::L))),
            0x66 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::HLI))),
            0x67 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::H, LoadByteSource::A))),
            0x68 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::B))),
            0x69 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::C))),
            0x6A => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::D))),
            0x6B => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::E))),
            0x6C => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::H))),
            0x6D => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::L))),
            0x6E => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::HLI))),
            0x6F => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::L, LoadByteSource::A))),

            0x70 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::B))),
            0x71 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::C))),
            0x72 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::D))),
            0x73 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::E))),
            0x74 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::H))),
            0x75 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::L))),
            0x77 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::A))),
            0x78 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::B))),
            0x79 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::C))),
            0x7A => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::D))),
            0x7B => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::E))),
            0x7C => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::H))),
            0x7D => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::L))),
            0x7E => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::HLI))),
            0x7F => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::A))),

            // Load instructions 16-bit
            0x01 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::BC, LoadWordSource::D16))),
            0x11 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::DE, LoadWordSource::D16))),
            0x21 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::HL, LoadWordSource::D16))),
            0x31 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::SP, LoadWordSource::D16))),
            0x08 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::D16, LoadWordSource::SP))),
            0xF9 => Some(Instruction::LD(LoadType::SPFromHL)),

            0xEA => Some(Instruction::LD(LoadType::ByteAddressFromA(ByteAddress::D16))),
            0xFA => Some(Instruction::LD(LoadType::AFromByteAddress(ByteAddress::D16))),

            0xE0 => Some(Instruction::LD(LoadType::ByteAddressFromA(ByteAddress::D8))),
            0xF0 => Some(Instruction::LD(LoadType::AFromByteAddress(ByteAddress::D8))),
            0xE2 => Some(Instruction::LD(LoadType::ByteAddressFromA(ByteAddress::C))),
            0xF2 => Some(Instruction::LD(LoadType::AFromByteAddress(ByteAddress::C))),

            0xF8 => Some(Instruction::LD(LoadType::HLFromSP)),

            // Increment and decrement instructions
            0x04 => Some(Instruction::INC(ArithmeticType::Byte(ByteTarget::B))),
            0x14 => Some(Instruction::INC(ArithmeticType::Byte(ByteTarget::D))),
            0x24 => Some(Instruction::INC(ArithmeticType::Byte(ByteTarget::H))),
            0x34 => Some(Instruction::INC(ArithmeticType::Byte(ByteTarget::HLI))),

            0x0C => Some(Instruction::INC(ArithmeticType::Byte(ByteTarget::C))),
            0x1C => Some(Instruction::INC(ArithmeticType::Byte(ByteTarget::E))),
            0x2C => Some(Instruction::INC(ArithmeticType::Byte(ByteTarget::L))),
            0x3C => Some(Instruction::INC(ArithmeticType::Byte(ByteTarget::A))),

            0x05 => Some(Instruction::DEC(ArithmeticType::Byte(ByteTarget::B))),
            0x15 => Some(Instruction::DEC(ArithmeticType::Byte(ByteTarget::D))),
            0x25 => Some(Instruction::DEC(ArithmeticType::Byte(ByteTarget::H))),
            0x35 => Some(Instruction::DEC(ArithmeticType::Byte(ByteTarget::HLI))),

            0x0D => Some(Instruction::DEC(ArithmeticType::Byte(ByteTarget::C))),
            0x1D => Some(Instruction::DEC(ArithmeticType::Byte(ByteTarget::E))),
            0x2D => Some(Instruction::DEC(ArithmeticType::Byte(ByteTarget::L))),
            0x3D => Some(Instruction::DEC(ArithmeticType::Byte(ByteTarget::A))),

            0x03 => Some(Instruction::INC(ArithmeticType::Word(WordTarget::BC))),
            0x13 => Some(Instruction::INC(ArithmeticType::Word(WordTarget::DE))),
            0x23 => Some(Instruction::INC(ArithmeticType::Word(WordTarget::HL))),
            0x33 => Some(Instruction::INC(ArithmeticType::Word(WordTarget::SP))),

            0x0B => Some(Instruction::DEC(ArithmeticType::Word(WordTarget::BC))),
            0x1B => Some(Instruction::DEC(ArithmeticType::Word(WordTarget::DE))),
            0x2B => Some(Instruction::DEC(ArithmeticType::Word(WordTarget::HL))),
            0x3B => Some(Instruction::DEC(ArithmeticType::Word(WordTarget::SP))),

            // Bit shift instructions
            0x07 => Some(Instruction::RLCA),
            0x0F => Some(Instruction::RRCA),
            0x17 => Some(Instruction::RLA),
            0x1F => Some(Instruction::RRA),

            // Aritmetic add instructions
            0x80 => Some(Instruction::ADD(ArithmeticType::Byte(ByteTarget::B))),
            0x81 => Some(Instruction::ADD(ArithmeticType::Byte(ByteTarget::C))),
            0x82 => Some(Instruction::ADD(ArithmeticType::Byte(ByteTarget::D))),
            0x83 => Some(Instruction::ADD(ArithmeticType::Byte(ByteTarget::E))),
            0x84 => Some(Instruction::ADD(ArithmeticType::Byte(ByteTarget::H))),
            0x85 => Some(Instruction::ADD(ArithmeticType::Byte(ByteTarget::L))),
            0x86 => Some(Instruction::ADD(ArithmeticType::Byte(ByteTarget::HLI))),
            0x87 => Some(Instruction::ADD(ArithmeticType::Byte(ByteTarget::A))),
            0xC6 => Some(Instruction::ADD(ArithmeticType::Byte(ByteTarget::D8))),

            0x88 => Some(Instruction::ADC(ArithmeticType::Byte(ByteTarget::B))),
            0x89 => Some(Instruction::ADC(ArithmeticType::Byte(ByteTarget::C))),
            0x8A => Some(Instruction::ADC(ArithmeticType::Byte(ByteTarget::D))),
            0x8B => Some(Instruction::ADC(ArithmeticType::Byte(ByteTarget::E))),
            0x8C => Some(Instruction::ADC(ArithmeticType::Byte(ByteTarget::H))),
            0x8D => Some(Instruction::ADC(ArithmeticType::Byte(ByteTarget::L))),
            0x8E => Some(Instruction::ADC(ArithmeticType::Byte(ByteTarget::HLI))),
            0x8F => Some(Instruction::ADC(ArithmeticType::Byte(ByteTarget::A))),
            0xCE => Some(Instruction::ADC(ArithmeticType::Byte(ByteTarget::D8))),

            0x09 => Some(Instruction::ADD(ArithmeticType::Word(WordTarget::BC))),
            0x19 => Some(Instruction::ADD(ArithmeticType::Word(WordTarget::DE))),
            0x29 => Some(Instruction::ADD(ArithmeticType::Word(WordTarget::HL))),
            0x39 => Some(Instruction::ADD(ArithmeticType::Word(WordTarget::SP))),
            0xE8 => Some(Instruction::ADD(ArithmeticType::SP)),

            // Arithmetic subtract
            0x90 => Some(Instruction::SUB(ByteTarget::B)),
            0x91 => Some(Instruction::SUB(ByteTarget::C)),
            0x92 => Some(Instruction::SUB(ByteTarget::D)),
            0x93 => Some(Instruction::SUB(ByteTarget::E)),
            0x94 => Some(Instruction::SUB(ByteTarget::H)),
            0x95 => Some(Instruction::SUB(ByteTarget::L)),
            0x96 => Some(Instruction::SUB(ByteTarget::HLI)),
            0x97 => Some(Instruction::SUB(ByteTarget::A)),
            0xD6 => Some(Instruction::SUB(ByteTarget::D8)),

            0x98 => Some(Instruction::SBC(ByteTarget::B)),
            0x99 => Some(Instruction::SBC(ByteTarget::C)),
            0x9A => Some(Instruction::SBC(ByteTarget::D)),
            0x9B => Some(Instruction::SBC(ByteTarget::E)),
            0x9C => Some(Instruction::SBC(ByteTarget::H)),
            0x9D => Some(Instruction::SBC(ByteTarget::L)),
            0x9E => Some(Instruction::SBC(ByteTarget::HLI)),
            0x9F => Some(Instruction::SBC(ByteTarget::A)),
            0xDE => Some(Instruction::SBC(ByteTarget::D8)),

            // Logic AND
            0xA0 => Some(Instruction::AND(ByteTarget::B)),
            0xA1 => Some(Instruction::AND(ByteTarget::C)),
            0xA2 => Some(Instruction::AND(ByteTarget::D)),
            0xA3 => Some(Instruction::AND(ByteTarget::E)),
            0xA4 => Some(Instruction::AND(ByteTarget::H)),
            0xA5 => Some(Instruction::AND(ByteTarget::L)),
            0xA6 => Some(Instruction::AND(ByteTarget::HLI)),
            0xA7 => Some(Instruction::AND(ByteTarget::A)),
            0xE6 => Some(Instruction::AND(ByteTarget::D8)),

            // Logic XOR
            0xA8 => Some(Instruction::XOR(ByteTarget::B)),
            0xA9 => Some(Instruction::XOR(ByteTarget::C)),
            0xAA => Some(Instruction::XOR(ByteTarget::D)),
            0xAB => Some(Instruction::XOR(ByteTarget::E)),
            0xAC => Some(Instruction::XOR(ByteTarget::H)),
            0xAD => Some(Instruction::XOR(ByteTarget::L)),
            0xAE => Some(Instruction::XOR(ByteTarget::HLI)),
            0xAF => Some(Instruction::XOR(ByteTarget::A)),
            0xEE => Some(Instruction::XOR(ByteTarget::D8)),

            // Logic OR
            0xB0 => Some(Instruction::OR(ByteTarget::B)),
            0xB1 => Some(Instruction::OR(ByteTarget::C)),
            0xB2 => Some(Instruction::OR(ByteTarget::D)),
            0xB3 => Some(Instruction::OR(ByteTarget::E)),
            0xB4 => Some(Instruction::OR(ByteTarget::H)),
            0xB5 => Some(Instruction::OR(ByteTarget::L)),
            0xB6 => Some(Instruction::OR(ByteTarget::HLI)),
            0xB7 => Some(Instruction::OR(ByteTarget::A)),
            0xF6 => Some(Instruction::OR(ByteTarget::D8)),

            // Logic CP
            0xB8 => Some(Instruction::CP(ByteTarget::B)),
            0xB9 => Some(Instruction::CP(ByteTarget::C)),
            0xBA => Some(Instruction::CP(ByteTarget::D)),
            0xBB => Some(Instruction::CP(ByteTarget::E)),
            0xBC => Some(Instruction::CP(ByteTarget::H)),
            0xBD => Some(Instruction::CP(ByteTarget::L)),
            0xBE => Some(Instruction::CP(ByteTarget::HLI)),
            0xBF => Some(Instruction::CP(ByteTarget::A)),
            0xFE => Some(Instruction::CP(ByteTarget::D8)),

            // Jump instructions
            0x18 => Some(Instruction::JR(JumpCondition::Always)),
            0x20 => Some(Instruction::JR(JumpCondition::NotZero)),
            0x30 => Some(Instruction::JR(JumpCondition::NotCarry)),
            0x28 => Some(Instruction::JR(JumpCondition::Zero)),
            0x38 => Some(Instruction::JR(JumpCondition::Carry)),

            0xC2 => Some(Instruction::JP(JumpType::Word(JumpCondition::NotZero))),
            0xD2 => Some(Instruction::JP(JumpType::Word(JumpCondition::NotCarry))),
            0xC3 => Some(Instruction::JP(JumpType::Word(JumpCondition::Always))),
            0xE9 => Some(Instruction::JP(JumpType::Address)),
            0xCA => Some(Instruction::JP(JumpType::Word(JumpCondition::Zero))),
            0xDA => Some(Instruction::JP(JumpType::Word(JumpCondition::Carry))),

            0xC4 => Some(Instruction::CALL(JumpCondition::NotZero)),
            0xD4 => Some(Instruction::CALL(JumpCondition::NotCarry)),
            0xCC => Some(Instruction::CALL(JumpCondition::Zero)),
            0xDC => Some(Instruction::CALL(JumpCondition::Carry)),
            0xCD => Some(Instruction::CALL(JumpCondition::Always)),

            0xC0 => Some(Instruction::RET(JumpCondition::NotZero)),
            0xD0 => Some(Instruction::RET(JumpCondition::NotCarry)),
            0xC8 => Some(Instruction::RET(JumpCondition::Zero)),
            0xD8 => Some(Instruction::RET(JumpCondition::Carry)),
            0xC9 => Some(Instruction::RET(JumpCondition::Always)),

            0xD9 => Some(Instruction::RETI),

            0xC7 => Some(Instruction::RST(RSTVec::H00)),
            0xD7 => Some(Instruction::RST(RSTVec::H10)),
            0xE7 => Some(Instruction::RST(RSTVec::H20)),
            0xF7 => Some(Instruction::RST(RSTVec::H30)),
            0xCF => Some(Instruction::RST(RSTVec::H08)),
            0xDF => Some(Instruction::RST(RSTVec::H18)),
            0xEF => Some(Instruction::RST(RSTVec::H28)),
            0xFF => Some(Instruction::RST(RSTVec::H38)),

            // Stack instructions
            0xC1 => Some(Instruction::POP(StackTarget::BC)),
            0xD1 => Some(Instruction::POP(StackTarget::DE)),
            0xE1 => Some(Instruction::POP(StackTarget::HL)),
            0xF1 => Some(Instruction::POP(StackTarget::AF)),

            0xC5 => Some(Instruction::PUSH(StackTarget::BC)),
            0xD5 => Some(Instruction::PUSH(StackTarget::DE)),
            0xE5 => Some(Instruction::PUSH(StackTarget::HL)),
            0xF5 => Some(Instruction::PUSH(StackTarget::AF)),

            // Misc
            0x00 => Some(Instruction::NOP),
            0x10 => Some(Instruction::STOP),
            0x76 => Some(Instruction::HALT),
            0x27 => Some(Instruction::DAA),
            0x37 => Some(Instruction::SCF),
            0x2F => Some(Instruction::CPL),
            0x3F => Some(Instruction::CCF),
            0xF3 => Some(Instruction::DI),
            0xFB => Some(Instruction::EI),
            0xCB => None, // Prefixed instruction should not land in this method
            _ => Some(Instruction::NOP), // Not confirmed
        }
    }

    // Prefixed instructions are instructions starting with 0xCB
    fn from_byte_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x00 => Some(Instruction::RLC(ByteTarget::B)),
            0x01 => Some(Instruction::RLC(ByteTarget::C)),
            0x02 => Some(Instruction::RLC(ByteTarget::D)),
            0x03 => Some(Instruction::RLC(ByteTarget::E)),
            0x04 => Some(Instruction::RLC(ByteTarget::H)),
            0x05 => Some(Instruction::RLC(ByteTarget::L)),
            0x06 => Some(Instruction::RLC(ByteTarget::HLI)),
            0x07 => Some(Instruction::RLC(ByteTarget::A)),

            0x08 => Some(Instruction::RRC(ByteTarget::B)),
            0x09 => Some(Instruction::RRC(ByteTarget::C)),
            0x0A => Some(Instruction::RRC(ByteTarget::D)),
            0x0B => Some(Instruction::RRC(ByteTarget::E)),
            0x0C => Some(Instruction::RRC(ByteTarget::H)),
            0x0D => Some(Instruction::RRC(ByteTarget::L)),
            0x0E => Some(Instruction::RRC(ByteTarget::HLI)),
            0x0F => Some(Instruction::RRC(ByteTarget::A)),

            0x10 => Some(Instruction::RL(ByteTarget::B)),
            0x11 => Some(Instruction::RL(ByteTarget::C)),
            0x12 => Some(Instruction::RL(ByteTarget::D)),
            0x13 => Some(Instruction::RL(ByteTarget::E)),
            0x14 => Some(Instruction::RL(ByteTarget::H)),
            0x15 => Some(Instruction::RL(ByteTarget::L)),
            0x16 => Some(Instruction::RL(ByteTarget::HLI)),
            0x17 => Some(Instruction::RL(ByteTarget::A)),

            0x18 => Some(Instruction::RR(ByteTarget::B)),
            0x19 => Some(Instruction::RR(ByteTarget::C)),
            0x1A => Some(Instruction::RR(ByteTarget::D)),
            0x1B => Some(Instruction::RR(ByteTarget::E)),
            0x1C => Some(Instruction::RR(ByteTarget::H)),
            0x1D => Some(Instruction::RR(ByteTarget::L)),
            0x1E => Some(Instruction::RR(ByteTarget::HLI)),
            0x1F => Some(Instruction::RR(ByteTarget::A)),

            0x20 => Some(Instruction::SLA(ByteTarget::B)),
            0x21 => Some(Instruction::SLA(ByteTarget::C)),
            0x22 => Some(Instruction::SLA(ByteTarget::D)),
            0x23 => Some(Instruction::SLA(ByteTarget::E)),
            0x24 => Some(Instruction::SLA(ByteTarget::H)),
            0x25 => Some(Instruction::SLA(ByteTarget::L)),
            0x26 => Some(Instruction::SLA(ByteTarget::HLI)),
            0x27 => Some(Instruction::SLA(ByteTarget::A)),

            0x28 => Some(Instruction::SRA(ByteTarget::B)),
            0x29 => Some(Instruction::SRA(ByteTarget::C)),
            0x2A => Some(Instruction::SRA(ByteTarget::D)),
            0x2B => Some(Instruction::SRA(ByteTarget::E)),
            0x2C => Some(Instruction::SRA(ByteTarget::H)),
            0x2D => Some(Instruction::SRA(ByteTarget::L)),
            0x2E => Some(Instruction::SRA(ByteTarget::HLI)),
            0x2F => Some(Instruction::SRA(ByteTarget::A)),

            0x30 => Some(Instruction::SWAP(ByteTarget::B)),
            0x31 => Some(Instruction::SWAP(ByteTarget::C)),
            0x32 => Some(Instruction::SWAP(ByteTarget::D)),
            0x33 => Some(Instruction::SWAP(ByteTarget::E)),
            0x34 => Some(Instruction::SWAP(ByteTarget::H)),
            0x35 => Some(Instruction::SWAP(ByteTarget::L)),
            0x36 => Some(Instruction::SWAP(ByteTarget::HLI)),
            0x37 => Some(Instruction::SWAP(ByteTarget::A)),

            0x38 => Some(Instruction::SRL(ByteTarget::B)),
            0x39 => Some(Instruction::SRL(ByteTarget::C)),
            0x3A => Some(Instruction::SRL(ByteTarget::D)),
            0x3B => Some(Instruction::SRL(ByteTarget::E)),
            0x3C => Some(Instruction::SRL(ByteTarget::H)),
            0x3D => Some(Instruction::SRL(ByteTarget::L)),
            0x3E => Some(Instruction::SRL(ByteTarget::HLI)),
            0x3F => Some(Instruction::SRL(ByteTarget::A)),

            // Bit instructions
            0x40 => Some(Instruction::BIT(0, ByteTarget::B)),
            0x41 => Some(Instruction::BIT(0, ByteTarget::C)),
            0x42 => Some(Instruction::BIT(0, ByteTarget::D)),
            0x43 => Some(Instruction::BIT(0, ByteTarget::E)),
            0x44 => Some(Instruction::BIT(0, ByteTarget::H)),
            0x45 => Some(Instruction::BIT(0, ByteTarget::L)),
            0x46 => Some(Instruction::BIT(0, ByteTarget::HLI)),
            0x47 => Some(Instruction::BIT(0, ByteTarget::A)),
            0x48 => Some(Instruction::BIT(1, ByteTarget::B)),
            0x49 => Some(Instruction::BIT(1, ByteTarget::C)),
            0x4A => Some(Instruction::BIT(1, ByteTarget::D)),
            0x4B => Some(Instruction::BIT(1, ByteTarget::E)),
            0x4C => Some(Instruction::BIT(1, ByteTarget::H)),
            0x4D => Some(Instruction::BIT(1, ByteTarget::L)),
            0x4E => Some(Instruction::BIT(1, ByteTarget::HLI)),
            0x4F => Some(Instruction::BIT(1, ByteTarget::A)),
            0x50 => Some(Instruction::BIT(2, ByteTarget::B)),
            0x51 => Some(Instruction::BIT(2, ByteTarget::C)),
            0x52 => Some(Instruction::BIT(2, ByteTarget::D)),
            0x53 => Some(Instruction::BIT(2, ByteTarget::E)),
            0x54 => Some(Instruction::BIT(2, ByteTarget::H)),
            0x55 => Some(Instruction::BIT(2, ByteTarget::L)),
            0x56 => Some(Instruction::BIT(2, ByteTarget::HLI)),
            0x57 => Some(Instruction::BIT(2, ByteTarget::A)),
            0x58 => Some(Instruction::BIT(3, ByteTarget::B)),
            0x59 => Some(Instruction::BIT(3, ByteTarget::C)),
            0x5A => Some(Instruction::BIT(3, ByteTarget::D)),
            0x5B => Some(Instruction::BIT(3, ByteTarget::E)),
            0x5C => Some(Instruction::BIT(3, ByteTarget::H)),
            0x5D => Some(Instruction::BIT(3, ByteTarget::L)),
            0x5E => Some(Instruction::BIT(3, ByteTarget::HLI)),
            0x5F => Some(Instruction::BIT(3, ByteTarget::A)),
            0x60 => Some(Instruction::BIT(4, ByteTarget::B)),
            0x61 => Some(Instruction::BIT(4, ByteTarget::C)),
            0x62 => Some(Instruction::BIT(4, ByteTarget::D)),
            0x63 => Some(Instruction::BIT(4, ByteTarget::E)),
            0x64 => Some(Instruction::BIT(4, ByteTarget::H)),
            0x65 => Some(Instruction::BIT(4, ByteTarget::L)),
            0x66 => Some(Instruction::BIT(4, ByteTarget::HLI)),
            0x67 => Some(Instruction::BIT(4, ByteTarget::A)),
            0x68 => Some(Instruction::BIT(5, ByteTarget::B)),
            0x69 => Some(Instruction::BIT(5, ByteTarget::C)),
            0x6A => Some(Instruction::BIT(5, ByteTarget::D)),
            0x6B => Some(Instruction::BIT(5, ByteTarget::E)),
            0x6C => Some(Instruction::BIT(5, ByteTarget::H)),
            0x6D => Some(Instruction::BIT(5, ByteTarget::L)),
            0x6E => Some(Instruction::BIT(5, ByteTarget::HLI)),
            0x6F => Some(Instruction::BIT(5, ByteTarget::A)),
            0x70 => Some(Instruction::BIT(6, ByteTarget::B)),
            0x71 => Some(Instruction::BIT(6, ByteTarget::C)),
            0x72 => Some(Instruction::BIT(6, ByteTarget::D)),
            0x73 => Some(Instruction::BIT(6, ByteTarget::E)),
            0x74 => Some(Instruction::BIT(6, ByteTarget::H)),
            0x75 => Some(Instruction::BIT(6, ByteTarget::L)),
            0x76 => Some(Instruction::BIT(6, ByteTarget::HLI)),
            0x77 => Some(Instruction::BIT(6, ByteTarget::A)),
            0x78 => Some(Instruction::BIT(7, ByteTarget::B)),
            0x79 => Some(Instruction::BIT(7, ByteTarget::C)),
            0x7A => Some(Instruction::BIT(7, ByteTarget::D)),
            0x7B => Some(Instruction::BIT(7, ByteTarget::E)),
            0x7C => Some(Instruction::BIT(7, ByteTarget::H)),
            0x7D => Some(Instruction::BIT(7, ByteTarget::L)),
            0x7E => Some(Instruction::BIT(7, ByteTarget::HLI)),
            0x7F => Some(Instruction::BIT(7, ByteTarget::A)),

            // RES instructions
            0x80 => Some(Instruction::RES(0, ByteTarget::B)),
            0x81 => Some(Instruction::RES(0, ByteTarget::C)),
            0x82 => Some(Instruction::RES(0, ByteTarget::D)),
            0x83 => Some(Instruction::RES(0, ByteTarget::E)),
            0x84 => Some(Instruction::RES(0, ByteTarget::H)),
            0x85 => Some(Instruction::RES(0, ByteTarget::L)),
            0x86 => Some(Instruction::RES(0, ByteTarget::HLI)),
            0x87 => Some(Instruction::RES(0, ByteTarget::A)),
            0x88 => Some(Instruction::RES(1, ByteTarget::B)),
            0x89 => Some(Instruction::RES(1, ByteTarget::C)),
            0x8A => Some(Instruction::RES(1, ByteTarget::D)),
            0x8B => Some(Instruction::RES(1, ByteTarget::E)),
            0x8C => Some(Instruction::RES(1, ByteTarget::H)),
            0x8D => Some(Instruction::RES(1, ByteTarget::L)),
            0x8E => Some(Instruction::RES(1, ByteTarget::HLI)),
            0x8F => Some(Instruction::RES(1, ByteTarget::A)),
            0x90 => Some(Instruction::RES(2, ByteTarget::B)),
            0x91 => Some(Instruction::RES(2, ByteTarget::C)),
            0x92 => Some(Instruction::RES(2, ByteTarget::D)),
            0x93 => Some(Instruction::RES(2, ByteTarget::E)),
            0x94 => Some(Instruction::RES(2, ByteTarget::H)),
            0x95 => Some(Instruction::RES(2, ByteTarget::L)),
            0x96 => Some(Instruction::RES(2, ByteTarget::HLI)),
            0x97 => Some(Instruction::RES(2, ByteTarget::A)),
            0x98 => Some(Instruction::RES(3, ByteTarget::B)),
            0x99 => Some(Instruction::RES(3, ByteTarget::C)),
            0x9A => Some(Instruction::RES(3, ByteTarget::D)),
            0x9B => Some(Instruction::RES(3, ByteTarget::E)),
            0x9C => Some(Instruction::RES(3, ByteTarget::H)),
            0x9D => Some(Instruction::RES(3, ByteTarget::L)),
            0x9E => Some(Instruction::RES(3, ByteTarget::HLI)),
            0x9F => Some(Instruction::RES(3, ByteTarget::A)),
            0xA0 => Some(Instruction::RES(4, ByteTarget::B)),
            0xA1 => Some(Instruction::RES(4, ByteTarget::C)),
            0xA2 => Some(Instruction::RES(4, ByteTarget::D)),
            0xA3 => Some(Instruction::RES(4, ByteTarget::E)),
            0xA4 => Some(Instruction::RES(4, ByteTarget::H)),
            0xA5 => Some(Instruction::RES(4, ByteTarget::L)),
            0xA6 => Some(Instruction::RES(4, ByteTarget::HLI)),
            0xA7 => Some(Instruction::RES(4, ByteTarget::A)),
            0xA8 => Some(Instruction::RES(5, ByteTarget::B)),
            0xA9 => Some(Instruction::RES(5, ByteTarget::C)),
            0xAA => Some(Instruction::RES(5, ByteTarget::D)),
            0xAB => Some(Instruction::RES(5, ByteTarget::E)),
            0xAC => Some(Instruction::RES(5, ByteTarget::H)),
            0xAD => Some(Instruction::RES(5, ByteTarget::L)),
            0xAE => Some(Instruction::RES(5, ByteTarget::HLI)),
            0xAF => Some(Instruction::RES(5, ByteTarget::A)),
            0xB0 => Some(Instruction::RES(6, ByteTarget::B)),
            0xB1 => Some(Instruction::RES(6, ByteTarget::C)),
            0xB2 => Some(Instruction::RES(6, ByteTarget::D)),
            0xB3 => Some(Instruction::RES(6, ByteTarget::E)),
            0xB4 => Some(Instruction::RES(6, ByteTarget::H)),
            0xB5 => Some(Instruction::RES(6, ByteTarget::L)),
            0xB6 => Some(Instruction::RES(6, ByteTarget::HLI)),
            0xB7 => Some(Instruction::RES(6, ByteTarget::A)),
            0xB8 => Some(Instruction::RES(7, ByteTarget::B)),
            0xB9 => Some(Instruction::RES(7, ByteTarget::C)),
            0xBA => Some(Instruction::RES(7, ByteTarget::D)),
            0xBB => Some(Instruction::RES(7, ByteTarget::E)),
            0xBC => Some(Instruction::RES(7, ByteTarget::H)),
            0xBD => Some(Instruction::RES(7, ByteTarget::L)),
            0xBE => Some(Instruction::RES(7, ByteTarget::HLI)),
            0xBF => Some(Instruction::RES(7, ByteTarget::A)),

            // SET instructions
            0xC0 => Some(Instruction::SET(0, ByteTarget::B)),
            0xC1 => Some(Instruction::SET(0, ByteTarget::C)),
            0xC2 => Some(Instruction::SET(0, ByteTarget::D)),
            0xC3 => Some(Instruction::SET(0, ByteTarget::E)),
            0xC4 => Some(Instruction::SET(0, ByteTarget::H)),
            0xC5 => Some(Instruction::SET(0, ByteTarget::L)),
            0xC6 => Some(Instruction::SET(0, ByteTarget::HLI)),
            0xC7 => Some(Instruction::SET(0, ByteTarget::A)),
            0xC8 => Some(Instruction::SET(1, ByteTarget::B)),
            0xC9 => Some(Instruction::SET(1, ByteTarget::C)),
            0xCA => Some(Instruction::SET(1, ByteTarget::D)),
            0xCB => Some(Instruction::SET(1, ByteTarget::E)),
            0xCC => Some(Instruction::SET(1, ByteTarget::H)),
            0xCD => Some(Instruction::SET(1, ByteTarget::L)),
            0xCE => Some(Instruction::SET(1, ByteTarget::HLI)),
            0xCF => Some(Instruction::SET(1, ByteTarget::A)),
            0xD0 => Some(Instruction::SET(2, ByteTarget::B)),
            0xD1 => Some(Instruction::SET(2, ByteTarget::C)),
            0xD2 => Some(Instruction::SET(2, ByteTarget::D)),
            0xD3 => Some(Instruction::SET(2, ByteTarget::E)),
            0xD4 => Some(Instruction::SET(2, ByteTarget::H)),
            0xD5 => Some(Instruction::SET(2, ByteTarget::L)),
            0xD6 => Some(Instruction::SET(2, ByteTarget::HLI)),
            0xD7 => Some(Instruction::SET(2, ByteTarget::A)),
            0xD8 => Some(Instruction::SET(3, ByteTarget::B)),
            0xD9 => Some(Instruction::SET(3, ByteTarget::C)),
            0xDA => Some(Instruction::SET(3, ByteTarget::D)),
            0xDB => Some(Instruction::SET(3, ByteTarget::E)),
            0xDC => Some(Instruction::SET(3, ByteTarget::H)),
            0xDD => Some(Instruction::SET(3, ByteTarget::L)),
            0xDE => Some(Instruction::SET(3, ByteTarget::HLI)),
            0xDF => Some(Instruction::SET(3, ByteTarget::A)),
            0xE0 => Some(Instruction::SET(4, ByteTarget::B)),
            0xE1 => Some(Instruction::SET(4, ByteTarget::C)),
            0xE2 => Some(Instruction::SET(4, ByteTarget::D)),
            0xE3 => Some(Instruction::SET(4, ByteTarget::E)),
            0xE4 => Some(Instruction::SET(4, ByteTarget::H)),
            0xE5 => Some(Instruction::SET(4, ByteTarget::L)),
            0xE6 => Some(Instruction::SET(4, ByteTarget::HLI)),
            0xE7 => Some(Instruction::SET(4, ByteTarget::A)),
            0xE8 => Some(Instruction::SET(5, ByteTarget::B)),
            0xE9 => Some(Instruction::SET(5, ByteTarget::C)),
            0xEA => Some(Instruction::SET(5, ByteTarget::D)),
            0xEB => Some(Instruction::SET(5, ByteTarget::E)),
            0xEC => Some(Instruction::SET(5, ByteTarget::H)),
            0xED => Some(Instruction::SET(5, ByteTarget::L)),
            0xEE => Some(Instruction::SET(5, ByteTarget::HLI)),
            0xEF => Some(Instruction::SET(5, ByteTarget::A)),
            0xF0 => Some(Instruction::SET(6, ByteTarget::B)),
            0xF1 => Some(Instruction::SET(6, ByteTarget::C)),
            0xF2 => Some(Instruction::SET(6, ByteTarget::D)),
            0xF3 => Some(Instruction::SET(6, ByteTarget::E)),
            0xF4 => Some(Instruction::SET(6, ByteTarget::H)),
            0xF5 => Some(Instruction::SET(6, ByteTarget::L)),
            0xF6 => Some(Instruction::SET(6, ByteTarget::HLI)),
            0xF7 => Some(Instruction::SET(6, ByteTarget::A)),
            0xF8 => Some(Instruction::SET(7, ByteTarget::B)),
            0xF9 => Some(Instruction::SET(7, ByteTarget::C)),
            0xFA => Some(Instruction::SET(7, ByteTarget::D)),
            0xFB => Some(Instruction::SET(7, ByteTarget::E)),
            0xFC => Some(Instruction::SET(7, ByteTarget::H)),
            0xFD => Some(Instruction::SET(7, ByteTarget::L)),
            0xFE => Some(Instruction::SET(7, ByteTarget::HLI)),
            0xFF => Some(Instruction::SET(7, ByteTarget::A)),
        }
    }

}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LD(loadtype) => {
                write!(f, "LD    {}", loadtype.to_string())?;
                Ok(())
            },
            &Instruction::INC(arithmetic_type) => {
                write!(f, "INC    {}", arithmetic_type.to_string())?;
                Ok(())
            },
            &Instruction::RET(jump_condition) => write!(f, "RET    {}", jump_condition),
            &Instruction::NOP => write!(f, "NOP"),
            _ => write!(f, "not implemented"),
        }
    }
}

impl Display for LoadType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => write!(f, "test"),
        }
    }
}

impl Display for ArithmeticType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArithmeticType::Byte(byte_target) => write!(f, "{}", byte_target),
            &ArithmeticType::Word(word_target) => write!(f, "{}", word_target),
            &ArithmeticType::SP => write!(f, "SP"),
        }
    }
}


impl Display for ByteTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ByteTarget::A => write!(f, "A"),
            ByteTarget::B => write!(f, "B"),
            &ByteTarget::C => write!(f, "C"),
            ByteTarget::D => write!(f, "D"),
            &ByteTarget::E => write!(f, "E"),
            ByteTarget::H => write!(f, "H"),
            &ByteTarget::L => write!(f, "L"),
            ByteTarget::D8 => write!(f, "D8"),
            &ByteTarget::HLI => write!(f, "HLI"),
        }
    }
}

impl Display for WordTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WordTarget::BC => write!(f, "BC"),
            WordTarget::DE => write!(f, "DE"),
            &WordTarget::HL => write!(f, "HL"),
            WordTarget::SP => write!(f, "SP"),
        }
    }
}

impl Display for JumpType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JumpType::Word(jump_condition) => write!(f, "{}", jump_condition),
            JumpType::Address => write!(f, "Address"),
        }
    }
}

impl Display for JumpCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JumpCondition::NotZero => write!(f, "NZ"),
            JumpCondition::Zero => write!(f, "Z"),
            JumpCondition::NotCarry => write!(f, "NC"),
            JumpCondition::Carry => write!(f, "C"),
            JumpCondition::Always => write!(f, ""),
        }
    }
}