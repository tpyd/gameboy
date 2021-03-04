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
}

impl Instruction {
    fn from_byte(byte: u8, prefixed: bool) -> Option<Instruction> {
        if prefixed {
            Instruction::from_byte_prefixed(byte)
        } else {
            Instruction::from_byte_not_prefixed(byte)
        }
    }

    fn from_byte_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x00 => Some(Instruction::RLC(PrefixTarget::B))
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

enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
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

/*
    The memory of the gameboy
    0x0000 - 0x00FF: bootstrap
    0x0100 - 0x3FFF: cartridge
*/
struct MemoryBus {
    memory: [u8; 0xFFFF],
}

impl MemoryBus {
    fn read_byte(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self.memory[address as usize] = value;
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
}

impl CPU {
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
        match instruction {
            Instruction::ADD(target) => self.add(target),
            Instruction::JP(test) => self.jump(test),
            Instruction::LD(load_type) => self.load(load_type),
            Instruction::PUSH(target) => self.push(target),
            Instruction::POP(target) => self.pop(target),
            Instruction::CALL(test) => self.call(test),
            Instruction::RET(test) => self.ret(test),
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

fn main() {
    println!("Hello, world!");
}
