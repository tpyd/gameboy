use std::collections::VecDeque;
use std::io::Write;
use std::path::Path;

use crate::register::Registers;
use crate::memory::MemoryBus;
use crate::ppu::Ppu;
use crate::interrupt::Interrupt;
use crate::instruction::*;

/*
    Implementation of the CPU LR35902
*/
pub struct Cpu {
    pub registers: Registers,
    pub pc: u16, // Program counter, tells which instruction its currently running
    pub sp: u16, // Stack pointer, points to the top of the stack
    pub bus: MemoryBus,
    pub ppu: Ppu,
    div_timer: u32,
    tima_timer: u32,
    ei_called: bool, // Whether to enable interrupt after instruction
    is_halted: bool,
    ime: bool, // Interrupt Master Enable
    instruction_history: VecDeque<String>,
    file: std::fs::File,
}

impl Cpu {
    pub fn with_rom(rom_path: &str) -> Self {
        let path = Path::new(rom_path);

        // Have to recreate Ppu as well to recreate memory reference properly
        let memory_bus = MemoryBus::new(Some(&path));
        let ppu = Ppu::new(memory_bus.get_mem_ref());

        Cpu {
            bus: memory_bus,
            ppu: ppu,
            ..Default::default()
        }
    }

    // Run the gameboy for 17 556 clocks. Which equals to one screen update.
    pub fn run(&mut self) {
        let mut cycles = 0;

        // Go through all the lines in the screen
        for ly in 0..144 {
            cycles = 0;

            // OAM Search
            self.ppu.oam_search(ly);
            while cycles < 80 {
                cycles += self.step();
            }

            // Pixel transfer
            self.ppu.pixel_transfer(ly);
            while cycles < 252 {
                cycles += self.step();
            }

            // Horizontal blank period
            while cycles < 456 {
                cycles += self.step();
            }
        }

        // Vertical blank period
        self.set_interrupt_flag(Interrupt::VBlank);
        for _ in 144..154 {
            while cycles < 4560 {
                cycles += self.step();
            }
        }
        self.unset_interrupt_flag(Interrupt::VBlank);
    }

    // Executes one CPU cycle. Reads and executes an instruction from the position of the pc and updates the pc
    // Returns the number of cycles performed
    pub fn step(&mut self) -> u32 {
        let mut instruction_byte = self.read_next_byte();
        let prefixed = instruction_byte == 0xCB;
        if prefixed {
            instruction_byte = self.read_next_byte();
        }

        // Lookup and execute next instruction and return size of instruction in bytes and how many cycles it took to execute
        let cycles = if let Some(instruction) = Instruction::from_byte(instruction_byte, prefixed) {
            self.instruction_history.push_front(format!("{:0>4X}: ${:0>2X}\t{:?}", self.pc, instruction_byte, instruction));
            self.instruction_history.truncate(100);
            //self.debug();

            let mut c = self.execute(instruction);

            // Enable interrupts if previous instruction call was EI
            if self.ei_called {
                // Do nothing if instruction was EI to delay with one instruction
                match Instruction::from_byte(instruction_byte, prefixed).unwrap() {
                    Instruction::EI => {},
                    _ => { self.ime = true; self.ei_called = false; },
                }
            }

            // Handle interrupts
            if self.ime {
                c += self.handle_interrupts();
            }

            c
        } else {
            let description = format!("0x{}{:x}", if prefixed { "cb" } else { "" }, instruction_byte);
            self.print_history();
            panic!("Unknown instruction found for: {}", description);
        };

        // Read serial port, used to debug with Blargg's test rom
        let serial_transfer_data = self.read_byte(0xFF01);      // SB
        let serial_transfer_control = self.read_byte(0xFF02);   // SC
        if serial_transfer_control == 0x81 {
            print!("{}", serial_transfer_data as char); // Maybe change to little endianess
            std::io::stdout().flush().unwrap();
            self.write_byte(0xFF02, 0x01);
        }

        self.update_timers(cycles);
        //self.log_state();

        cycles as u32
    }

    pub fn log_state(&mut self) {
        let pcmem1 = self.bus.read_byte(self.pc as usize);
        let pcmem2 = self.bus.read_byte((self.pc+1) as usize);
        let pcmem3 = self.bus.read_byte((self.pc+2) as usize);
        let pcmem4 = self.bus.read_byte((self.pc+3) as usize);

        let string = format!(
            "A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X}",
            self.registers.a, u8::from(self.registers.f), self.registers.b, self.registers.c, self.registers.d, self.registers.e, self.registers.h, self.registers.l,
            self.sp, self.pc, pcmem1, pcmem2, pcmem3, pcmem4
        );

        writeln!(self.file, "{}", string).unwrap();
    }

    // Prints debug information in the console
    #[allow(dead_code)]
    fn debug(&self) {
        println!("---------");
        println!("AF: 0x{:0>4X}\nBC: 0x{:0>4X}\nDE: 0x{:0>4X}\nHL: 0x{:0>4X}",
            self.registers.get_af(),
            self.registers.get_bc(),
            self.registers.get_de(),
            self.registers.get_hl());
        println!("---------");
        println!("SP: 0x{:0>4X}", self.sp);
        println!("PC: 0x{:0>4X}", self.pc-1);
        println!("---------");
        println!("{}", self.instruction_history.front().unwrap());
    }

    // Prints the instruction history
    fn print_history(&self) {
        for s in self.instruction_history.iter().rev() {
            println!("{}", s);
        }
    }

    // Handles interrupts
    fn handle_interrupts(&mut self) -> usize {
        // IE contains which iterrupts are enabled (set by the ROM)
        let interrupt_enable = self.read_byte(0xffff);
        let interrupt_flag = self.read_byte(0xff0f);

        let interrupt = Interrupt::get_prioritized_interrupt(interrupt_flag, interrupt_enable);

        let interrupt_vector = match interrupt {
            Interrupt::VBlank   => RSTVec::I40,
            Interrupt::LcdStat  => RSTVec::I48,
            Interrupt::Timer    => RSTVec::I50,
            Interrupt::Serial   => RSTVec::I58,
            Interrupt::Joypad   => RSTVec::I60,
            _ => return 20,
        };

        self.di();
        self.unset_interrupt_flag(interrupt);
        self.rst(interrupt_vector);

        20 // 5 M-cycles, includes rst call
    }

    // Sets the given interrupt flag in IF
    // This is usually set by the CPU automatically when appropriate
    // and when interrupts are enabled. ROMS would normally set the
    // IE bits instead to request an interrupt (0xffff).
    fn set_interrupt_flag(&mut self, interrupt_type: Interrupt) {
        let mut mem_ref = self.bus.base.borrow_mut();
        let interrupt_flag = &mem_ref[0xff0f];

        let new_if: u8;
        match interrupt_type {
            Interrupt::VBlank => new_if = (1 << 0) | interrupt_flag,
            Interrupt::LcdStat => new_if = (1 << 1) | interrupt_flag,
            Interrupt::Timer => new_if = (1 << 2) | interrupt_flag,
            Interrupt::Serial => new_if = (1 << 3) | interrupt_flag,
            Interrupt::Joypad => new_if = (1 << 4) | interrupt_flag,
            _ => return,
        }

        mem_ref[0xff0f] = new_if;
    }

    // Unsets the given interrupt flag in IF
    fn unset_interrupt_flag(&mut self, interrupt_type: Interrupt) {
        let mut mem_ref = self.bus.base.borrow_mut();
        let interrupt_flag = &mem_ref[0xff0f];

        let new_if: u8;
        match interrupt_type {
            Interrupt::VBlank => new_if = !(1 << 0) & interrupt_flag,
            Interrupt::LcdStat => new_if = !(1 << 1) & interrupt_flag,
            Interrupt::Timer => new_if = !(1 << 2) & interrupt_flag,
            Interrupt::Serial => new_if = !(1 << 3) & interrupt_flag,
            Interrupt::Joypad => new_if = !(1 << 4) & interrupt_flag,
            _ => return,
        }

        mem_ref[0xff0f] = new_if;
    }

    // Increments the timer register div - divider register if neccessary
    fn update_timers(&mut self, cycles: usize) {
        self.div_timer += cycles as u32;
        self.tima_timer += cycles as u32;  // TODO undefined overflow

        let mut mem_ref = self.bus.base.borrow_mut();

        // Update div timer
        if self.div_timer >= 16384 {
            let timer = mem_ref[0xff04];
            let new_val = timer.wrapping_add(1);
            mem_ref[0xff04] = new_val;
            self.div_timer %= 16384;
        }

        // Update tima timer
        let tac = mem_ref[0xff07];
        let timer_enable = (1 << 2) & tac != 0;
        if timer_enable {
            let bit1 = (1 << 0) & tac != 0;
            let bit0 = (1 << 1) & tac != 0;
            let clock_inc = match (bit1, bit0) {
                (false, false) => 4096,   // Cpu clock / 1024
                (false, true) => 262144, // Cpu clock / 16
                (true, false) => 65536,  // Cpu clock / 64
                (true, true) => 16384,  // Cpu clock / 256
            };

            let tma = mem_ref[0xff06];

            // Check if increment is needed
            if self.tima_timer >= clock_inc {
                let timer = mem_ref[0xff05];
                let (new_val, overflow) = timer.overflowing_add(1);
                if overflow {
                    // Set register to TMA register and request interrupt
                    mem_ref[0xff05] = tma;
                    drop(mem_ref); // Need to drop memory reference since its immutable apparently
                    self.set_interrupt_flag(Interrupt::Timer);
                } else {
                    mem_ref[0xff05] = new_val;
                }

                // Reset internal timer
                self.tima_timer %= clock_inc;
            }
        }
    }

    // Resets the timer register div - divider register
    fn reset_div(&mut self) {
        let mut mem_ref = self.bus.base.borrow_mut();
        mem_ref[0xff04] = 0;
    }

    // Executes a given instruction on the CPU. Every instruction execution controls pc movement by itself.
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
            Instruction::RLA => { self.rotate(ByteTarget::A, true, true, false); 4 },
            Instruction::RLCA => { self.rotate(ByteTarget::A, false, true, false); 4 },
            Instruction::RRA => { self.rotate(ByteTarget::A, true, false, false); 4 },
            Instruction::RRCA => { self.rotate(ByteTarget::A, false, false, false); 4 },

            Instruction::RLC(target) => self.rotate(target, false, true, true),
            Instruction::RL(target) => self.rotate(target, true, true, true),
            Instruction::RRC(target) => self.rotate(target, false, false, true),
            Instruction::RR(target) => self.rotate(target, true, false, true),
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
            Instruction::NOP => 4,
            Instruction::HALT => { self.is_halted = true; 4 },
            Instruction::STOP => self.stop(),
        }
    }

    /*
        ADD and ADC instruction.
        Reads the current value from target (register or memory value). Adds the
        value to register A or HL depending on instruction overflowing if necessary.
    */
    fn add(&mut self, target: ArithmeticType, add_carry: bool) -> usize {
        let mut cycles = 4; // Default number of cycles for ADD instruction
        match target {
            ArithmeticType::Byte(byte_target) => {
                let source_value = match byte_target {
                    ByteTarget::A => self.registers.a,
                    ByteTarget::B => self.registers.b,
                    ByteTarget::C => self.registers.c,
                    ByteTarget::D => self.registers.d,
                    ByteTarget::E => self.registers.e,
                    ByteTarget::H => self.registers.h,
                    ByteTarget::L => self.registers.l,
                    ByteTarget::D8 => { cycles = 8; self.read_next_byte() },
                    ByteTarget::HLI => { cycles = 8; self.read_byte(self.registers.get_hl()) },
                };

                let (mut new_value, mut add_overflow) = self.registers.a.overflowing_add(source_value);
 
                if add_carry {
                    let (new_adc_value, adc_overflow) = new_value.overflowing_add(self.registers.f.carry as u8);
                    
                    new_value = new_adc_value;
                    add_overflow |= adc_overflow;
                }
                
                // Half carry is set if bit 3 overflows
                let half_carry = (((self.registers.a & 0xf) + (source_value & 0xf) + ((add_carry & self.registers.f.carry) as u8)) & 0x10) == 0x10;
                
                self.registers.a = new_value;

                self.registers.f.zero = new_value == 0;
                self.registers.f.carry = add_overflow;
                self.registers.f.half_carry = half_carry;


            },
            ArithmeticType::Word(word_target) => {
                cycles = 8;
                let mut value = match word_target {
                    WordTarget::BC => self.registers.get_bc(),
                    WordTarget::DE => self.registers.get_de(),
                    WordTarget::HL => self.registers.get_hl(),
                    WordTarget::SP => self.sp,
                };

                if add_carry { value += self.registers.f.carry as u16; }
                let (new_value, did_overflow) = self.registers.get_hl().overflowing_add(value);

                // Half-carry is set if bit 11 overflows
                self.registers.f.carry = did_overflow;
                self.registers.f.half_carry = (self.registers.get_hl() & 0x0FFF) + (value & 0x0FFF) > 0x0FFF;

                self.registers.set_hl(new_value)
            },
            ArithmeticType::SP => {
                cycles = 16;
                let mut value = self.read_next_byte() as i8 as u16;
                if add_carry { value += self.registers.f.carry as u16; }
                let (new_value, _) = self.sp.overflowing_add(value);

                // Instruction sets carry bit if overflow occured on bit 7, so did_overflow can't be used here
                self.registers.f.zero = false;
                self.registers.f.subtract = false;
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
        let source_value = match target {
            ByteTarget::A => self.registers.a,
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::D8 => { cycles = 8; self.read_next_byte() },
            ByteTarget::HLI => { cycles = 8; self.read_byte(self.registers.get_hl()) },
        };
        let (mut new_value, mut did_overflow) = self.registers.a.overflowing_sub(source_value);
        let mut half_carry = (source_value & 0x0F) > (self.registers.a & 0x0F);

        if carry {
            let (sbc_value, did_overflow2) = new_value.overflowing_sub(self.registers.f.carry as u8);
            new_value = sbc_value;
            did_overflow |= did_overflow2;
            half_carry = (source_value & 0x0F) + self.registers.f.carry as u8 > (self.registers.a & 0x0F);
        }

        // Carry bit is set if subtraction has to borrow
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = did_overflow;
        self.registers.f.half_carry = half_carry;

        self.registers.a = new_value;

        cycles
    }

    /*
        INC instruction
        Increments target register by 1
    */
    fn inc(&mut self, target: ArithmeticType) -> usize {
        let mut cycles = 4;
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
                        cycles = 12;
                        let value = self.read_byte(self.registers.get_hl()).wrapping_add(1);
                        self.write_byte(self.registers.get_hl(), value);
                        value
                    },
                    _ => panic!("Got invalid INC instruction.")
                };

                self.registers.f.zero = new_value == 0;
                self.registers.f.subtract = false;
                self.registers.f.half_carry = (new_value.wrapping_sub(1) & 0x0F) == 0x0F;
            },
            ArithmeticType::Word(word_target) => {
                cycles = 8;
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
        let mut cycles = 4;
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
                        cycles = 12;
                        let value = self.read_byte(self.registers.get_hl()).wrapping_sub(1);
                        self.write_byte(self.registers.get_hl(), value);
                        value
                    },
                    _ => panic!("Got invalid DEC instruction")
                };

                self.registers.f.zero = new_value == 0;
                self.registers.f.subtract = true;
                self.registers.f.half_carry = (new_value.wrapping_add(1) & 0x0F) == 0;
            },
            ArithmeticType::Word(word_target) => {
                cycles = 8;
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
        let source_value = match target {
            ByteTarget::A => self.registers.a,
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::D8 => { cycles = 8; self.read_next_byte() },
            ByteTarget::HLI => { cycles = 8; self.read_byte(self.registers.get_hl()) },
        };
        self.registers.a &= source_value;

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
        let source_value = match target {
            ByteTarget::A => self.registers.a,
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::D8 => { cycles = 8; self.read_next_byte() },
            ByteTarget::HLI => { cycles = 8; self.read_byte(self.registers.get_hl()) },
        };

        self.registers.a ^= source_value;

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
        let source_value = match target {
            ByteTarget::A => self.registers.a,
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::D8 => { cycles = 8; self.read_next_byte() },
            ByteTarget::HLI => { cycles = 8; self.read_byte(self.registers.get_hl()) },
        };
        self.registers.a |= source_value;

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;

        cycles
    }

    /**
     * CP instruction
     * Subtracts the target value from A and sets flags. Doesn't store the result. Used for comparison.
     */
    fn cp(&mut self, target: ByteTarget) -> usize {
        let mut cycles = 4;
        let source_value = match target {
            ByteTarget::A => self.registers.a,
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::D8 => { cycles = 8; self.read_next_byte() },
            ByteTarget::HLI => { cycles = 8; self.read_byte(self.registers.get_hl()) },
        };
        let (new_value, carry) = self.registers.a.overflowing_sub(source_value);

        // Carry bit is set if subtraction has to borrow
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = carry;
        self.registers.f.half_carry = (source_value & 0xf) > (self.registers.a & 0xf);

        cycles
    }

    /**
     * Rotate instruction
     * Contains RL, RR, RLC, RLCA, RLA, RRC, RRCA and RRA
     * Rotates register left. Carry parameter specifies whether to rotate through carry or not
     */
    fn rotate(&mut self, target: ByteTarget, through_carry: bool, left: bool, set_zero: bool) -> usize {
        let mut cycles = 8;
        let source_value = match target {
            ByteTarget::A => { cycles = 4; self.registers.a },
            ByteTarget::B => self.registers.b,
            ByteTarget::C => self.registers.c,
            ByteTarget::D => self.registers.d,
            ByteTarget::E => self.registers.e,
            ByteTarget::H => self.registers.h,
            ByteTarget::L => self.registers.l,
            ByteTarget::HLI => { cycles = 16; self.read_byte(self.registers.get_hl()) },
            ByteTarget::D8 => panic!("Got invalid enum ByteTarget::D8 in rotate instruction"),
        };

        let mut new_value: u8;
        let carry: u8;

        if left {
            new_value = source_value.rotate_left(1);
            carry = new_value & 0x01;
        } else {
            new_value = source_value.rotate_right(1);
            carry = new_value & 0x80;
        }

        // Set wrapped bit to carry if instruction is 'through carry'
        if through_carry {
            // Toggle the bit only if it's different from the carry flag
            if left && new_value & 0x01 != self.registers.f.carry as u8 {
                new_value ^= 0x01;
            }
            if !left && new_value & 0x80 != (self.registers.f.carry as u8) << 7 {
                new_value ^= 0x80;
            }
        }

        match target {
            ByteTarget::A => self.registers.a = new_value,
            ByteTarget::B => self.registers.b = new_value,
            ByteTarget::C => self.registers.c = new_value,
            ByteTarget::D => self.registers.d = new_value,
            ByteTarget::E => self.registers.e = new_value,
            ByteTarget::H => self.registers.h = new_value,
            ByteTarget::L => self.registers.l = new_value,
            ByteTarget::HLI => self.write_byte(self.registers.get_hl(), new_value),
            _ => {},
        }

        self.registers.f.zero = if set_zero { new_value == 0 } else { false };
        self.registers.f.subtract = false;
        self.registers.f.carry = carry != 0;
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
            ByteTarget::HLI => { cycles = 16; self.read_byte(self.registers.get_hl()) },
            ByteTarget::D8 => panic!("Got invalid enum ByteTarget::D8 in shift instruction"),
        };

        let msb = source_value & 0x80;
        let lsb = source_value & 0x01;
        let c = if left { msb >> 7 } else { lsb };
        source_value = if left { source_value << 1 } else { source_value >> 1 };

        // Right arithmetic shift inserts msb insted of 0
        if !logic && !left {
            source_value |= msb;
        }

        match target {
            ByteTarget::A => self.registers.a = source_value,
            ByteTarget::B => self.registers.b = source_value,
            ByteTarget::C => self.registers.c = source_value,
            ByteTarget::D => self.registers.d = source_value,
            ByteTarget::E => self.registers.e = source_value,
            ByteTarget::H => self.registers.h = source_value,
            ByteTarget::L => self.registers.l = source_value,
            ByteTarget::HLI => self.write_byte(self.registers.get_hl(), source_value),
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
            ByteTarget::HLI => { cycles = 16; self.read_byte(self.registers.get_hl()) },
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
            ByteTarget::HLI => self.write_byte(self.registers.get_hl(), source_value),
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
            ByteTarget::HLI => { cycles = 12; self.read_byte(self.registers.get_hl()) },
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
            ByteTarget::HLI => { cycles = 16; self.read_byte(self.registers.get_hl()) },
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
            ByteTarget::HLI => self.write_byte(self.registers.get_hl(), source_value),
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
        let relative_dest = self.read_next_byte();

        // To clarify, an operand of 0 is equivalent to no jumping.
        if relative_dest == 0 {
            return cycles;
        }

        let relative_val = relative_dest as i8 as u16;

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
                    LoadByteSource::HLI => { cycles += 4; self.read_byte(self.registers.get_hl()) },
                };
                match target {
                    LoadByteTarget::A => self.registers.a = source_value,
                    LoadByteTarget::B => self.registers.b = source_value,
                    LoadByteTarget::C => self.registers.c = source_value,
                    LoadByteTarget::D => self.registers.d = source_value,
                    LoadByteTarget::E => self.registers.e = source_value,
                    LoadByteTarget::H => self.registers.h = source_value,
                    LoadByteTarget::L => self.registers.l = source_value,
                    LoadByteTarget::HLI => { cycles += 4; self.write_byte(self.registers.get_hl(), source_value) },
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
                        self.write_byte(word, (source_value & 0x00FF) as u8);
                        self.write_byte(word + 1, (source_value >> 8) as u8);
                        cycles = 20
                    }
                }
            },
            LoadType::AFromIndirect(source) => {
                cycles = 8;
                let source_value = match source {
                    LoadMemoryLocation::BC => self.read_byte(self.registers.get_bc()),
                    LoadMemoryLocation::DE => self.read_byte(self.registers.get_de()),
                    LoadMemoryLocation::HLinc => {
                        let value = self.read_byte(self.registers.get_hl());
                        self.registers.set_hl(self.registers.get_hl().wrapping_add(1));
                        value
                    },
                    LoadMemoryLocation::HLdec => {
                        let value = self.read_byte(self.registers.get_hl());
                        self.registers.set_hl(self.registers.get_hl().wrapping_sub(1));
                        value
                    }
                };
                self.registers.a = source_value;
            },
            LoadType::IndirectFromA(target) => {
                cycles = 8;
                let source_value = self.registers.a;
                match target {
                    LoadMemoryLocation::BC => self.write_byte(self.registers.get_bc(), source_value),
                    LoadMemoryLocation::DE => self.write_byte(self.registers.get_de(), source_value),
                    LoadMemoryLocation::HLinc => {
                        self.write_byte(self.registers.get_hl(), source_value);
                        self.registers.set_hl(self.registers.get_hl().wrapping_add(1));
                    },
                    LoadMemoryLocation::HLdec => {
                        self.write_byte(self.registers.get_hl(), source_value);
                        self.registers.set_hl(self.registers.get_hl().wrapping_sub(1));
                    },
                }
            },
            LoadType::AFromByteAddress(source) => {
                cycles = 8;
                let source_value = match source {
                    ByteAddress::D8 => {
                        cycles = 12;
                        let byte = self.read_next_byte();
                        self.read_byte(0xFF00 + byte as u16)
                    },
                    ByteAddress::C => self.read_byte(0xFF00 + self.registers.c as u16),
                    ByteAddress::D16 => { let word = self.read_next_word(); self.read_byte(word) },
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
                        self.write_byte(0xFF00 + byte as u16, source_value)
                    },
                    ByteAddress::C => self.write_byte(0xFF00 + self.registers.c as u16, source_value),
                    ByteAddress::D16 => { let word = self.read_next_word(); self.write_byte(word, source_value) },
                }
            },
            LoadType::HLFromSP => {
                cycles = 12;
                let next_byte = self.read_next_byte() as i8 as u16;
                let source_value = self.sp.wrapping_add(next_byte);
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
        self.write_byte(self.sp, ((value & 0xFF00) >> 8) as u8);

        self.sp = self.sp.wrapping_sub(1);
        self.write_byte(self.sp, (value & 0x00FF) as u8);
    }

    /*
        POP instruction
        Pops a value from the stack onto the target register
    */
    fn pop(&mut self, target: StackTarget) -> usize {
        let value = self.pop_value();
        match target {
            StackTarget::AF => self.registers.set_af(value),
            StackTarget::BC => self.registers.set_bc(value),
            StackTarget::DE => self.registers.set_de(value),
            StackTarget::HL => self.registers.set_hl(value),
        };

        12
    }

    // Separate function for popping a value from the stack
    fn pop_value(&mut self) -> u16 {
        let lsb = self.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        let msb = self.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        (msb << 8) | lsb
    }

    /*
        CALL instruction
        Calls a function by setting the pc to the address given by the next 2 bytes
    */
    fn call(&mut self, test: JumpCondition) -> usize {
        let mut cycles = 12;
        let new_pc = self.read_next_word();

        let jump_condition = match test {
            JumpCondition::NotZero => !self.registers.f.zero,
            JumpCondition::NotCarry => !self.registers.f.carry,
            JumpCondition::Zero => self.registers.f.zero,
            JumpCondition::Carry => self.registers.f.carry,
            JumpCondition::Always => true,
        };

        if jump_condition {
            cycles = 24;
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
        self.pc = vec as u16;

        16
    }

    /*
        DAA instruction
        Changes register A to contain a decimal number. Often after an arithmetic operation.
        For example: 15+27 = 3C, running DAA after changes it to 42.
    */
    fn daa(&mut self) -> usize {
        let mut adjustment = 0;

        if self.registers.f.subtract {
            if self.registers.f.half_carry {
                adjustment += 0x6;
            }

            if self.registers.f.carry {
                adjustment += 0x60;
            }

            self.registers.a = self.registers.a.wrapping_sub(adjustment);
        } else {
            if self.registers.f.half_carry || self.registers.a & 0xF > 0x9 {
                adjustment += 0x6;
            }

            if self.registers.f.carry || self.registers.a > 0x99 {
                adjustment += 0x60;
                self.registers.f.carry = true;
            }

            self.registers.a = self.registers.a.wrapping_add(adjustment);
        }

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.half_carry = false;

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
        self.registers.a = !self.registers.a;
        
        self.registers.f.subtract = true;
        self.registers.f.half_carry = true;

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
        // EI followed by DI will not allow for any interrupts between them.
        self.ei_called = false;
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
        self.reset_div();

        4
    }

    // Reads the next byte and increments the pc
    fn read_next_byte(&mut self) -> u8 {
        let next_byte = self.read_byte(self.pc);
        self.pc = self.pc.wrapping_add(1);
        next_byte
    }

    // Reads the next word  and increments the pc
    fn read_next_word(&mut self) -> u16 {
        let right = self.read_byte(self.pc) as u16;
        let left = (self.read_byte(self.pc.wrapping_add(1)) as u16) << 8;
        self.pc = self.pc.wrapping_add(2);
        left | right
    }

    // Read value from memory
    fn read_byte(&self, address: u16) -> u8 {
        let address = address as usize;
        self.bus.read_byte(address)
    }

    // Write value to memory
    fn write_byte(&mut self, address: u16, value: u8) {
        let address = address as usize;

        self.bus.write_byte(address, value);
        self.ppu.update(address);
    }
}

impl Default for Cpu {
    fn default() -> Self {
        let mem = MemoryBus::new(None);
        let ppu = Ppu::new(mem.get_mem_ref());

        let file = std::fs::File::create("log.txt")
            .unwrap();

        Cpu {
            registers: Registers::new(),
            pc: 0x0100,
            sp: 0xFFFE, // See https://gbdev.io/pandocs/#power-up-sequence
            bus: mem,
            ppu: ppu,
            div_timer: 0,
            tima_timer: 0,
            ei_called: false,
            is_halted: false,
            ime: true,
            instruction_history: VecDeque::new(),
            // debugger: Debugger::new(Path::new("TEMP_VALUE")),
            file: file,
        }
    }
}

