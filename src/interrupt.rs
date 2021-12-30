
pub enum Interrupt {
    VBlank,
    LcdStat,
    Timer,
    Serial,
    Joypad,
    None
}

impl Interrupt {
    // Get the first enabled interrupt that in the prioritized order
    pub fn get_prioritized_interrupt(byte: u8) -> Self {
        // Go through all interrupt bits
        for bit_index in 0..=4 {
            // Get the first enabled bit
            let bit_enabled = (byte >> bit_index) & 0b1 != 0;
            if bit_enabled {
                // Return the associated interrupt
                let interrupt = match bit_index {
                    0 => Interrupt::VBlank,
                    1 => Interrupt::LcdStat,
                    2 => Interrupt::Timer,
                    3 => Interrupt::Serial,
                    4 => Interrupt::Joypad,
                    _ => Interrupt::None, // Will never be reached
                };

                return interrupt;
            }
        }

        Interrupt::None
    }
}
