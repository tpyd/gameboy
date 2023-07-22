use crate::types::*;
use std::{fmt, fmt::Display}; // TODO dont use fmt::Display and Display


impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LD(loadtype) => {
                write!(f, "LD   {}", loadtype.to_string())?;
                Ok(())
            },
            &Instruction::INC(arithmetic_type) => {
                write!(f, "INC  {}", arithmetic_type.to_string())?;
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
            &ByteTarget::HLI => write!(f, "[HL]"),
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