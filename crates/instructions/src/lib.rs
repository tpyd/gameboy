mod types;
mod display;

use types::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inc_test() {
        // Byte
        let instrs = Instruction::INC(ArithmeticType::Byte(ByteTarget::A));
        assert_eq!(format!("{instrs}"), "INC  A");
        let instrs = Instruction::INC(ArithmeticType::Byte(ByteTarget::B));
        assert_eq!(format!("{instrs}"), "INC  B");
        let instrs = Instruction::INC(ArithmeticType::Byte(ByteTarget::C));
        assert_eq!(format!("{instrs}"), "INC  C");
        let instrs = Instruction::INC(ArithmeticType::Byte(ByteTarget::D));
        assert_eq!(format!("{instrs}"), "INC  D");
        let instrs = Instruction::INC(ArithmeticType::Byte(ByteTarget::E));
        assert_eq!(format!("{instrs}"), "INC  E");
        let instrs = Instruction::INC(ArithmeticType::Byte(ByteTarget::H));
        assert_eq!(format!("{instrs}"), "INC  H");
        let instrs = Instruction::INC(ArithmeticType::Byte(ByteTarget::L));
        assert_eq!(format!("{instrs}"), "INC  L");
        let instrs = Instruction::INC(ArithmeticType::Byte(ByteTarget::D8));
        assert_eq!(format!("{instrs}"), "INC  D8");
        let instrs = Instruction::INC(ArithmeticType::Byte(ByteTarget::HLI));
        assert_eq!(format!("{instrs}"), "INC  [HL]");

        // Word
        let instrs = Instruction::INC(ArithmeticType::Word(WordTarget::BC));
        assert_eq!(format!("{instrs}"), "INC  BC");
        let instrs = Instruction::INC(ArithmeticType::Word(WordTarget::DE));
        assert_eq!(format!("{instrs}"), "INC  DE");
        let instrs = Instruction::INC(ArithmeticType::Word(WordTarget::HL));
        assert_eq!(format!("{instrs}"), "INC  HL");
        let instrs = Instruction::INC(ArithmeticType::Word(WordTarget::SP));
        assert_eq!(format!("{instrs}"), "INC  SP");
    }
}
