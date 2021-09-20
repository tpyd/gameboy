/*
    Allocates an array on the heap, filled with the given inital value.
*/
pub fn alloc_boxed_array<T: Clone + Default, const N: usize>() -> Box<[T; N]> {
    use std::convert::TryInto;

    let vec = vec![T::default(); N];
    let box_slice: Box<[T]> = vec.into();
    box_slice.try_into().ok().unwrap()
}

/**
 * Convert licensee code in the cartridge to a readable string
 */
pub fn get_licensee_name(code: u8) -> String {
    match code {
        0x00 => String::from("None"),
        0x01 => String::from("Nintendo R&D1"),
        0x08 => String::from("Capcom"),
        0x13 => String::from("Electronic Arts"),
        0x18 => String::from("Hudson Soft"),
        0x19 => String::from("b-ai"),
        0x20 => String::from("kss"),
        0x22 => String::from("pow"),
        0x24 => String::from("PCM Complete"),
        0x25 => String::from("san-x"),
        0x28 => String::from("Kemco Japan"),
        0x29 => String::from("seta"),
        0x30 => String::from("Viacom"),
        0x31 => String::from("Nintendo"),
        0x32 => String::from("Bandai"),
        0x33 => String::from("Ocean/Acclaim"),
        0x34 => String::from("Konami"),
        0x35 => String::from("Hector"),
        0x37 => String::from("Taito"),
        0x38 => String::from("Hudson"),
        0x39 => String::from("Banpresto"),
        0x41 => String::from("Ubi Soft"),
        0x42 => String::from("Atlus"),
        0x44 => String::from("Malibu"),
        0x46 => String::from("angel"),
        0x47 => String::from("Bullet-Proof"),
        0x49 => String::from("irem"),
        0x50 => String::from("Absolute"),
        0x51 => String::from("Acclaim"),
        0x52 => String::from("Activision"),
        0x53 => String::from("American sammy"),
        0x54 => String::from("Konami"),
        0x55 => String::from("Hi tech entertainment"),
        0x56 => String::from("LJN"),
        0x57 => String::from("Matchbox"),
        0x58 => String::from("Mattel"),
        0x59 => String::from("Milton Bradley"),
        0x60 => String::from("Titus"),
        0x61 => String::from("Virgin"),
        0x64 => String::from("LucasArts"),
        0x67 => String::from("Ocean"),
        0x69 => String::from("Electronic Arts"),
        0x70 => String::from("Infogrames"),
        0x71 => String::from("Interplay"),
        0x72 => String::from("Broderbund"),
        0x73 => String::from("sculptured"),
        0x75 => String::from("sci"),
        0x78 => String::from("THQ"),
        0x79 => String::from("Accolade"),
        0x80 => String::from("misawa"),
        0x83 => String::from("lozc"),
        0x86 => String::from("Tokuma Shoten Intermedia"),
        0x87 => String::from("Tsukuda Original"),
        0x91 => String::from("Chunsoft"),
        0x92 => String::from("Video system"),
        0x93 => String::from("Ocean/Acclaim"),
        0x95 => String::from("Varie"),
        0x96 => String::from("Yonezawa/s'pal"),
        0x97 => String::from("Kaneko"),
        0x99 => String::from("Pack in soft"),
        0xA4 => String::from("Konami (Yu-Gi-Oh!)"),
        _ => String::from("Undefined")
    }
}

/**
 * Gets the cartridge type and returns it as a String
 */
pub fn get_cartridge_type(code: u8) -> String {
    match code {
        0x00 =>	String::from("ROM ONLY"),
        0x01 =>	String::from("MBC1"),
        0x02 =>	String::from("MBC1+RAM"),
        0x03 =>	String::from("MBC1+RAM+BATTERY"),
        0x05 =>	String::from("MBC2"),
        0x06 =>	String::from("MBC2+BATTERY"),
        0x08 =>	String::from("ROM+RAM *"),
        0x09 =>	String::from("ROM+RAM+BATTERY *"),
        0x0B =>	String::from("MMM01"),
        0x0C =>	String::from("MMM01+RAM"),
        0x0D =>	String::from("MMM01+RAM+BATTERY"),
        0x0F =>	String::from("MBC3+TIMER+BATTERY"),
        0x10 =>	String::from("MBC3+TIMER+RAM+BATTERY **"),
        0x11 =>	String::from("MBC3"),
        0x12 =>	String::from("MBC3+RAM **"),
        0x13 =>	String::from("MBC3+RAM+BATTERY **"),
        0x19 =>	String::from("MBC5"),
        0x1A =>	String::from("MBC5+RAM"),
        0x1B =>	String::from("MBC5+RAM+BATTERY"),
        0x1C =>	String::from("MBC5+RUMBLE"),
        0x1D =>	String::from("MBC5+RUMBLE+RAM"),
        0x1E =>	String::from("MBC5+RUMBLE+RAM+BATTERY"),
        0x20 =>	String::from("MBC6"),
        0x22 =>	String::from("MBC7+SENSOR+RUMBLE+RAM+BATTERY"),
        0xFC =>	String::from("POCKET CAMERA"),
        0xFD =>	String::from("BANDAI TAMA5"),
        0xFE =>	String::from("HuC3"),
        0xFF =>	String::from("HuC1+RAM+BATTERY"),
        _ => String::from("Undefined")
    }
}