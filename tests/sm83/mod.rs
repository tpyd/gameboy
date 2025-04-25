use std::fs::File;

use serde::Deserialize;
use gameboy::cpu::Cpu;

#[derive(Deserialize)]
struct Test {
    name: String,
    initial: State,
    r#final: State
}

#[derive(Deserialize)]
struct State {
    pc: u16,
    sp: u16,
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    ram: Vec<Vec<u16>>,
}

fn test_instruction(file: &str) {
    let file = File::open(file).unwrap();
    let tests: Vec<Test> = serde_json::from_reader(file).unwrap();

    for test in tests {
        let mut cpu = Cpu::default();
        set_initial_state(&mut cpu, &test);
        cpu.step();
        verify(&cpu, &test);
    }
}

fn set_initial_state(cpu: &mut Cpu, test: &Test) {
    cpu.pc = test.initial.pc;
    cpu.sp = test.initial.sp;
    cpu.registers.a = test.initial.a;
    cpu.registers.b = test.initial.b;
    cpu.registers.c = test.initial.c;
    cpu.registers.d = test.initial.d;
    cpu.registers.e = test.initial.e;
    cpu.registers.f = test.initial.f.into();
    cpu.registers.h = test.initial.h;
    cpu.registers.l = test.initial.l;

    let ram_values: Vec<(usize, u8)> = test.initial.ram
        .iter()
        .map(|v| (v[0] as usize, v[1] as u8))
        .collect();

    for (address, value) in ram_values {
        cpu.bus.write_byte(address, value);
    }
}

fn verify(cpu: &Cpu, test: &Test) {
    assert_eq!(cpu.pc, test.r#final.pc, "pc is incorrect - test {}", test.name);
    assert_eq!(cpu.sp, test.r#final.sp, "sp is incorrecet - test {}", test.name);
    assert_eq!(cpu.registers.a, test.r#final.a, "a register is incorrect - test {}", test.name);
    assert_eq!(cpu.registers.b, test.r#final.b, "b register is incorrect - test {}", test.name);
    assert_eq!(cpu.registers.c, test.r#final.c, "c register is incorrect - test {}", test.name);
    assert_eq!(cpu.registers.d, test.r#final.d, "d register is incorrect - test {}", test.name);
    assert_eq!(cpu.registers.e, test.r#final.e, "e register is incorrect - test {}", test.name);
    assert_eq!(u8::from(cpu.registers.f), test.r#final.f, "flags register is incorrect - test {}", test.name);
    assert_eq!(cpu.registers.h, test.r#final.h, "h register is incorrect test {}", test.name);
    assert_eq!(cpu.registers.l, test.r#final.l, "l register is incorrect test {}", test.name);

    let ram_values: Vec<(usize, u8)> = test.r#final.ram
        .iter()
        .map(|v| (v[0] as usize, v[1] as u8))
        .collect();

    for (address, value) in ram_values {
        let ram_value = cpu.bus.read_byte(address);
        assert_eq!(ram_value, value, "ram value is incorrect - test {}", test.name)
    }
}

macro_rules! generate_tests {
    ($name:ident, $file:literal) => {
        #[test]
        fn $name() {
            test_instruction($file);
        }
    };
}

// generate_tests!(noop, "tests/sm83/data/00.json");
// generate_tests!(ld_bc_u16, "tests/sm83/data/01.json");
// generate_tests!(ld_bc_address_a, "tests/sm83/data/02.json");
// generate_tests!(inc_bc, "tests/sm83/data/03.json");
// generate_tests!(inc_b, "tests/sm83/data/04.json");
// generate_tests!(dec_b, "tests/sm83/data/05.json");
// generate_tests!(ld_b_u8, "tests/sm83/data/06.json");
// generate_tests!(rlca, "tests/sm83/data/07.json");
// generate_tests!(ld_u16_address_sp, "tests/sm83/data/08.json");
// generate_tests!(add_hl_bc, "tests/sm83/data/09.json");
// generate_tests!(ld_a_bc_address, "tests/sm83/data/0a.json");
// generate_tests!(dec_bc, "tests/sm83/data/0b.json");
// generate_tests!(inc_c, "tests/sm83/data/0c.json");
// generate_tests!(dec_c, "tests/sm83/data/0d.json");
// generate_tests!(ld_c_u8, "tests/sm83/data/0e.json");
// generate_tests!(rrca, "tests/sm83/data/0f.json");
//
// generate_tests!(stop, "tests/sm83/data/10.json");
// generate_tests!(ld_de_u16, "tests/sm83/data/11.json");
// generate_tests!(ld_de_address_a, "tests/sm83/data/12.json");
// generate_tests!(inc_de, "tests/sm83/data/13.json");
// generate_tests!(inc_d, "tests/sm83/data/14.json");
// generate_tests!(dec_d, "tests/sm83/data/15.json");
// generate_tests!(ld_d_u8, "tests/sm83/data/16.json");
// generate_tests!(rla, "tests/sm83/data/17.json");
// generate_tests!(jr_i8, "tests/sm83/data/18.json");
// generate_tests!(add_hl_de, "tests/sm83/data/19.json");
// generate_tests!(ld_a_de_address, "tests/sm83/data/1a.json");
// generate_tests!(dec_de, "tests/sm83/data/1b.json");
// generate_tests!(inc_e, "tests/sm83/data/1c.json");
// generate_tests!(dec_e, "tests/sm83/data/1d.json");
// generate_tests!(ld_e_u8, "tests/sm83/data/1e.json");
// generate_tests!(rra, "tests/sm83/data/1f.json");
//
// generate_tests!(jr_nz_i8, "tests/sm83/data/20.json");
// generate_tests!(ld_hl_u16, "tests/sm83/data/21.json");
// generate_tests!(ld_hlp_address_a, "tests/sm83/data/22.json");
// generate_tests!(inc_hl, "tests/sm83/data/23.json");
// generate_tests!(inc_h, "tests/sm83/data/24.json");
// generate_tests!(dec_h, "tests/sm83/data/25.json");
// generate_tests!(ld_h_u8, "tests/sm83/data/26.json");
// generate_tests!(daa, "tests/sm83/data/27.json");
// generate_tests!(jr_z_i8, "tests/sm83/data/28.json");
// generate_tests!(add_hl_hl, "tests/sm83/data/29.json");
// generate_tests!(ld_a_hlp_address, "tests/sm83/data/2a.json");
// generate_tests!(dec_hl, "tests/sm83/data/2b.json");
// generate_tests!(inc_l, "tests/sm83/data/2c.json");
// generate_tests!(dec_l, "tests/sm83/data/2d.json");
// generate_tests!(ld_l_u8, "tests/sm83/data/2e.json");
// generate_tests!(cpl, "tests/sm83/data/2f.json");

generate_tests!(jr_nc_i8, "tests/sm83/data/30.json");
generate_tests!(ld_sp_u16, "tests/sm83/data/31.json");
generate_tests!(ld_hlm_address_a, "tests/sm83/data/32.json");
generate_tests!(inc_sp, "tests/sm83/data/33.json");
generate_tests!(inc_hl_address, "tests/sm83/data/34.json");
generate_tests!(dec_hl_address, "tests/sm83/data/35.json");
generate_tests!(ld_hl_address_u8, "tests/sm83/data/36.json");
generate_tests!(scf, "tests/sm83/data/37.json");
generate_tests!(jr_c_i8, "tests/sm83/data/38.json");
generate_tests!(add_hl_sp, "tests/sm83/data/39.json");
generate_tests!(ld_a_hlm_address, "tests/sm83/data/3a.json");
generate_tests!(dec_sp, "tests/sm83/data/3b.json");
generate_tests!(inc_a, "tests/sm83/data/3c.json");
generate_tests!(dec_a, "tests/sm83/data/3d.json");
generate_tests!(ld_a_u8, "tests/sm83/data/3e.json");
generate_tests!(ccf, "tests/sm83/data/3f.json");

