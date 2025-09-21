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
    let mut cpu;

    for test in tests {
        cpu = Cpu::default();
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

generate_tests!(noop, "tests/sm83/data/00.json");
generate_tests!(ld_bc_u16, "tests/sm83/data/01.json");
generate_tests!(ld_bc_address_a, "tests/sm83/data/02.json");
generate_tests!(inc_bc, "tests/sm83/data/03.json");
generate_tests!(inc_b, "tests/sm83/data/04.json");
generate_tests!(dec_b, "tests/sm83/data/05.json");
generate_tests!(ld_b_u8, "tests/sm83/data/06.json");
generate_tests!(rlca, "tests/sm83/data/07.json");
generate_tests!(ld_u16_address_sp, "tests/sm83/data/08.json");
generate_tests!(add_hl_bc, "tests/sm83/data/09.json");
generate_tests!(ld_a_bc_address, "tests/sm83/data/0a.json");
generate_tests!(dec_bc, "tests/sm83/data/0b.json");
generate_tests!(inc_c, "tests/sm83/data/0c.json");
generate_tests!(dec_c, "tests/sm83/data/0d.json");
generate_tests!(ld_c_u8, "tests/sm83/data/0e.json");
generate_tests!(rrca, "tests/sm83/data/0f.json");

generate_tests!(stop, "tests/sm83/data/10.json");
generate_tests!(ld_de_u16, "tests/sm83/data/11.json");
generate_tests!(ld_de_address_a, "tests/sm83/data/12.json");
generate_tests!(inc_de, "tests/sm83/data/13.json");
generate_tests!(inc_d, "tests/sm83/data/14.json");
generate_tests!(dec_d, "tests/sm83/data/15.json");
generate_tests!(ld_d_u8, "tests/sm83/data/16.json");
generate_tests!(rla, "tests/sm83/data/17.json");
generate_tests!(jr_i8, "tests/sm83/data/18.json");
generate_tests!(add_hl_de, "tests/sm83/data/19.json");
generate_tests!(ld_a_de_address, "tests/sm83/data/1a.json");
generate_tests!(dec_de, "tests/sm83/data/1b.json");
generate_tests!(inc_e, "tests/sm83/data/1c.json");
generate_tests!(dec_e, "tests/sm83/data/1d.json");
generate_tests!(ld_e_u8, "tests/sm83/data/1e.json");
generate_tests!(rra, "tests/sm83/data/1f.json");

generate_tests!(jr_nz_i8, "tests/sm83/data/20.json");
generate_tests!(ld_hl_u16, "tests/sm83/data/21.json");
generate_tests!(ld_hli_address_a, "tests/sm83/data/22.json");
generate_tests!(inc_hl, "tests/sm83/data/23.json");
generate_tests!(inc_h, "tests/sm83/data/24.json");
generate_tests!(dec_h, "tests/sm83/data/25.json");
generate_tests!(ld_h_u8, "tests/sm83/data/26.json");
generate_tests!(daa, "tests/sm83/data/27.json");
generate_tests!(jr_z_i8, "tests/sm83/data/28.json");
generate_tests!(add_hl_hl, "tests/sm83/data/29.json");
generate_tests!(ld_a_hli_address, "tests/sm83/data/2a.json");
generate_tests!(dec_hl, "tests/sm83/data/2b.json");
generate_tests!(inc_l, "tests/sm83/data/2c.json");
generate_tests!(dec_l, "tests/sm83/data/2d.json");
generate_tests!(ld_l_u8, "tests/sm83/data/2e.json");
generate_tests!(cpl, "tests/sm83/data/2f.json");

generate_tests!(jr_nc_i8, "tests/sm83/data/30.json");
generate_tests!(ld_sp_u16, "tests/sm83/data/31.json");
generate_tests!(ld_hld_address_a, "tests/sm83/data/32.json");
generate_tests!(inc_sp, "tests/sm83/data/33.json");
generate_tests!(inc_hl_address, "tests/sm83/data/34.json");
generate_tests!(dec_hl_address, "tests/sm83/data/35.json");
generate_tests!(ld_hl_address_u8, "tests/sm83/data/36.json");
generate_tests!(scf, "tests/sm83/data/37.json");
generate_tests!(jr_c_i8, "tests/sm83/data/38.json");
generate_tests!(add_hl_sp, "tests/sm83/data/39.json");
generate_tests!(ld_a_hld_address, "tests/sm83/data/3a.json");
generate_tests!(dec_sp, "tests/sm83/data/3b.json");
generate_tests!(inc_a, "tests/sm83/data/3c.json");
generate_tests!(dec_a, "tests/sm83/data/3d.json");
generate_tests!(ld_a_u8, "tests/sm83/data/3e.json");
generate_tests!(ccf, "tests/sm83/data/3f.json");

generate_tests!(ld_b_b, "tests/sm83/data/40.json");
generate_tests!(ld_b_c, "tests/sm83/data/41.json");
generate_tests!(ld_b_d, "tests/sm83/data/42.json");
generate_tests!(ld_b_e, "tests/sm83/data/43.json");
generate_tests!(ld_b_h, "tests/sm83/data/44.json");
generate_tests!(ld_b_l, "tests/sm83/data/45.json");
generate_tests!(ld_b_hl_address, "tests/sm83/data/46.json");
generate_tests!(ld_b_a, "tests/sm83/data/47.json");
generate_tests!(ld_c_b, "tests/sm83/data/48.json");
generate_tests!(ld_c_c, "tests/sm83/data/49.json");
generate_tests!(ld_c_d, "tests/sm83/data/4a.json");
generate_tests!(ld_c_e, "tests/sm83/data/4b.json");
generate_tests!(ld_c_h, "tests/sm83/data/4c.json");
generate_tests!(ld_c_l, "tests/sm83/data/4d.json");
generate_tests!(ld_c_hl_address, "tests/sm83/data/4e.json");
generate_tests!(ld_c_a, "tests/sm83/data/4f.json");

generate_tests!(ld_d_b, "tests/sm83/data/50.json");
generate_tests!(ld_d_c, "tests/sm83/data/51.json");
generate_tests!(ld_d_d, "tests/sm83/data/52.json");
generate_tests!(ld_d_e, "tests/sm83/data/53.json");
generate_tests!(ld_d_h, "tests/sm83/data/54.json");
generate_tests!(ld_d_l, "tests/sm83/data/55.json");
generate_tests!(ld_d_hl_address, "tests/sm83/data/56.json");
generate_tests!(ld_d_a, "tests/sm83/data/57.json");
generate_tests!(ld_e_b, "tests/sm83/data/58.json");
generate_tests!(ld_e_c, "tests/sm83/data/59.json");
generate_tests!(ld_e_d, "tests/sm83/data/5a.json");
generate_tests!(ld_e_e, "tests/sm83/data/5b.json");
generate_tests!(ld_e_h, "tests/sm83/data/5c.json");
generate_tests!(ld_e_l, "tests/sm83/data/5d.json");
generate_tests!(ld_e_hl_address, "tests/sm83/data/5e.json");
generate_tests!(ld_e_a, "tests/sm83/data/5f.json");

generate_tests!(ld_h_b, "tests/sm83/data/60.json");
generate_tests!(ld_h_c, "tests/sm83/data/61.json");
generate_tests!(ld_h_d, "tests/sm83/data/62.json");
generate_tests!(ld_h_e, "tests/sm83/data/63.json");
generate_tests!(ld_h_h, "tests/sm83/data/64.json");
generate_tests!(ld_h_l, "tests/sm83/data/65.json");
generate_tests!(ld_h_hl_address, "tests/sm83/data/66.json");
generate_tests!(ld_h_a, "tests/sm83/data/67.json");
generate_tests!(ld_l_b, "tests/sm83/data/68.json");
generate_tests!(ld_l_c, "tests/sm83/data/69.json");
generate_tests!(ld_l_d, "tests/sm83/data/6a.json");
generate_tests!(ld_l_e, "tests/sm83/data/6b.json");
generate_tests!(ld_l_h, "tests/sm83/data/6c.json");
generate_tests!(ld_l_l, "tests/sm83/data/6d.json");
generate_tests!(ld_l_hl_address, "tests/sm83/data/6e.json");
generate_tests!(ld_l_a, "tests/sm83/data/6f.json");

generate_tests!(ld_hl_address_b, "tests/sm83/data/70.json");
generate_tests!(ld_hl_address_c, "tests/sm83/data/71.json");
generate_tests!(ld_hl_address_d, "tests/sm83/data/72.json");
generate_tests!(ld_hl_address_e, "tests/sm83/data/73.json");
generate_tests!(ld_hl_address_h, "tests/sm83/data/74.json");
generate_tests!(ld_hl_address_l, "tests/sm83/data/75.json");
generate_tests!(halt, "tests/sm83/data/76.json");
generate_tests!(ld_hl_address_a, "tests/sm83/data/77.json");
generate_tests!(ld_a_b, "tests/sm83/data/78.json");
generate_tests!(ld_a_c, "tests/sm83/data/79.json");
generate_tests!(ld_a_d, "tests/sm83/data/7a.json");
generate_tests!(ld_a_e, "tests/sm83/data/7b.json");
generate_tests!(ld_a_h, "tests/sm83/data/7c.json");
generate_tests!(ld_a_l, "tests/sm83/data/7d.json");
generate_tests!(ld_a_hl_address, "tests/sm83/data/7e.json");
generate_tests!(ld_a_a, "tests/sm83/data/7f.json");

generate_tests!(add_a_b, "tests/sm83/data/80.json");
generate_tests!(add_a_c, "tests/sm83/data/81.json");
generate_tests!(add_a_d, "tests/sm83/data/82.json");
generate_tests!(add_a_e, "tests/sm83/data/83.json");
generate_tests!(add_a_h, "tests/sm83/data/84.json");
generate_tests!(add_a_l, "tests/sm83/data/85.json");
generate_tests!(add_a_hl_address, "tests/sm83/data/86.json");
generate_tests!(add_a_a, "tests/sm83/data/87.json");
generate_tests!(adc_a_b, "tests/sm83/data/88.json");
generate_tests!(adc_a_c, "tests/sm83/data/89.json");
generate_tests!(adc_a_d, "tests/sm83/data/8a.json");
generate_tests!(adc_a_e, "tests/sm83/data/8b.json");
generate_tests!(adc_a_h, "tests/sm83/data/8c.json");
generate_tests!(adc_a_l, "tests/sm83/data/8d.json");
generate_tests!(adc_a_hl_address, "tests/sm83/data/8e.json");
generate_tests!(adc_a_a, "tests/sm83/data/8f.json");

generate_tests!(sub_a_b, "tests/sm83/data/90.json");
generate_tests!(sub_a_c, "tests/sm83/data/91.json");
generate_tests!(sub_a_d, "tests/sm83/data/92.json");
generate_tests!(sub_a_e, "tests/sm83/data/93.json");
generate_tests!(sub_a_h, "tests/sm83/data/94.json");
generate_tests!(sub_a_l, "tests/sm83/data/95.json");
generate_tests!(sub_a_hl_address, "tests/sm83/data/96.json");
generate_tests!(sub_a_a, "tests/sm83/data/97.json");
generate_tests!(sbc_a_b, "tests/sm83/data/98.json");
generate_tests!(sbc_a_c, "tests/sm83/data/99.json");
generate_tests!(sbc_a_d, "tests/sm83/data/9a.json");
generate_tests!(sbc_a_e, "tests/sm83/data/9b.json");
generate_tests!(sbc_a_h, "tests/sm83/data/9c.json");
generate_tests!(sbc_a_l, "tests/sm83/data/9d.json");
generate_tests!(sbc_a_hl_address, "tests/sm83/data/9e.json");
generate_tests!(sbc_a_a, "tests/sm83/data/9f.json");

generate_tests!(and_a_b, "tests/sm83/data/a0.json");
generate_tests!(and_a_c, "tests/sm83/data/a1.json");
generate_tests!(and_a_d, "tests/sm83/data/a2.json");
generate_tests!(and_a_e, "tests/sm83/data/a3.json");
generate_tests!(and_a_h, "tests/sm83/data/a4.json");
generate_tests!(and_a_l, "tests/sm83/data/a5.json");
generate_tests!(and_a_hl_address, "tests/sm83/data/a6.json");
generate_tests!(and_a_a, "tests/sm83/data/a7.json");
generate_tests!(xor_a_b, "tests/sm83/data/a8.json");
generate_tests!(xor_a_c, "tests/sm83/data/a9.json");
generate_tests!(xor_a_d, "tests/sm83/data/aa.json");
generate_tests!(xor_a_e, "tests/sm83/data/ab.json");
generate_tests!(xor_a_h, "tests/sm83/data/ac.json");
generate_tests!(xor_a_l, "tests/sm83/data/ad.json");
generate_tests!(xor_a_hl_address, "tests/sm83/data/ae.json");
generate_tests!(xor_a_a, "tests/sm83/data/af.json");

generate_tests!(or_a_b, "tests/sm83/data/b0.json");
generate_tests!(or_a_c, "tests/sm83/data/b1.json");
generate_tests!(or_a_d, "tests/sm83/data/b2.json");
generate_tests!(or_a_e, "tests/sm83/data/b3.json");
generate_tests!(or_a_h, "tests/sm83/data/b4.json");
generate_tests!(or_a_l, "tests/sm83/data/b5.json");
generate_tests!(or_a_hl_address, "tests/sm83/data/b6.json");
generate_tests!(or_a_a, "tests/sm83/data/b7.json");
generate_tests!(cp_a_b, "tests/sm83/data/b8.json");
generate_tests!(cp_a_c, "tests/sm83/data/b9.json");
generate_tests!(cp_a_d, "tests/sm83/data/ba.json");
generate_tests!(cp_a_e, "tests/sm83/data/bb.json");
generate_tests!(cp_a_h, "tests/sm83/data/bc.json");
generate_tests!(cp_a_l, "tests/sm83/data/bd.json");
generate_tests!(cp_a_hl_address, "tests/sm83/data/be.json");
generate_tests!(cp_a_a, "tests/sm83/data/bf.json");

generate_tests!(ret_nz, "tests/sm83/data/c0.json");
generate_tests!(pop_bc, "tests/sm83/data/c1.json");
generate_tests!(jp_nz_u16, "tests/sm83/data/c2.json");
generate_tests!(jp_u16, "tests/sm83/data/c3.json");
generate_tests!(call_nz_u16, "tests/sm83/data/c4.json");
generate_tests!(push_bc, "tests/sm83/data/c5.json");
generate_tests!(add_a_u8, "tests/sm83/data/c6.json");
generate_tests!(rst_00, "tests/sm83/data/c7.json");
generate_tests!(ret_z, "tests/sm83/data/c8.json");
generate_tests!(ret, "tests/sm83/data/c9.json");
generate_tests!(jp_z_u16, "tests/sm83/data/ca.json");
generate_tests!(call_z_u16, "tests/sm83/data/cc.json");
generate_tests!(call_u16, "tests/sm83/data/cd.json");
generate_tests!(adc_a_u8, "tests/sm83/data/ce.json");
generate_tests!(rst_08, "tests/sm83/data/cf.json");

generate_tests!(ret_nc, "tests/sm83/data/d0.json");
generate_tests!(pop_de, "tests/sm83/data/d1.json");
generate_tests!(jp_nc_u16, "tests/sm83/data/d2.json");
generate_tests!(call_nc_u16, "tests/sm83/data/d4.json");
generate_tests!(push_de, "tests/sm83/data/d5.json");
generate_tests!(sub_a_u8, "tests/sm83/data/d6.json");
generate_tests!(rst_10, "tests/sm83/data/d7.json");
generate_tests!(ret_c, "tests/sm83/data/d8.json");
generate_tests!(reti, "tests/sm83/data/d9.json");
generate_tests!(jp_c_u16, "tests/sm83/data/da.json");
generate_tests!(call_c_u16, "tests/sm83/data/dc.json");
generate_tests!(sbc_a_u8, "tests/sm83/data/de.json");
generate_tests!(rst_18, "tests/sm83/data/df.json");

generate_tests!(ld_u8_stack_a, "tests/sm83/data/e0.json");
generate_tests!(pop_hl, "tests/sm83/data/e1.json");
generate_tests!(ld_c_stack_a, "tests/sm83/data/e2.json");
generate_tests!(push_hl, "tests/sm83/data/e5.json");
generate_tests!(and_a_u8, "tests/sm83/data/e6.json");
generate_tests!(rst_20, "tests/sm83/data/e7.json");
generate_tests!(add_sp_i8, "tests/sm83/data/e8.json");
generate_tests!(jp_hl, "tests/sm83/data/e9.json");
generate_tests!(ld_u16_address_a, "tests/sm83/data/ea.json");
generate_tests!(xor_a_u8, "tests/sm83/data/ee.json");
generate_tests!(rst_28, "tests/sm83/data/ef.json");

generate_tests!(ld_a_u8_stack, "tests/sm83/data/f0.json");
generate_tests!(pop_af, "tests/sm83/data/f1.json");
generate_tests!(ld_a_c_stack, "tests/sm83/data/f2.json");
generate_tests!(di, "tests/sm83/data/f3.json");
generate_tests!(push_af, "tests/sm83/data/f5.json");
generate_tests!(or_a_u8, "tests/sm83/data/f6.json");
generate_tests!(rst_30, "tests/sm83/data/f7.json");
generate_tests!(ld_hl_sp_i8, "tests/sm83/data/f8.json");
generate_tests!(ld_sp_hl, "tests/sm83/data/f9.json");
generate_tests!(ld_a_u16_address, "tests/sm83/data/fa.json");
generate_tests!(ei, "tests/sm83/data/fb.json");
generate_tests!(cp_a_u8, "tests/sm83/data/fe.json");
generate_tests!(rst_38, "tests/sm83/data/ff.json");

generate_tests!(rlc_b, "tests/sm83/data/cb 00.json");
generate_tests!(rlc_c, "tests/sm83/data/cb 01.json");
generate_tests!(rlc_d, "tests/sm83/data/cb 02.json");
generate_tests!(rlc_e, "tests/sm83/data/cb 03.json");
generate_tests!(rlc_h, "tests/sm83/data/cb 04.json");
generate_tests!(rlc_l, "tests/sm83/data/cb 05.json");
generate_tests!(rlc_hl_address, "tests/sm83/data/cb 06.json");
generate_tests!(rlc_a, "tests/sm83/data/cb 07.json");
generate_tests!(rrc_b, "tests/sm83/data/cb 08.json");
generate_tests!(rrc_c, "tests/sm83/data/cb 09.json");
generate_tests!(rrc_d, "tests/sm83/data/cb 0a.json");
generate_tests!(rrc_e, "tests/sm83/data/cb 0b.json");
generate_tests!(rrc_h, "tests/sm83/data/cb 0c.json");
generate_tests!(rrc_l, "tests/sm83/data/cb 0d.json");
generate_tests!(rrc_hl_address, "tests/sm83/data/cb 0e.json");
generate_tests!(rrc_a, "tests/sm83/data/cb 0f.json");

generate_tests!(rl_b, "tests/sm83/data/cb 10.json");
generate_tests!(rl_c, "tests/sm83/data/cb 11.json");
generate_tests!(rl_d, "tests/sm83/data/cb 12.json");
generate_tests!(rl_e, "tests/sm83/data/cb 13.json");
generate_tests!(rl_h, "tests/sm83/data/cb 14.json");
generate_tests!(rl_l, "tests/sm83/data/cb 15.json");
generate_tests!(rl_hl_address, "tests/sm83/data/cb 16.json");
generate_tests!(rl_a, "tests/sm83/data/cb 17.json");
generate_tests!(rr_b, "tests/sm83/data/cb 18.json");
generate_tests!(rr_c, "tests/sm83/data/cb 19.json");
generate_tests!(rr_d, "tests/sm83/data/cb 1a.json");
generate_tests!(rr_e, "tests/sm83/data/cb 1b.json");
generate_tests!(rr_h, "tests/sm83/data/cb 1c.json");
generate_tests!(rr_l, "tests/sm83/data/cb 1d.json");
generate_tests!(rr_hl_address, "tests/sm83/data/cb 1e.json");
generate_tests!(rr_a, "tests/sm83/data/cb 1f.json");

generate_tests!(sla_b, "tests/sm83/data/cb 20.json");
generate_tests!(sla_c, "tests/sm83/data/cb 21.json");
generate_tests!(sla_d, "tests/sm83/data/cb 22.json");
generate_tests!(sla_e, "tests/sm83/data/cb 23.json");
generate_tests!(sla_h, "tests/sm83/data/cb 24.json");
generate_tests!(sla_l, "tests/sm83/data/cb 25.json");
generate_tests!(sla_hl_address, "tests/sm83/data/cb 26.json");
generate_tests!(sla_a, "tests/sm83/data/cb 27.json");
generate_tests!(sra_b, "tests/sm83/data/cb 28.json");
generate_tests!(sra_c, "tests/sm83/data/cb 29.json");
generate_tests!(sra_d, "tests/sm83/data/cb 2a.json");
generate_tests!(sra_e, "tests/sm83/data/cb 2b.json");
generate_tests!(sra_h, "tests/sm83/data/cb 2c.json");
generate_tests!(sra_l, "tests/sm83/data/cb 2d.json");
generate_tests!(sra_hl_address, "tests/sm83/data/cb 2e.json");
generate_tests!(sra_a, "tests/sm83/data/cb 2f.json");

generate_tests!(swap_b, "tests/sm83/data/cb 30.json");
generate_tests!(swap_c, "tests/sm83/data/cb 31.json");
generate_tests!(swap_d, "tests/sm83/data/cb 32.json");
generate_tests!(swap_e, "tests/sm83/data/cb 33.json");
generate_tests!(swap_h, "tests/sm83/data/cb 34.json");
generate_tests!(swap_l, "tests/sm83/data/cb 35.json");
generate_tests!(swap_hl_address, "tests/sm83/data/cb 36.json");
generate_tests!(swap_a, "tests/sm83/data/cb 37.json");
generate_tests!(srl_b, "tests/sm83/data/cb 38.json");
generate_tests!(srl_c, "tests/sm83/data/cb 39.json");
generate_tests!(srl_d, "tests/sm83/data/cb 3a.json");
generate_tests!(srl_e, "tests/sm83/data/cb 3b.json");
generate_tests!(srl_h, "tests/sm83/data/cb 3c.json");
generate_tests!(srl_l, "tests/sm83/data/cb 3d.json");
generate_tests!(srl_hl_address, "tests/sm83/data/cb 3e.json");
generate_tests!(srl_a, "tests/sm83/data/cb 3f.json");

generate_tests!(bit_0_b, "tests/sm83/data/cb 40.json");
generate_tests!(bit_0_c, "tests/sm83/data/cb 41.json");
generate_tests!(bit_0_d, "tests/sm83/data/cb 42.json");
generate_tests!(bit_0_e, "tests/sm83/data/cb 43.json");
generate_tests!(bit_0_h, "tests/sm83/data/cb 44.json");
generate_tests!(bit_0_l, "tests/sm83/data/cb 45.json");
generate_tests!(bit_0_hl_address, "tests/sm83/data/cb 46.json");
generate_tests!(bit_0_a, "tests/sm83/data/cb 47.json");
generate_tests!(bit_1_b, "tests/sm83/data/cb 48.json");
generate_tests!(bit_1_c, "tests/sm83/data/cb 49.json");
generate_tests!(bit_1_d, "tests/sm83/data/cb 4a.json");
generate_tests!(bit_1_e, "tests/sm83/data/cb 4b.json");
generate_tests!(bit_1_h, "tests/sm83/data/cb 4c.json");
generate_tests!(bit_1_l, "tests/sm83/data/cb 4d.json");
generate_tests!(bit_1_hl_address, "tests/sm83/data/cb 4e.json");
generate_tests!(bit_1_a, "tests/sm83/data/cb 4f.json");

generate_tests!(bit_2_b, "tests/sm83/data/cb 50.json");
generate_tests!(bit_2_c, "tests/sm83/data/cb 51.json");
generate_tests!(bit_2_d, "tests/sm83/data/cb 52.json");
generate_tests!(bit_2_e, "tests/sm83/data/cb 53.json");
generate_tests!(bit_2_h, "tests/sm83/data/cb 54.json");
generate_tests!(bit_2_l, "tests/sm83/data/cb 55.json");
generate_tests!(bit_2_hl_address, "tests/sm83/data/cb 56.json");
generate_tests!(bit_2_a, "tests/sm83/data/cb 57.json");
generate_tests!(bit_3_b, "tests/sm83/data/cb 58.json");
generate_tests!(bit_3_c, "tests/sm83/data/cb 59.json");
generate_tests!(bit_3_d, "tests/sm83/data/cb 5a.json");
generate_tests!(bit_3_e, "tests/sm83/data/cb 5b.json");
generate_tests!(bit_3_h, "tests/sm83/data/cb 5c.json");
generate_tests!(bit_3_l, "tests/sm83/data/cb 5d.json");
generate_tests!(bit_3_hl_address, "tests/sm83/data/cb 5e.json");
generate_tests!(bit_3_a, "tests/sm83/data/cb 5f.json");

generate_tests!(bit_4_b, "tests/sm83/data/cb 60.json");
generate_tests!(bit_4_c, "tests/sm83/data/cb 61.json");
generate_tests!(bit_4_d, "tests/sm83/data/cb 62.json");
generate_tests!(bit_4_e, "tests/sm83/data/cb 63.json");
generate_tests!(bit_4_h, "tests/sm83/data/cb 64.json");
generate_tests!(bit_4_l, "tests/sm83/data/cb 65.json");
generate_tests!(bit_4_hl_address, "tests/sm83/data/cb 66.json");
generate_tests!(bit_4_a, "tests/sm83/data/cb 67.json");
generate_tests!(bit_5_b, "tests/sm83/data/cb 68.json");
generate_tests!(bit_5_c, "tests/sm83/data/cb 69.json");
generate_tests!(bit_5_d, "tests/sm83/data/cb 6a.json");
generate_tests!(bit_5_e, "tests/sm83/data/cb 6b.json");
generate_tests!(bit_5_h, "tests/sm83/data/cb 6c.json");
generate_tests!(bit_5_l, "tests/sm83/data/cb 6d.json");
generate_tests!(bit_5_hl_address, "tests/sm83/data/cb 6e.json");
generate_tests!(bit_5_a, "tests/sm83/data/cb 6f.json");

generate_tests!(bit_6_b, "tests/sm83/data/cb 70.json");
generate_tests!(bit_6_c, "tests/sm83/data/cb 71.json");
generate_tests!(bit_6_d, "tests/sm83/data/cb 72.json");
generate_tests!(bit_6_e, "tests/sm83/data/cb 73.json");
generate_tests!(bit_6_h, "tests/sm83/data/cb 74.json");
generate_tests!(bit_6_l, "tests/sm83/data/cb 75.json");
generate_tests!(bit_6_hl_address, "tests/sm83/data/cb 76.json");
generate_tests!(bit_6_a, "tests/sm83/data/cb 77.json");
generate_tests!(bit_7_b, "tests/sm83/data/cb 78.json");
generate_tests!(bit_7_c, "tests/sm83/data/cb 79.json");
generate_tests!(bit_7_d, "tests/sm83/data/cb 7a.json");
generate_tests!(bit_7_e, "tests/sm83/data/cb 7b.json");
generate_tests!(bit_7_h, "tests/sm83/data/cb 7c.json");
generate_tests!(bit_7_l, "tests/sm83/data/cb 7d.json");
generate_tests!(bit_7_hl_address, "tests/sm83/data/cb 7e.json");
generate_tests!(bit_7_a, "tests/sm83/data/cb 7f.json");

generate_tests!(res_0_b, "tests/sm83/data/cb 80.json");
generate_tests!(res_0_c, "tests/sm83/data/cb 81.json");
generate_tests!(res_0_d, "tests/sm83/data/cb 82.json");
generate_tests!(res_0_e, "tests/sm83/data/cb 83.json");
generate_tests!(res_0_h, "tests/sm83/data/cb 84.json");
generate_tests!(res_0_l, "tests/sm83/data/cb 85.json");
generate_tests!(res_0_hl_address, "tests/sm83/data/cb 86.json");
generate_tests!(res_0_a, "tests/sm83/data/cb 87.json");
generate_tests!(res_1_b, "tests/sm83/data/cb 88.json");
generate_tests!(res_1_c, "tests/sm83/data/cb 89.json");
generate_tests!(res_1_d, "tests/sm83/data/cb 8a.json");
generate_tests!(res_1_e, "tests/sm83/data/cb 8b.json");
generate_tests!(res_1_h, "tests/sm83/data/cb 8c.json");
generate_tests!(res_1_l, "tests/sm83/data/cb 8d.json");
generate_tests!(res_1_hl_address, "tests/sm83/data/cb 8e.json");
generate_tests!(res_1_a, "tests/sm83/data/cb 8f.json");

generate_tests!(res_2_b, "tests/sm83/data/cb 90.json");
generate_tests!(res_2_c, "tests/sm83/data/cb 91.json");
generate_tests!(res_2_d, "tests/sm83/data/cb 92.json");
generate_tests!(res_2_e, "tests/sm83/data/cb 93.json");
generate_tests!(res_2_h, "tests/sm83/data/cb 94.json");
generate_tests!(res_2_l, "tests/sm83/data/cb 95.json");
generate_tests!(res_2_hl_address, "tests/sm83/data/cb 96.json");
generate_tests!(res_2_a, "tests/sm83/data/cb 97.json");
generate_tests!(res_3_b, "tests/sm83/data/cb 98.json");
generate_tests!(res_3_c, "tests/sm83/data/cb 99.json");
generate_tests!(res_3_d, "tests/sm83/data/cb 9a.json");
generate_tests!(res_3_e, "tests/sm83/data/cb 9b.json");
generate_tests!(res_3_h, "tests/sm83/data/cb 9c.json");
generate_tests!(res_3_l, "tests/sm83/data/cb 9d.json");
generate_tests!(res_3_hl_address, "tests/sm83/data/cb 9e.json");
generate_tests!(res_3_a, "tests/sm83/data/cb 9f.json");

generate_tests!(res_4_b, "tests/sm83/data/cb a0.json");
generate_tests!(res_4_c, "tests/sm83/data/cb a1.json");
generate_tests!(res_4_d, "tests/sm83/data/cb a2.json");
generate_tests!(res_4_e, "tests/sm83/data/cb a3.json");
generate_tests!(res_4_h, "tests/sm83/data/cb a4.json");
generate_tests!(res_4_l, "tests/sm83/data/cb a5.json");
generate_tests!(res_4_hl_address, "tests/sm83/data/cb a6.json");
generate_tests!(res_4_a, "tests/sm83/data/cb a7.json");
generate_tests!(res_5_b, "tests/sm83/data/cb a8.json");
generate_tests!(res_5_c, "tests/sm83/data/cb a9.json");
generate_tests!(res_5_d, "tests/sm83/data/cb aa.json");
generate_tests!(res_5_e, "tests/sm83/data/cb ab.json");
generate_tests!(res_5_h, "tests/sm83/data/cb ac.json");
generate_tests!(res_5_l, "tests/sm83/data/cb ad.json");
generate_tests!(res_5_hl_address, "tests/sm83/data/cb ae.json");
generate_tests!(res_5_a, "tests/sm83/data/cb af.json");

generate_tests!(res_6_b, "tests/sm83/data/cb b0.json");
generate_tests!(res_6_c, "tests/sm83/data/cb b1.json");
generate_tests!(res_6_d, "tests/sm83/data/cb b2.json");
generate_tests!(res_6_e, "tests/sm83/data/cb b3.json");
generate_tests!(res_6_h, "tests/sm83/data/cb b4.json");
generate_tests!(res_6_l, "tests/sm83/data/cb b5.json");
generate_tests!(res_6_hl_address, "tests/sm83/data/cb b6.json");
generate_tests!(res_6_a, "tests/sm83/data/cb b7.json");
generate_tests!(res_7_b, "tests/sm83/data/cb b8.json");
generate_tests!(res_7_c, "tests/sm83/data/cb b9.json");
generate_tests!(res_7_d, "tests/sm83/data/cb ba.json");
generate_tests!(res_7_e, "tests/sm83/data/cb bb.json");
generate_tests!(res_7_h, "tests/sm83/data/cb bc.json");
generate_tests!(res_7_l, "tests/sm83/data/cb bd.json");
generate_tests!(res_7_hl_address, "tests/sm83/data/cb be.json");
generate_tests!(res_7_a, "tests/sm83/data/cb bf.json");

generate_tests!(set_0_b, "tests/sm83/data/cb c0.json");
generate_tests!(set_0_c, "tests/sm83/data/cb c1.json");
generate_tests!(set_0_d, "tests/sm83/data/cb c2.json");
generate_tests!(set_0_e, "tests/sm83/data/cb c3.json");
generate_tests!(set_0_h, "tests/sm83/data/cb c4.json");
generate_tests!(set_0_l, "tests/sm83/data/cb c5.json");
generate_tests!(set_0_hl_address, "tests/sm83/data/cb c6.json");
generate_tests!(set_0_a, "tests/sm83/data/cb c7.json");
generate_tests!(set_1_b, "tests/sm83/data/cb c8.json");
generate_tests!(set_1_c, "tests/sm83/data/cb c9.json");
generate_tests!(set_1_d, "tests/sm83/data/cb ca.json");
generate_tests!(set_1_e, "tests/sm83/data/cb cb.json");
generate_tests!(set_1_h, "tests/sm83/data/cb cc.json");
generate_tests!(set_1_l, "tests/sm83/data/cb cd.json");
generate_tests!(set_1_hl_address, "tests/sm83/data/cb ce.json");
generate_tests!(set_1_a, "tests/sm83/data/cb cf.json");

generate_tests!(set_2_b, "tests/sm83/data/cb d0.json");
generate_tests!(set_2_c, "tests/sm83/data/cb d1.json");
generate_tests!(set_2_d, "tests/sm83/data/cb d2.json");
generate_tests!(set_2_e, "tests/sm83/data/cb d3.json");
generate_tests!(set_2_h, "tests/sm83/data/cb d4.json");
generate_tests!(set_2_l, "tests/sm83/data/cb d5.json");
generate_tests!(set_2_hl_address, "tests/sm83/data/cb d6.json");
generate_tests!(set_2_a, "tests/sm83/data/cb d7.json");
generate_tests!(set_3_b, "tests/sm83/data/cb d8.json");
generate_tests!(set_3_c, "tests/sm83/data/cb d9.json");
generate_tests!(set_3_d, "tests/sm83/data/cb da.json");
generate_tests!(set_3_e, "tests/sm83/data/cb db.json");
generate_tests!(set_3_h, "tests/sm83/data/cb dc.json");
generate_tests!(set_3_l, "tests/sm83/data/cb dd.json");
generate_tests!(set_3_hl_address, "tests/sm83/data/cb de.json");
generate_tests!(set_3_a, "tests/sm83/data/cb df.json");

generate_tests!(set_4_b, "tests/sm83/data/cb e0.json");
generate_tests!(set_4_c, "tests/sm83/data/cb e1.json");
generate_tests!(set_4_d, "tests/sm83/data/cb e2.json");
generate_tests!(set_4_e, "tests/sm83/data/cb e3.json");
generate_tests!(set_4_h, "tests/sm83/data/cb e4.json");
generate_tests!(set_4_l, "tests/sm83/data/cb e5.json");
generate_tests!(set_4_hl_address, "tests/sm83/data/cb e6.json");
generate_tests!(set_4_a, "tests/sm83/data/cb e7.json");
generate_tests!(set_5_b, "tests/sm83/data/cb e8.json");
generate_tests!(set_5_c, "tests/sm83/data/cb e9.json");
generate_tests!(set_5_d, "tests/sm83/data/cb ea.json");
generate_tests!(set_5_e, "tests/sm83/data/cb eb.json");
generate_tests!(set_5_h, "tests/sm83/data/cb ec.json");
generate_tests!(set_5_l, "tests/sm83/data/cb ed.json");
generate_tests!(set_5_hl_address, "tests/sm83/data/cb ee.json");
generate_tests!(set_5_a, "tests/sm83/data/cb ef.json");

generate_tests!(set_6_b, "tests/sm83/data/cb f0.json");
generate_tests!(set_6_c, "tests/sm83/data/cb f1.json");
generate_tests!(set_6_d, "tests/sm83/data/cb f2.json");
generate_tests!(set_6_e, "tests/sm83/data/cb f3.json");
generate_tests!(set_6_h, "tests/sm83/data/cb f4.json");
generate_tests!(set_6_l, "tests/sm83/data/cb f5.json");
generate_tests!(set_6_hl_address, "tests/sm83/data/cb f6.json");
generate_tests!(set_6_a, "tests/sm83/data/cb f7.json");
generate_tests!(set_7_b, "tests/sm83/data/cb f8.json");
generate_tests!(set_7_c, "tests/sm83/data/cb f9.json");
generate_tests!(set_7_d, "tests/sm83/data/cb fa.json");
generate_tests!(set_7_e, "tests/sm83/data/cb fb.json");
generate_tests!(set_7_h, "tests/sm83/data/cb fc.json");
generate_tests!(set_7_l, "tests/sm83/data/cb fd.json");
generate_tests!(set_7_hl_address, "tests/sm83/data/cb fe.json");
generate_tests!(set_7_a, "tests/sm83/data/cb ff.json");

