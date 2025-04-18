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

#[test]
fn test_noop() {
    let file = File::open("tests/sm83/data/00.json").unwrap();
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
    assert_eq!(cpu.pc, test.r#final.pc);
    assert_eq!(cpu.sp, test.r#final.sp);
    assert_eq!(cpu.registers.a, test.r#final.a);
    assert_eq!(cpu.registers.b, test.r#final.b);
    assert_eq!(cpu.registers.c, test.r#final.c);
    assert_eq!(cpu.registers.d, test.r#final.d);
    assert_eq!(cpu.registers.e, test.r#final.e);
    assert_eq!(u8::from(cpu.registers.f), test.r#final.f);
    assert_eq!(cpu.registers.h, test.r#final.h);
    assert_eq!(cpu.registers.l, test.r#final.l);

    let ram_values: Vec<(usize, u8)> = test.initial.ram
        .iter()
        .map(|v| (v[0] as usize, v[1] as u8))
        .collect();

    for (address, value) in ram_values {
        let ram_value = cpu.bus.read_byte(address);
        assert_eq!(ram_value, value)
    }
}

