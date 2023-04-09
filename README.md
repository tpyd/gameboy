# A Gameboy emulator written in Rust

## About
This project is mainly for educational purposes.

To initialize submodules, clone using `--recurse-submodules`, or run the command `git submodule update --init` after cloning.

# Status
There are the tests that the emulator successfully passes.

| Test                  | Pass |
| --                    | --   |
| 01-special            | ❌ |
| 02-interrupts         | ❌ |
| 03-op sp,hl           | Needs to run for longer |
| 04-op r,imm           | ✔ |
| 05-op rp              | ✔ |
| 06-ld r,r             | ✔ |
| 07-jr,jp,call,ret,rst | Does not end |
| 08-misc instrs        | Does not end |
| 09-op r,r             | ✔ |
| 10-bit ops            | ✔ |
| 11-op a,(hl)          | ✔ |

## Resources
https://gbdev.io/

Opcodes table: https://izik1.github.io/gbops/

Instructions with flag behaviour: https://rgbds.gbdev.io/docs/v0.5.2/gbz80.7/

