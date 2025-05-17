# A Gameboy emulator written in Rust

This project is primarily for educational purposes.

# Status
These are the tests that the emulator successfully passes.

## Blargg
| Test                  | Pass |
| --                    | --   |
| 01-special            | ✔    |
| 02-interrupts         | ❌   |
| 03-op sp,hl           | ✔    | 
| 04-op r,imm           | ✔    |
| 05-op rp              | ✔    |
| 06-ld r,r             | ✔    |
| 07-jr,jp,call,ret,rst | ✔    |
| 08-misc instrs        | ✔    |
| 09-op r,r             | ✔    |
| 10-bit ops            | ✔    |
| 11-op a,(hl)          | ✔    |

## JSMoo-based SM83 JSON tests
All tests passes ✔

## Resources
https://gbdev.io/

Opcodes table: https://izik1.github.io/gbops/

Instructions with flag behaviour: https://rgbds.gbdev.io/docs/v0.9.1/gbz80.7

