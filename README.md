# LC3-Lean

A **formally verified** LC3 virtual machine written in [Lean 4](https://lean-lang.org/), following the article [Write your Own Virtual Machine](https://www.jmeiners.com/lc3-vm/).

All 14 LC3 instruction opcodes are implemented and **proven correct** against the [LC3 ISA specification](https://www.jmeiners.com/lc3-vm/supplies/lc3-isa.pdf) — zero `sorry` statements in the codebase. Each opcode has a formal theorem stating that the implementation matches the ISA-defined behavior.

## Building

Clone this repository and build the project:

```
lake build
```

This produces a binary at `.lake/build/bin/lc3-lean`.

To also build the formal verification library:

```
lake build LC3Correct
```

## Running

Run any assembled LC3 object file (`.obj`):

```
.lake/build/bin/lc3-lean <path-to-program.obj>
```

For example, to play 2048:

```
.lake/build/bin/lc3-lean programs/2048.obj
```

The VM supports:
- All 14 LC3 instructions (ADD, AND, NOT, BR, JMP, JSR, LD, LDI, LDR, LEA, ST, STI, STR, TRAP)
- I/O trap routines (GETC, OUT, PUTS, IN, PUTSP, HALT)
- Memory-mapped I/O (keyboard status/data registers at 0xFE00/0xFE02)
- Programs that use ANSI terminal escape codes

Any `.obj` file assembled for the LC3 ISA should work. You can find LC3 programs to run [here](https://github.com/justinmeiners/lc3-vm) or assemble your own using an [LC3 assembler](https://github.com/chiragsakhuja/lc3tools).

**Note:** Raw terminal mode is provided via a small C FFI (see `c/terminal.c`), which enables character-at-a-time input and non-blocking keyboard polling on POSIX systems. All VM logic — instruction execution, memory, registers, trap routines, and formal proofs — is implemented entirely in Lean 4. The C code is solely for terminal I/O.

## Project Structure

- **`LC3Lean/`** — The VM implementation
  - `Registers.lean` — 8 general-purpose registers, PC, condition flags
  - `Memory.lean` — 64KB (2^16) addressable memory
  - `Instructions.lean` — Opcode decoding
  - `Execution.lean` — Fetch-decode-execute cycle and all 14 opcode handlers
  - `Trap.lean` — I/O trap routines (PUTS, GETC, OUT, IN, PUTSP, HALT)
- **`LC3Correct/`** — Formal verification ([details](LC3Correct/README.md))
  - `ExecutionCorrect.lean` — Correctness theorems for all 14 opcodes
  - `RegisterLemmas.lean` — Register read/write properties
  - `MemoryLemmas.lean` — Memory read/write properties
- **`Main.lean`** — Entry point, binary loader, execution loop with MMIO and I/O trap dispatch
- **`c/terminal.c`** — C FFI for raw terminal mode (POSIX `termios`/`select`)
- **`programs/`** — LC3 object files (2048, Rogue)

## Contributions

This project is listed as a community LC3 implementation. See the [contributions page](https://www.jmeiners.com/lc3-vm/#contributions) for other implementations in various languages.
