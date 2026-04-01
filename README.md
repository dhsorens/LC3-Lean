# LC3-Lean

A formally verified LC3 virtual machine in Lean 4, following the article [Write your Own Virtual Machine](https://www.jmeiners.com/lc3-vm/).

All 14 LC3 instruction opcodes are implemented and formally verified to match the [LC3 ISA specification](https://www.jmeiners.com/lc3-vm/supplies/lc3-isa.pdf) — zero `sorry` statements in the codebase.

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

Run an assembled LC3 program:

```
.lake/build/bin/lc3-lean programs/2048.obj
```

**Note:** Input currently requires pressing Enter after each keystroke. Raw terminal mode is not yet implemented.

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
- **`Main.lean`** — Entry point, binary loader, execution loop with I/O trap dispatch
- **`programs/`** — LC3 object files (2048, Rogue)

## Contributions

This project is listed as a community LC3 implementation. See the [contributions page](https://www.jmeiners.com/lc3-vm/#contributions) for other implementations in various languages.
