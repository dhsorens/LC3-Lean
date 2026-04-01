# Formal Verification

This directory contains formal proofs that the LC3 VM implementation correctly implements the [LC3 ISA](https://www.jmeiners.com/lc3-vm/supplies/lc3-isa.pdf).

## What's Proven

**`ExecutionCorrect.lean`** — Correctness theorems for all 14 opcodes:
- ALU operations: ADD, AND, NOT (correct computation + condition code updates)
- Memory loads: LD, LDI, LDR, LEA (correct address computation + condition codes)
- Memory stores: ST, STI, STR (correct address computation, registers unchanged)
- Control flow: BR (correct condition flag matching), JMP (correct register read), JSR/JSRR (R7 saved, correct jump target)
- System: TRAP (R7 saved, PC set from trap vector table)

**`RegisterLemmas.lean`** — Properties of register read/write:
- Read validity: `read` succeeds for all GP register indices (0–7)
- Write validity: `write` succeeds for all GP register indices (0–7)
- Read-after-write: writing then reading the same register returns the written value

**`MemoryLemmas.lean`** — Properties of memory read/write:
- Read-after-write (same address): returns the written value
- Read-after-write (different address): returns the original value

## Building

```
lake build LC3Correct
```
