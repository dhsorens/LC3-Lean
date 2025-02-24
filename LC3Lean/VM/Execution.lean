-- Implement execution logic.
import LC3Lean.VM.Memory
import LC3Lean.VM.Registers
import LC3Lean.VM.Instructions

namespace Execution.VM
open VM.Memory
open VM.Registers
open VM.Instructions

def todo {A : Type} : A := sorry

-- the auxiliary functions
section aux



end aux


-- execution logic for each opcode

def op_br (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

-- TODO immediate mode
def op_add (dr : Registers) (sr1 : Registers) (sr2 : Registers)
  (reg : Register) (mem : Memory) : Option (Register × Memory) := do
  -- load from register
  let res1 ← VM.Registers.read reg sr1
  let res2 ← VM.Registers.read reg sr2
  -- add them together and write to the register
  let reg' ← VM.Registers.write reg dr (res1 + res2)
  some (reg', mem)

def op_ld (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_st (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_jsr (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

-- TODO immediate mode
def op_and  (dr : Registers) (sr1 : Registers) (sr2 : Registers)
  (reg : Register) (mem : Memory) : Option (Register × Memory) := do
  -- load from register
  let res1 ← VM.Registers.read reg sr1
  let res2 ← VM.Registers.read reg sr2
  -- add them together and write to the register
  let reg' ← VM.Registers.write reg dr (res1.land res2)
  some (reg', mem)

def op_ldr (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_str (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_rti (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_not (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_ldi (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_sti (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_jmp (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_res (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_lea (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none

def op_trap (reg : Register) (mem : Memory) : Option (Register × Memory) :=
  none


-- a function to execute instructions

-- 1. Load one instruction from memory at the address of the PC register.
-- 2. Increment the PC register.
-- 3. Look at the opcode to determine which type of instruction it should perform.
-- 4. Perform the instruction using the parameters in the instruction.
-- 5. Go back to step 1.

def execute (reg : Register) (mem : Memory) : Option (Register × Memory) := do
  -- 1. Load one instruction from memory at the address of the PC register.
  let instr_addr ← VM.Registers.read reg .R_PC
  let instr := VM.Memory.read mem instr_addr
  -- 2. Increment the PC register.
  let reg' ← VM.Registers.write reg .R_PC (instr_addr + 1)
  -- 3. Look at the opcode to determine which type of instruction it should perform.
  -- 4. Perform the instruction using the parameters in the instruction.
  let (reg',mem') ←
    match VM.Instructions.instr_op instr with
    | .OP_BR   => op_br   reg mem -- branch
    | .OP_ADD  => todo -- op_add  reg mem -- add
    | .OP_LD   => op_ld   reg mem -- load
    | .OP_ST   => op_st   reg mem -- store
    | .OP_JSR  => op_jsr  reg mem -- jump register
    | .OP_AND  => todo -- op_and  reg mem -- bitwise and
    | .OP_LDR  => op_ldr  reg mem -- load register
    | .OP_STR  => op_str  reg mem -- store register
    | .OP_RTI  => op_rti  reg mem -- unused
    | .OP_NOT  => op_not  reg mem -- bitwise not
    | .OP_LDI  => op_ldi  reg mem -- load indirect
    | .OP_STI  => op_sti  reg mem -- store indirect
    | .OP_JMP  => op_jmp  reg mem -- jump
    | .OP_RES  => op_res  reg mem -- reserved (unused)
    | .OP_LEA  => op_lea  reg mem -- load effective address
    | .OP_TRAP => op_trap reg mem -- execute trap
  -- 5. Go back to step 1.
  todo
  -- execute reg' mem'

end Execution.VM
