-- Implement execution logic.
-- specification: https://www.jmeiners.com/lc3-vm/supplies/lc3-isa.pdf
import LC3Lean.VM.Memory
import LC3Lean.VM.Registers
import LC3Lean.VM.Instructions

namespace Execution.VM
open VM.Memory
open VM.Registers
open VM.Instructions

-- the auxiliary functions
section aux

def sign_extend (x : UInt16) (bit_count : UInt16) : UInt16 :=
  if ((x >>> (bit_count - 1)).land 1 == 1)
  then x.lor (0xFFF <<< bit_count)
  else x

#eval sign_extend 0b0111 4  -- 7
#eval sign_extend 0b1000 4  -- -8

def set_condition_codes (reg : Register) (value : UInt16) : Register :=
  let cond :=
    if value == 0 then VM.Registers.ConditionFlag.Z
    else if value.land 0x8000 != 0 then VM.Registers.ConditionFlag.N
    else VM.Registers.ConditionFlag.P
  { reg with cond := cond }

end aux

-- execution logic for each opcode
def op_add (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let dr  := (instr >>> 9).land 0x7
  let sr1 := (instr >>> 6).land 0x7
  let res1 ← VM.Registers.read reg sr1
  -- if bit[5] == 0 then DR = SR1 + SR2
  if (instr >>> 5).land 0x1 == 0x0 then
    let sr2 := instr.land 0x7
    -- load from register
    let res2 ← VM.Registers.read reg sr2
    -- add them together and write to the register
    let reg' ← VM.Registers.write reg dr (res1 + res2)
    some (reg', mem)
  -- else DR = SR1 + SEXT(imm5)
  else -- (instr >>> 5).land 0x1 == 0x1
    let imm := sign_extend (instr.land 0x1F) 5
    -- add them together and write to the register
    let reg' ← VM.Registers.write reg dr (res1 + imm)
    some (reg', mem)

def op_and (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let dr  := (instr >>> 9).land 0x7
  let sr1 := (instr >>> 6).land 0x7
  let res1 ← VM.Registers.read reg sr1
  -- if bit[5] == 0 then DR = SR1 AND SR2
  if (instr >>> 5).land 0x1 == 0x0 then
    let sr2 := instr.land 0x7
    -- load from register
    let res2 ← VM.Registers.read reg sr2
    -- take .land and write to the register
    let reg' ← VM.Registers.write reg dr (res1.land res2)
    some (reg', mem)
  else -- (instr >>> 5).land 0x1 == 0x1
    let imm := sign_extend (instr.land 0x31) 5
    -- take .land and write to the register
    let reg' ← VM.Registers.write reg dr (res1.land imm)
    some (reg', mem)

def op_br (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let n := (instr >>> 9).land 0x1
  let z := (instr >>> 10).land 0x1
  let p := (instr >>> 11).land 0x1
  let cond := reg.cond
  if (n == 0x1 && cond == VM.Registers.ConditionFlag.N) ||
     (z == 0x1 && cond == VM.Registers.ConditionFlag.Z) ||
     (p == 0x1 && cond == VM.Registers.ConditionFlag.P) then
  let offset := sign_extend (instr.land 0x1FF) 9
  let reg' := { reg with pc := reg.pc + offset }
  some (reg', mem)
  -- nothing to do
  else some (reg,mem)

-- JMP and RET
def op_jmp (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) :=
  let base_r := (instr >>> 6).land 0x7
  let reg' := { reg with pc := base_r }
  some (reg',mem)

-- JSR and JSRR
def op_jsr (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let bit := (instr >>> 11).land 0x1
  if bit == 0x0 then
    let pc' := (instr >>> 6).land 0x7
    let reg' := { reg with pc := pc' }
    some (reg',mem)
  else -- bit == 0x1
    let offset := sign_extend (instr.land 0x7FF) 11
    let reg' := { reg with pc := reg.pc + offset }
    some (reg', mem)

def op_ld (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let dr := (instr >>> 9).land 0x7
  let offset := sign_extend (instr.land 0x1FF) 9
  let addr := reg.pc + offset
  let value := VM.Memory.read mem addr
  let reg' ← VM.Registers.write reg dr value
  let cond :=
    if value == 0 then VM.Registers.ConditionFlag.Z
    else if value.land 0x8000 != 0 then VM.Registers.ConditionFlag.N
    else VM.Registers.ConditionFlag.P
  let reg'' := { reg' with cond := cond }
  some (reg'', mem)

def op_ldi (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let dr := (instr >>> 9).land 0x7
  let offset := sign_extend (instr.land 0x1FF) 9
  let addr := reg.pc + offset
  let indirect_addr := VM.Memory.read mem addr
  let value := VM.Memory.read mem indirect_addr
  let reg' ← VM.Registers.write reg dr value
  let reg'' := set_condition_codes reg' value
  some (reg'', mem)

def op_ldr (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let dr := (instr >>> 9).land 0x7
  let base_r := (instr >>> 6).land 0x7
  let offset := sign_extend (instr.land 0x3F) 6
  let base_val ← VM.Registers.read reg base_r
  let addr := base_val + offset
  let value := VM.Memory.read mem addr
  let reg' ← VM.Registers.write reg dr value
  let reg'' := set_condition_codes reg' value
  some (reg'', mem)

def op_lea (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let dr := (instr >>> 9).land 0x7
  let offset := sign_extend (instr.land 0x1FF) 9
  let addr := reg.pc + offset
  let reg' ← VM.Registers.write reg dr addr
  let reg'' := set_condition_codes reg' addr
  some (reg'', mem)

def op_not (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let dr := (instr >>> 9).land 0x7
  let sr := (instr >>> 6).land 0x7
  let res ← VM.Registers.read reg sr
  let value := res.complement
  let reg' ← VM.Registers.write reg dr value
  let reg'' := set_condition_codes reg' value
  some (reg'', mem)

-- unused
def op_rti (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := none

def op_st (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let sr := (instr >>> 9).land 0x7
  let offset := sign_extend (instr.land 0x1FF) 9
  let addr := reg.pc + offset
  let value ← VM.Registers.read reg sr
  let mem' := VM.Memory.write mem addr value
  some (reg, mem')

def op_sti (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let sr := (instr >>> 9).land 0x7
  let offset := sign_extend (instr.land 0x1FF) 9
  let addr := reg.pc + offset
  let indirect_addr := VM.Memory.read mem addr
  let value ← VM.Registers.read reg sr
  let mem' := VM.Memory.write mem indirect_addr value
  some (reg, mem')

def op_str (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do
  let sr := (instr >>> 9).land 0x7
  let base_r := (instr >>> 6).land 0x7
  let offset := sign_extend (instr.land 0x3F) 6
  let base_val ← VM.Registers.read reg base_r
  let addr := base_val + offset
  let value ← VM.Registers.read reg sr
  let mem' := VM.Memory.write mem addr value
  some (reg, mem')

def todo {A : Type} : A := sorry
def op_trap (instr : UInt16) (reg : Register) (mem : Memory) :
  Option (Register × Memory) := do todo
  -- let trapvector8 := instr.land 0xFF
  -- let reg' ← VM.Registers.write reg VM.Registers.R7 reg.pc
  -- let addr := trapvector8
  -- let pc := VM.Memory.read mem addr
  -- let reg'' := { reg' with pc := pc }
  -- some (reg'', mem)

def op_res (_ : UInt16) (_ : Register) (_ : Memory) :
  Option (Register × Memory) := none

-- a function to execute instructions

-- 1. Load one instruction from memory at the address of the PC register.
-- 2. Increment the PC register.
-- 3. Look at the opcode to determine which type of instruction it should perform.
-- 4. Perform the instruction using the parameters in the instruction.
-- 5. Go back to step 1.

def execute_step (reg : Register) (mem : Memory) : Option (Register × Memory) := do
  -- 1. Load one instruction from memory at the address of the PC register.
  let instr_addr := reg.pc
  let instr := VM.Memory.read mem instr_addr
  -- 2. Increment the PC register.
  let reg' := { reg with pc := reg.pc + 1 }
  -- 3. Look at the opcode to determine which type of instruction it should perform.
  let instr' ← VM.Instructions.instr_to_op instr
  -- 4. Perform the instruction using the parameters in the instruction.
  let (reg'',mem') ←
    match instr' with
    | .OP_BR   => op_br instr reg' mem -- branch
    | .OP_ADD  => op_add instr reg' mem -- add
    | .OP_LD   => op_ld instr reg' mem -- load
    | .OP_ST   => op_st instr reg' mem -- store
    | .OP_JSR  => op_jsr instr reg' mem -- jump register
    | .OP_AND  => op_and instr reg' mem -- bitwise and
    | .OP_LDR  => op_ldr instr reg' mem -- load register
    | .OP_STR  => op_str instr reg' mem -- store register
    | .OP_RTI  => op_rti instr reg' mem -- unused
    | .OP_NOT  => op_not instr reg' mem -- bitwise not
    | .OP_LDI  => op_ldi instr reg' mem -- load indirect
    | .OP_STI  => op_sti instr reg' mem -- store indirect
    | .OP_JMP  => op_jmp instr reg' mem -- jump
    | .OP_RES  => op_res instr reg' mem -- reserved (unused)
    | .OP_LEA  => op_lea instr reg' mem -- load effective address
    | .OP_TRAP => op_trap instr reg' mem -- execute trap
  -- 5. Go back to step 1.
  some (reg'',mem') -- execute reg' mem' (or could use concept of gas ... max steps)

def execute_loop (reg : Register) (mem : Memory) : IO Unit := do
  let mut reg' := reg
  let mut mem' := mem
  while true do
    match execute_step reg' mem' with
    | some (new_reg, new_mem) =>
      reg' := new_reg
      mem' := new_mem
    | none =>
      break

end Execution.VM
