import LC3Lean.Execution
open Execution
open Registers
open Memory

-- Formal specification of opcode semantics.
-- Each theorem states: if the opcode function returns Some (reg', mem'),
-- then the postconditions hold.

section ADD

-- op_add: DR = SR1 + SR2 (register mode) or DR = SR1 + SEXT(imm5) (immediate mode)
-- Condition codes are updated. Memory is unchanged.
theorem op_add_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  ∀ (reg' : Register) (mem' : Memory),
  let dr := (instr >>> 9).land 0x7
  let sr1 := (instr >>> 6).land 0x7
  let mode := (instr >>> 5).land 0x1
  let sr2 := instr.land 0x7
  let imm5 := sign_extend (instr.land 0x1F) 5
  op_add instr reg mem = some (reg', mem') →
  -- Memory unchanged
  mem' = mem ∧
  -- Result depends on mode bit
  (mode = 0 →
    ∀ r1 r2,
    Registers.read reg sr1 = some r1 →
    Registers.read reg sr2 = some r2 →
    ∃ reg_written,
      Registers.write reg dr (r1 + r2) = some reg_written ∧
      reg' = set_condition_codes reg_written (r1 + r2)) ∧
  (mode = 1 →
    ∀ r1,
    Registers.read reg sr1 = some r1 →
    ∃ reg_written,
      Registers.write reg dr (r1 + imm5) = some reg_written ∧
      reg' = set_condition_codes reg_written (r1 + imm5)) := by
  sorry

end ADD

section AND

-- op_and: DR = SR1 AND SR2 (register mode) or DR = SR1 AND SEXT(imm5) (immediate mode)
-- Condition codes are updated. Memory is unchanged.
theorem op_and_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  ∀ (reg' : Register) (mem' : Memory),
  op_and instr reg mem = some (reg', mem') →
  let dr := (instr >>> 9).land 0x7
  let sr1 := (instr >>> 6).land 0x7
  let mode := (instr >>> 5).land 0x1
  let sr2 := instr.land 0x7
  let imm5 := sign_extend (instr.land 0x1F) 5
  mem' = mem ∧
  (mode = 0 →
    ∀ r1 r2,
    Registers.read reg sr1 = some r1 →
    Registers.read reg sr2 = some r2 →
    ∃ reg_written,
      Registers.write reg dr (r1.land r2) = some reg_written ∧
      reg' = set_condition_codes reg_written (r1.land r2)) ∧
  (mode = 1 →
    ∀ r1,
    Registers.read reg sr1 = some r1 →
    ∃ reg_written,
      Registers.write reg dr (r1.land imm5) = some reg_written ∧
      reg' = set_condition_codes reg_written (r1.land imm5)) := by
  sorry

end AND

section BR

-- op_br: if condition flags match, PC = PC + SEXT(PCoffset9). Memory unchanged.
theorem op_br_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_br instr reg mem with
  | none => true
  | some (reg', mem') =>
    let n := (instr >>> 11).land 0x1
    let z := (instr >>> 10).land 0x1
    let p := (instr >>> 9).land 0x1
    let offset := sign_extend (instr.land 0x1FF) 9
    mem' = mem ∧
    ((n = 1 ∧ reg.cond = Registers.ConditionFlag.N) ∨
     (z = 1 ∧ reg.cond = Registers.ConditionFlag.Z) ∨
     (p = 1 ∧ reg.cond = Registers.ConditionFlag.P) →
     reg' = { reg with pc := reg.pc + offset }) ∧
    (¬((n = 1 ∧ reg.cond = Registers.ConditionFlag.N) ∨
       (z = 1 ∧ reg.cond = Registers.ConditionFlag.Z) ∨
       (p = 1 ∧ reg.cond = Registers.ConditionFlag.P)) →
     reg' = reg)
    := by sorry

end BR

section JMP

-- op_jmp: PC = value of register BaseR (bits[8:6]). Memory unchanged.
theorem op_jmp_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_jmp instr reg mem with
  | none => true
  | some (reg', mem') =>
    let base_r := (instr >>> 6).land 0x7
    mem' = mem ∧
    (∀ base_val, Registers.read reg base_r = some base_val →
      reg' = { reg with pc := base_val })
    := by sorry

end JMP

section JSR

-- op_jsr: R7 = PC, then PC = BaseR (JSRR) or PC = PC + SEXT(PCoffset11) (JSR).
-- Memory unchanged.
theorem op_jsr_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_jsr instr reg mem with
  | none => true
  | some (reg', mem') =>
    let mode := (instr >>> 11).land 0x1
    let base_r := (instr >>> 6).land 0x7
    let offset := sign_extend (instr.land 0x7FF) 11
    mem' = mem ∧
    -- JSRR mode: read BaseR first, then save R7, then jump
    (mode = 0 →
      ∀ base_val reg_r7,
      Registers.read reg base_r = some base_val →
      Registers.write reg 7 reg.pc = some reg_r7 →
      reg' = { reg_r7 with pc := base_val }) ∧
    -- JSR mode: save R7, then jump to PC + offset
    (mode = 1 →
      ∀ reg_r7,
      Registers.write reg 7 reg.pc = some reg_r7 →
      reg' = { reg_r7 with pc := reg_r7.pc + offset })
    := by sorry

end JSR

section LD

-- op_ld: DR = mem[PC + SEXT(PCoffset9)], condition codes updated. Memory unchanged.
theorem op_ld_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_ld instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    let value := Memory.read mem addr
    mem' = mem ∧
    (∃ reg_temp,
      Registers.write reg dr value = some reg_temp ∧
      reg' = set_condition_codes reg_temp value)
    := by sorry

end LD

section LDI

-- op_ldi: DR = mem[mem[PC + SEXT(PCoffset9)]], condition codes updated. Memory unchanged.
theorem op_ldi_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_ldi instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    let indirect_addr := Memory.read mem addr
    let value := Memory.read mem indirect_addr
    mem' = mem ∧
    (∃ reg_temp,
      Registers.write reg dr value = some reg_temp ∧
      reg' = set_condition_codes reg_temp value)
    := by sorry

end LDI

section LDR

-- op_ldr: DR = mem[BaseR + SEXT(offset6)], condition codes updated. Memory unchanged.
theorem op_ldr_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_ldr instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let base_r := (instr >>> 6).land 0x7
    let offset := sign_extend (instr.land 0x3F) 6
    mem' = mem ∧
    (∀ base_val,
      Registers.read reg base_r = some base_val →
      let addr := base_val + offset
      let value := Memory.read mem addr
      ∃ reg_temp,
        Registers.write reg dr value = some reg_temp ∧
        reg' = set_condition_codes reg_temp value)
    := by sorry

end LDR

section LEA

-- op_lea: DR = PC + SEXT(PCoffset9), condition codes updated. Memory unchanged.
theorem op_lea_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_lea instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    mem' = mem ∧
    (∃ reg_temp,
      Registers.write reg dr addr = some reg_temp ∧
      reg' = set_condition_codes reg_temp addr)
    := by sorry

end LEA

section NOT

-- op_not: DR = NOT(SR), condition codes updated. Memory unchanged.
theorem op_not_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_not instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let sr := (instr >>> 6).land 0x7
    mem' = mem ∧
    (∀ val,
      Registers.read reg sr = some val →
      let result := val.complement
      ∃ reg_temp,
        Registers.write reg dr result = some reg_temp ∧
        reg' = set_condition_codes reg_temp result)
    := by sorry

end NOT

section ST

-- op_st: mem[PC + SEXT(PCoffset9)] = SR. Registers unchanged.
theorem op_st_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_st instr reg mem with
  | none => true
  | some (reg', mem') =>
    let sr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    reg' = reg ∧
    (∀ value,
      Registers.read reg sr = some value →
      mem' = Memory.write mem addr value)
    := by sorry

end ST

section STI

-- op_sti: mem[mem[PC + SEXT(PCoffset9)]] = SR. Registers unchanged.
theorem op_sti_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_sti instr reg mem with
  | none => true
  | some (reg', mem') =>
    let sr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    let indirect_addr := Memory.read mem addr
    reg' = reg ∧
    (∀ value,
      Registers.read reg sr = some value →
      mem' = Memory.write mem indirect_addr value)
    := by sorry

end STI

section STR

-- op_str: mem[BaseR + SEXT(offset6)] = SR. Registers unchanged.
theorem op_str_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_str instr reg mem with
  | none => true
  | some (reg', mem') =>
    let sr := (instr >>> 9).land 0x7
    let base_r := (instr >>> 6).land 0x7
    let offset := sign_extend (instr.land 0x3F) 6
    reg' = reg ∧
    (∀ base_val value,
      Registers.read reg base_r = some base_val →
      Registers.read reg sr = some value →
      let addr := base_val + offset
      mem' = Memory.write mem addr value)
    := by sorry

end STR

section TRAP

-- op_trap: R7 = PC, PC = mem[TRAPVECT8]. Memory unchanged.
theorem op_trap_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_trap instr reg mem with
  | none => true
  | some (reg', mem') =>
    let trap_vector := instr.land 0xFF
    let pc := Memory.read mem trap_vector
    mem' = mem ∧
    (∃ reg_temp,
      Registers.write reg 7 reg.pc = some reg_temp ∧
      reg' = { reg_temp with pc := pc })
    := by sorry

end TRAP
