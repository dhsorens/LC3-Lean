import LC3Lean.Execution
open Execution
open Registers
open Memory

-- formal specification of opcode semantics

section Instructions

-- todo a specification that instructions go to the right places

end Instructions

section ADD

-- Specification for a successful call to op_add:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. If bit[5] = 0:
--    - DR = SR1 + SR2 where
--      DR = bits[11:9]
--      SR1 = bits[8:6]
--      SR2 = bits[2:0]
-- 2. If bit[5] = 1:
--    - DR = SR1 + SEXT(imm5) where
--      DR = bits[11:9]
--      SR1 = bits[8:6]
--      imm5 = bits[4:0]
-- 3. Memory is unchanged
-- 4. Returns None if register reads/writes fail

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
    Registers.write reg dr (r1 + r2) = some reg') ∧
  (mode = 1 →
    ∀ r1,
    Registers.read reg sr1 = some r1 →
    Registers.write reg dr (r1 + imm5) = some reg') := by
    -- proof
    intro reg' mem' dr sr1 mode sr2 imm5
    -- get all the information from op_add; proves mem' = mem
    unfold op_add
    simp
    cases (Registers.read reg ((instr >>> 6).land 7)) with | none => simp | some res1 =>
    simp
    cases (Registers.read reg (instr.land 7)) with | none => sorry | some res2 =>
    simp
    cases (Registers.write reg ((instr >>> 9).land 7) (res1 + res2)) with | none => sorry | some reg'' =>
    simp
    by_cases h_mode : ((instr >>> 5).land 1 = 0)
    -- get the mode bit ; proceed with cases
    . intro h_add
      rw [h_mode] at h_add ; simp at h_add
      cases h_add with | intro h_reg h_mem =>
      constructor ; rw [h_mem]
      constructor
      . intro _ ; exact h_reg
      . intro _ ; rw [←h_reg]
        sorry -- should already be solved
    . intro h_add

      sorry


end ADD


section AND

-- Specification for op_and:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. If bit[5] = 0:
--    - DR = SR1 AND SR2 where
--      DR = bits[11:9]
--      SR1 = bits[8:6]
--      SR2 = bits[2:0]
-- 2. If bit[5] = 1:
--    - DR = SR1 AND SEXT(imm5) where
--      DR = bits[11:9]
--      SR1 = bits[8:6]
--      imm5 = bits[4:0]
-- 3. Memory is unchanged
-- 4. Returns None if register reads/writes fail

theorem op_and_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  ∀ (reg' : Register) (mem' : Memory),
  op_and instr reg mem = some (reg', mem') →
  let dr := (instr >>> 9).land 0x7
  let sr1 := (instr >>> 6).land 0x7
  let mode := (instr >>> 5).land 0x1
  let sr2 := instr.land 0x7
  let imm5 := sign_extend (instr.land 0x1F) 5
  -- Memory unchanged
  mem' = mem ∧
  -- Result depends on mode bit
  (mode = 0 →
    ∀ r1 r2,
    Registers.read reg sr1 = some r1 →
    Registers.read reg sr2 = some r2 →
    Registers.write reg dr (r1.land r2) = some reg') ∧
  (mode = 1 →
    ∀ r1,
    Registers.read reg sr1 = some r1 →
    Registers.write reg dr (r1.land imm5) = some reg') := by
  intro reg' mem'
  unfold op_and ; simp
  cases (Registers.read reg ((instr >>> 6).land 7)) with | none => simp | some res1 =>
  simp
  intro h_and
  constructor
  . by_cases h_1 : ((instr >>> 5).land 1 = 0)
    . rw [h_1] at h_and ; simp at h_and
      sorry
    . sorry
  . constructor
    . intro h_land
      rw [h_land] at h_and ; simp at h_and
      intro r1 r2 h_r1 h_r2
      rw [h_r2] at h_and ; simp at h_and
      sorry
    . sorry


  -- by_cases h_mode : ((instr >>> 5).land 1 = 0)
  -- -- get the mode bit ; proceed with cases
  -- . rw [h_mode]; simp
  --   cases (Registers.read reg (instr.land 7)) with | none => simp | some res2 =>
  --   simp
  --   cases (Registers.write reg ((instr >>> 9).land 7) (res1.land res2)) with | none => simp | some reg'' =>
  --   simp
  --   intro h_reg h_mem ; rw [h_reg,h_mem]
  --   simp
  -- . have h_null : (instr >>> 5).land 1 = 1 := by sorry
  --   rw [h_null]; simp -- TODO
  --   cases (Registers.write reg ((instr >>> 9).land 7)
  --     (res1.land (sign_extend (instr.land 49) 5))) with
  --   | none => simp
  --   | some reg'' =>
  --     simp
  --     intro h_reg h_mem ; rw [h_mem]
  --     constructor ; simp
  --     -- bug?
  --     sorry

end AND

section BR

-- Specification for op_br:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. If (n AND N) OR (z AND Z) OR (p AND P):
--    - PC = PC + SEXT(PCoffset9)
--    where n = bit[11], z = bit[10], p = bit[9]
--    PCoffset9 = bits[8:0]
-- 2. Memory is unchanged
-- 3. Returns None if register access fails

theorem op_br_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_br instr reg mem with
  | none => true
  | some (reg', mem') =>
    let n := (instr >>> 11).land 0x1
    let z := (instr >>> 10).land 0x1
    let p := (instr >>> 9).land 0x1
    let offset := sign_extend (instr.land 0x1FF) 9
    -- Memory unchanged
    mem' = mem ∧
    -- Branch taken if condition matches
    ((n = 1 ∧ reg.cond = Registers.ConditionFlag.N) ∨
     (z = 1 ∧ reg.cond = Registers.ConditionFlag.Z) ∨
     (p = 1 ∧ reg.cond = Registers.ConditionFlag.P) →
     reg' = { reg with pc := reg.pc + offset }) ∧
    -- Branch not taken if condition doesn't match
    (¬((n = 1 ∧ reg.cond = Registers.ConditionFlag.N) ∨
       (z = 1 ∧ reg.cond = Registers.ConditionFlag.Z) ∨
       (p = 1 ∧ reg.cond = Registers.ConditionFlag.P)) →
     reg' = reg)
    := by sorry

end BR

section JMP

-- Specification for op_jmp (includes RET):
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. PC = BaseR where BaseR = bits[8:6]
-- 2. Memory is unchanged
-- 3. Returns None if register access fails
-- Note: RET is a special case where BaseR = R7

theorem op_jmp_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_jmp instr reg mem with
  | none => true
  | some (reg', mem') =>
    let base_r := (instr >>> 6).land 0x7
    -- Memory unchanged
    mem' = mem ∧
    -- PC updated to BaseR
    reg' = { reg with pc := base_r }
    := by sorry

end JMP

section JSR

-- Specification for op_jsr (includes JSRR):
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. R7 = PC
-- 2. If bit[11] = 0 (JSRR):
--    - PC = BaseR where BaseR = bits[8:6]
-- 3. If bit[11] = 1 (JSR):
--    - PC = PC + SEXT(PCoffset11) where PCoffset11 = bits[10:0]
-- 4. Memory is unchanged
-- 5. Returns None if register access fails

theorem op_jsr_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_jsr instr reg mem with
  | none => true
  | some (reg', mem') =>
    let mode := (instr >>> 11).land 0x1
    let base_r := (instr >>> 6).land 0x7
    let offset := sign_extend (instr.land 0x7FF) 11
    -- Memory unchanged
    mem' = mem ∧
    -- Mode determines PC update
    (mode = 0 →
      reg' = { reg with pc := base_r }) ∧
    (mode = 1 →
      reg' = { reg with pc := reg.pc + offset })
    := by sorry

end JSR

section LD

-- Specification for op_ld:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. DR = mem[PC + SEXT(PCoffset9)] where
--    DR = bits[11:9]
--    PCoffset9 = bits[8:0]
-- 2. Update condition codes based on DR value
-- 3. Memory is unchanged
-- 4. Returns None if register access fails

theorem op_ld_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_ld instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    let value := Memory.read mem addr
    -- Memory unchanged
    mem' = mem ∧
    -- DR updated with memory value
    (∃ reg_temp,
      Registers.write reg dr value = some reg_temp ∧
      -- Condition codes updated
      reg' = set_condition_codes reg_temp value)
    := by sorry

end LD

section LDI

-- Specification for op_ldi:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. DR = mem[mem[PC + SEXT(PCoffset9)]] where
--    DR = bits[11:9]
--    PCoffset9 = bits[8:0]
-- 2. Update condition codes based on DR value
-- 3. Memory is unchanged
-- 4. Returns None if register access fails

theorem op_ldi_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_ldi instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    let indirect_addr := Memory.read mem addr
    let value := Memory.read mem indirect_addr
    -- Memory unchanged
    mem' = mem ∧
    -- DR updated with indirect memory value
    (∃ reg_temp,
      Registers.write reg dr value = some reg_temp ∧
      -- Condition codes updated
      reg' = set_condition_codes reg_temp value)
    := by sorry

end LDI

section LDR

-- Specification for op_ldr:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. DR = mem[BaseR + SEXT(offset6)] where
--    DR = bits[11:9]
--    BaseR = bits[8:6]
--    offset6 = bits[5:0]
-- 2. Update condition codes based on DR value
-- 3. Memory is unchanged
-- 4. Returns None if register access fails

theorem op_ldr_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_ldr instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let base_r := (instr >>> 6).land 0x7
    let offset := sign_extend (instr.land 0x3F) 6
    -- Memory unchanged
    mem' = mem ∧
    -- DR updated with memory value at base + offset
    (∀ base_val,
      Registers.read reg base_r = some base_val →
      let addr := base_val + offset
      let value := Memory.read mem addr
      ∃ reg_temp,
        Registers.write reg dr value = some reg_temp ∧
        -- Condition codes updated
        reg' = set_condition_codes reg_temp value)
    := by sorry

end LDR

section LEA

-- Specification for op_lea:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. DR = PC + SEXT(PCoffset9) where
--    DR = bits[11:9]
--    PCoffset9 = bits[8:0]
-- 2. Update condition codes based on DR value
-- 3. Memory is unchanged
-- 4. Returns None if register access fails

theorem op_lea_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_lea instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    -- Memory unchanged
    mem' = mem ∧
    -- DR updated with effective address
    (∃ reg_temp,
      Registers.write reg dr addr = some reg_temp ∧
      -- Condition codes updated
      reg' = set_condition_codes reg_temp addr)
    := by sorry

end LEA

section NOT

-- Specification for op_not:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. DR = NOT(SR) where
--    DR = bits[11:9]
--    SR = bits[8:6]
-- 2. Update condition codes based on DR value
-- 3. Memory is unchanged
-- 4. Returns None if register access fails

theorem op_not_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_not instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let sr := (instr >>> 6).land 0x7
    -- Memory unchanged
    mem' = mem ∧
    -- DR updated with NOT of SR
    (∀ val,
      Registers.read reg sr = some val →
      let result := val.complement
      ∃ reg_temp,
        Registers.write reg dr result = some reg_temp ∧
        -- Condition codes updated
        reg' = set_condition_codes reg_temp result)
    := by sorry

end NOT

section ST

-- Specification for op_st:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem') where:
-- 1. mem[PC + SEXT(PCoffset9)] = SR where
--    SR = bits[11:9]
--    PCoffset9 = bits[8:0]
-- 2. Registers unchanged
-- 3. Returns None if register access fails

theorem op_st_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_st instr reg mem with
  | none => true
  | some (reg', mem') =>
    let sr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    -- Registers unchanged
    reg' = reg ∧
    -- Memory updated at computed address
    (∀ value,
      Registers.read reg sr = some value →
      mem' = Memory.write mem addr value)
    := by sorry

end ST

section STI

-- Specification for op_sti:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem') where:
-- 1. mem[mem[PC + SEXT(PCoffset9)]] = SR where
--    SR = bits[11:9]
--    PCoffset9 = bits[8:0]
-- 2. Registers unchanged
-- 3. Returns None if register access fails

theorem op_sti_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_sti instr reg mem with
  | none => true
  | some (reg', mem') =>
    let sr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    let indirect_addr := Memory.read mem addr
    -- Registers unchanged
    reg' = reg ∧
    -- Memory updated at indirect address
    (∀ value,
      Registers.read reg sr = some value →
      mem' = Memory.write mem indirect_addr value)
    := by sorry

end STI

section STR

-- Specification for op_str:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem') where:
-- 1. mem[BaseR + SEXT(offset6)] = SR where
--    SR = bits[11:9]
--    BaseR = bits[8:6]
--    offset6 = bits[5:0]
-- 2. Registers unchanged
-- 3. Returns None if register access fails

theorem op_str_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_str instr reg mem with
  | none => true
  | some (reg', mem') =>
    let sr := (instr >>> 9).land 0x7
    let base_r := (instr >>> 6).land 0x7
    let offset := sign_extend (instr.land 0x3F) 6
    -- Registers unchanged
    reg' = reg ∧
    -- Memory updated at base + offset
    (∀ base_val value,
      Registers.read reg base_r = some base_val →
      Registers.read reg sr = some value →
      let addr := base_val + offset
      mem' = Memory.write mem addr value)
    := by sorry

end STR

section TRAP

-- Specification for op_trap:
-- Given an instruction instr, registers reg, and memory mem
-- Returns Some (reg', mem) where:
-- 1. R7 = PC
-- 2. PC = mem[TRAPVECT8] where TRAPVECT8 = bits[7:0]
-- 3. Memory is unchanged
-- 4. Returns None if register access fails

theorem op_trap_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_trap instr reg mem with
  | none => true
  | some (reg', mem') =>
    let trap_vector := instr.land 0xFF
    let pc := Memory.read mem trap_vector
    -- Memory unchanged
    mem' = mem ∧
    -- R7 gets old PC, PC gets trap vector address
    (∃ reg_temp,
      Registers.write reg 7 reg.pc = some reg_temp ∧
      reg' = { reg_temp with pc := pc })
    := by sorry

end TRAP
