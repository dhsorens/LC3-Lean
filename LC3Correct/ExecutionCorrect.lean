import LC3Lean.Execution
open Execution
open Registers
open Memory

-- Formal specification of opcode semantics.
-- Each theorem states: if the opcode function returns Some (reg', mem'),
-- then the postconditions hold.

-- Bridge lemma for ConditionFlag BEq
private theorem cond_beq_eq (a b : ConditionFlag) : (a == b) = true ↔ a = b := by
  cases a <;> cases b <;> decide

section ADD

theorem op_add_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_add instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let sr1 := (instr >>> 6).land 0x7
    let mode := (instr >>> 5).land 0x1
    let sr2 := instr.land 0x7
    let imm5 := sign_extend (instr.land 0x1F) 5
    mem' = mem ∧
    (mode = 0 → ∀ r1 r2, Registers.read reg sr1 = some r1 → Registers.read reg sr2 = some r2 →
      ∃ reg_written, Registers.write reg dr (r1 + r2) = some reg_written ∧
        reg' = set_condition_codes reg_written (r1 + r2)) ∧
    (mode = 1 → ∀ r1, Registers.read reg sr1 = some r1 →
      ∃ reg_written, Registers.write reg dr (r1 + imm5) = some reg_written ∧
        reg' = set_condition_codes reg_written (r1 + imm5))
    := by
  unfold op_add; simp
  cases Registers.read reg ((instr >>> 6).land 7) with
  | none => simp
  | some res1 =>
    simp; cases Registers.read reg (instr.land 7) with
    | none =>
      simp; cases Registers.write reg ((instr >>> 9).land 7) (res1 + sign_extend (instr.land 31) 5) with
      | none => simp
      | some rw => split <;> simp_all
    | some res2 =>
      simp; cases Registers.write reg ((instr >>> 9).land 7) (res1 + res2) with
      | none =>
        simp; cases Registers.write reg ((instr >>> 9).land 7) (res1 + sign_extend (instr.land 31) 5) with
        | none => simp
        | some rw => split <;> simp_all
      | some rw =>
        cases Registers.write reg ((instr >>> 9).land 7) (res1 + sign_extend (instr.land 31) 5) with
        | none => split <;> simp_all
        | some rw2 =>
          split
          · simp_all
          · rename_i heq; by_cases h_if : (instr >>> 5).land 1 = 0 <;> simp_all

end ADD

section AND

theorem op_and_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_and instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let sr1 := (instr >>> 6).land 0x7
    let mode := (instr >>> 5).land 0x1
    let sr2 := instr.land 0x7
    let imm5 := sign_extend (instr.land 0x1F) 5
    mem' = mem ∧
    (mode = 0 → ∀ r1 r2, Registers.read reg sr1 = some r1 → Registers.read reg sr2 = some r2 →
      ∃ reg_written, Registers.write reg dr (r1.land r2) = some reg_written ∧
        reg' = set_condition_codes reg_written (r1.land r2)) ∧
    (mode = 1 → ∀ r1, Registers.read reg sr1 = some r1 →
      ∃ reg_written, Registers.write reg dr (r1.land imm5) = some reg_written ∧
        reg' = set_condition_codes reg_written (r1.land imm5))
    := by
  unfold op_and; simp
  cases Registers.read reg ((instr >>> 6).land 7) with
  | none => simp
  | some res1 =>
    simp; cases Registers.read reg (instr.land 7) with
    | none =>
      simp; cases Registers.write reg ((instr >>> 9).land 7) (res1.land (sign_extend (instr.land 31) 5)) with
      | none => simp
      | some rw => split <;> simp_all
    | some res2 =>
      simp; cases Registers.write reg ((instr >>> 9).land 7) (res1.land res2) with
      | none =>
        simp; cases Registers.write reg ((instr >>> 9).land 7) (res1.land (sign_extend (instr.land 31) 5)) with
        | none => simp
        | some rw => split <;> simp_all
      | some rw =>
        cases Registers.write reg ((instr >>> 9).land 7) (res1.land (sign_extend (instr.land 31) 5)) with
        | none => split <;> simp_all
        | some rw2 =>
          split
          · simp_all
          · rename_i heq; by_cases h_if : (instr >>> 5).land 1 = 0 <;> simp_all

end AND

section BR

theorem op_br_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_br instr reg mem with
  | none => true
  | some (reg', mem') =>
    let n := (instr >>> 11).land 0x1
    let z := (instr >>> 10).land 0x1
    let p := (instr >>> 9).land 0x1
    let offset := sign_extend (instr.land 0x1FF) 9
    mem' = mem ∧
    ((n = 1 ∧ reg.cond = ConditionFlag.N) ∨
     (z = 1 ∧ reg.cond = ConditionFlag.Z) ∨
     (p = 1 ∧ reg.cond = ConditionFlag.P) →
     reg' = { reg with pc := reg.pc + offset }) ∧
    (¬((n = 1 ∧ reg.cond = ConditionFlag.N) ∨
       (z = 1 ∧ reg.cond = ConditionFlag.Z) ∨
       (p = 1 ∧ reg.cond = ConditionFlag.P)) →
     reg' = reg)
    := by
  unfold op_br; simp only []
  split
  · trivial
  · rename_i reg' mem' heq
    by_cases h_cond : ((instr >>> 11).land 1 == 1 && reg.cond == ConditionFlag.N ||
        (instr >>> 10).land 1 == 1 && reg.cond == ConditionFlag.Z ||
        (instr >>> 9).land 1 == 1 && reg.cond == ConditionFlag.P) = true
    · rw [if_pos h_cond] at heq
      obtain ⟨h1, h2⟩ := Prod.mk.inj (Option.some.inj heq)
      subst h1; subst h2
      simp [Bool.or_eq_true, Bool.and_eq_true, cond_beq_eq] at h_cond
      refine ⟨rfl, fun _ => rfl, ?_⟩
      intro h_not; exfalso
      rcases h_cond with (⟨h1, h2⟩ | ⟨h1, h2⟩) | ⟨h1, h2⟩
      · exact h_not (Or.inl ⟨h1, h2⟩)
      · exact h_not (Or.inr (Or.inl ⟨h1, h2⟩))
      · exact h_not (Or.inr (Or.inr ⟨h1, h2⟩))
    · rw [if_neg h_cond] at heq
      obtain ⟨h1, h2⟩ := Prod.mk.inj (Option.some.inj heq)
      subst h1; subst h2
      refine ⟨rfl, ?_, fun _ => rfl⟩
      intro h_prop; exfalso; apply h_cond
      simp [Bool.or_eq_true, Bool.and_eq_true, cond_beq_eq]
      rcases h_prop with ⟨h1, h2⟩ | ⟨h1, h2⟩ | ⟨h1, h2⟩
      · exact Or.inl (Or.inl ⟨h1, h2⟩)
      · exact Or.inl (Or.inr ⟨h1, h2⟩)
      · exact Or.inr ⟨h1, h2⟩

end BR

section JMP

theorem op_jmp_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_jmp instr reg mem with
  | none => true
  | some (reg', mem') =>
    let base_r := (instr >>> 6).land 0x7
    mem' = mem ∧
    (∀ base_val, Registers.read reg base_r = some base_val →
      reg' = { reg with pc := base_val })
    := by
  unfold op_jmp; simp
  cases Registers.read reg ((instr >>> 6).land 7) with
  | none => simp
  | some base_val => simp

end JMP

section JSR

theorem op_jsr_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_jsr instr reg mem with
  | none => true
  | some (reg', mem') =>
    let mode := (instr >>> 11).land 0x1
    let base_r := (instr >>> 6).land 0x7
    let offset := sign_extend (instr.land 0x7FF) 11
    mem' = mem ∧
    (mode = 0 →
      ∀ base_val reg_r7,
      Registers.read reg base_r = some base_val →
      Registers.write reg 7 reg.pc = some reg_r7 →
      reg' = { reg_r7 with pc := base_val }) ∧
    (mode = 1 →
      ∀ reg_r7,
      Registers.write reg 7 reg.pc = some reg_r7 →
      reg' = { reg_r7 with pc := reg_r7.pc + offset })
    := by
  unfold op_jsr; simp only []
  split
  · trivial
  · rename_i reg' mem' heq
    by_cases h_mode : ((instr >>> 11).land 1 == 0) = true
    · simp [h_mode] at heq
      cases h_read : Registers.read reg ((instr >>> 6).land 7) with
      | none => simp [h_read] at heq
      | some base_val =>
        simp [h_read] at heq
        cases h_write : Registers.write reg 7 reg.pc with
        | none => simp [h_write] at heq
        | some reg_r7 =>
          simp [h_write] at heq
          obtain ⟨h1, h2⟩ := heq; subst h1; subst h2
          simp [beq_iff_eq] at h_mode
          exact ⟨rfl, fun _ _ _ hr hw => by simp_all,
                 fun h => absurd h (by rw [h_mode]; simp)⟩
    · simp [h_mode] at heq
      cases h_write : Registers.write reg 7 reg.pc with
      | none => simp [h_write] at heq
      | some reg_r7 =>
        simp [h_write] at heq
        obtain ⟨h1, h2⟩ := heq; subst h1; subst h2
        simp [beq_iff_eq] at h_mode
        exact ⟨rfl, fun h => absurd h h_mode,
               fun _ _ hw => by simp_all⟩

end JSR

section LD

theorem op_ld_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_ld instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    let value := Memory.read mem addr
    mem' = mem ∧
    (∃ reg_temp, Registers.write reg dr value = some reg_temp ∧
      reg' = set_condition_codes reg_temp value)
    := by
  unfold op_ld; simp
  cases Registers.write reg ((instr >>> 9).land 7) (Memory.read mem (reg.pc + sign_extend (instr.land 511) 9)) with
  | none => simp
  | some reg_written => simp

end LD

section LDI

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
    (∃ reg_temp, Registers.write reg dr value = some reg_temp ∧
      reg' = set_condition_codes reg_temp value)
    := by
  unfold op_ldi; simp
  cases Registers.write reg ((instr >>> 9).land 7) (Memory.read mem (Memory.read mem (reg.pc + sign_extend (instr.land 511) 9))) with
  | none => simp
  | some reg_written => simp

end LDI

section LDR

theorem op_ldr_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_ldr instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let base_r := (instr >>> 6).land 0x7
    let offset := sign_extend (instr.land 0x3F) 6
    mem' = mem ∧
    (∀ base_val, Registers.read reg base_r = some base_val →
      let addr := base_val + offset
      let value := Memory.read mem addr
      ∃ reg_temp, Registers.write reg dr value = some reg_temp ∧
        reg' = set_condition_codes reg_temp value)
    := by
  unfold op_ldr; simp
  cases Registers.read reg ((instr >>> 6).land 7) with
  | none => simp
  | some base_val =>
    simp
    cases Registers.write reg ((instr >>> 9).land 7) (Memory.read mem (base_val + sign_extend (instr.land 63) 6)) with
    | none => simp
    | some reg_written => simp

end LDR

section LEA

theorem op_lea_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_lea instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    mem' = mem ∧
    (∃ reg_temp, Registers.write reg dr addr = some reg_temp ∧
      reg' = set_condition_codes reg_temp addr)
    := by
  unfold op_lea; simp
  cases Registers.write reg ((instr >>> 9).land 7) (reg.pc + sign_extend (instr.land 511) 9) with
  | none => simp
  | some reg_written => simp

end LEA

section NOT

theorem op_not_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_not instr reg mem with
  | none => true
  | some (reg', mem') =>
    let dr := (instr >>> 9).land 0x7
    let sr := (instr >>> 6).land 0x7
    mem' = mem ∧
    (∀ val, Registers.read reg sr = some val →
      let result := val.complement
      ∃ reg_temp, Registers.write reg dr result = some reg_temp ∧
        reg' = set_condition_codes reg_temp result)
    := by
  unfold op_not; simp
  cases Registers.read reg ((instr >>> 6).land 7) with
  | none => simp
  | some val =>
    simp
    cases Registers.write reg ((instr >>> 9).land 7) val.complement with
    | none => simp
    | some reg_written => simp

end NOT

section ST

theorem op_st_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_st instr reg mem with
  | none => true
  | some (reg', mem') =>
    let sr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    reg' = reg ∧
    (∀ value, Registers.read reg sr = some value →
      mem' = Memory.write mem addr value)
    := by
  unfold op_st; simp
  cases Registers.read reg ((instr >>> 9).land 7) with
  | none => simp
  | some value => simp

end ST

section STI

theorem op_sti_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_sti instr reg mem with
  | none => true
  | some (reg', mem') =>
    let sr := (instr >>> 9).land 0x7
    let offset := sign_extend (instr.land 0x1FF) 9
    let addr := reg.pc + offset
    let indirect_addr := Memory.read mem addr
    reg' = reg ∧
    (∀ value, Registers.read reg sr = some value →
      mem' = Memory.write mem indirect_addr value)
    := by
  unfold op_sti; simp
  cases Registers.read reg ((instr >>> 9).land 7) with
  | none => simp
  | some value => simp

end STI

section STR

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
    := by
  unfold op_str; simp
  cases Registers.read reg ((instr >>> 6).land 7) with
  | none => simp
  | some base_val =>
    simp
    cases Registers.read reg ((instr >>> 9).land 7) with
    | none => simp
    | some value => simp

end STR

section TRAP

theorem op_trap_spec (instr : UInt16) (reg : Register) (mem : Memory) :
  match op_trap instr reg mem with
  | none => true
  | some (reg', mem') =>
    let trap_vector := instr.land 0xFF
    let pc := Memory.read mem trap_vector
    mem' = mem ∧
    (∃ reg_temp, Registers.write reg 7 reg.pc = some reg_temp ∧
      reg' = { reg_temp with pc := pc })
    := by
  unfold op_trap; simp
  cases Registers.write reg 7 reg.pc with
  | none => simp
  | some reg_temp => simp

end TRAP
