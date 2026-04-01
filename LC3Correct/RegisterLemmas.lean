import LC3Lean.Registers

open Registers

-- Helper lemmas for reasoning about register read/write operations.

-- Valid GP register indices are 0-7.
-- Registers.read and Registers.write go through uint16_to_reg, which
-- maps 0-7 to R_R0..R_R7. We prove that read/write succeed for these indices
-- and establish read-after-write properties.

section validity

theorem read_valid_0 (reg : Register) : ∃ v, Registers.read reg 0 = some v := by
  unfold Registers.read Registers.uint16_to_reg; simp

theorem read_valid_1 (reg : Register) : ∃ v, Registers.read reg 1 = some v := by
  unfold Registers.read Registers.uint16_to_reg; simp

theorem read_valid_2 (reg : Register) : ∃ v, Registers.read reg 2 = some v := by
  unfold Registers.read Registers.uint16_to_reg; simp

theorem read_valid_3 (reg : Register) : ∃ v, Registers.read reg 3 = some v := by
  unfold Registers.read Registers.uint16_to_reg; simp

theorem read_valid_4 (reg : Register) : ∃ v, Registers.read reg 4 = some v := by
  unfold Registers.read Registers.uint16_to_reg; simp

theorem read_valid_5 (reg : Register) : ∃ v, Registers.read reg 5 = some v := by
  unfold Registers.read Registers.uint16_to_reg; simp

theorem read_valid_6 (reg : Register) : ∃ v, Registers.read reg 6 = some v := by
  unfold Registers.read Registers.uint16_to_reg; simp

theorem read_valid_7 (reg : Register) : ∃ v, Registers.read reg 7 = some v := by
  unfold Registers.read Registers.uint16_to_reg; simp

theorem write_valid_0 (reg : Register) (v : UInt16) :
    ∃ reg', Registers.write reg 0 v = some reg' := by
  unfold Registers.write Registers.uint16_to_reg; simp

theorem write_valid_1 (reg : Register) (v : UInt16) :
    ∃ reg', Registers.write reg 1 v = some reg' := by
  unfold Registers.write Registers.uint16_to_reg; simp

theorem write_valid_2 (reg : Register) (v : UInt16) :
    ∃ reg', Registers.write reg 2 v = some reg' := by
  unfold Registers.write Registers.uint16_to_reg; simp

theorem write_valid_3 (reg : Register) (v : UInt16) :
    ∃ reg', Registers.write reg 3 v = some reg' := by
  unfold Registers.write Registers.uint16_to_reg; simp

theorem write_valid_4 (reg : Register) (v : UInt16) :
    ∃ reg', Registers.write reg 4 v = some reg' := by
  unfold Registers.write Registers.uint16_to_reg; simp

theorem write_valid_5 (reg : Register) (v : UInt16) :
    ∃ reg', Registers.write reg 5 v = some reg' := by
  unfold Registers.write Registers.uint16_to_reg; simp

theorem write_valid_6 (reg : Register) (v : UInt16) :
    ∃ reg', Registers.write reg 6 v = some reg' := by
  unfold Registers.write Registers.uint16_to_reg; simp

theorem write_valid_7 (reg : Register) (v : UInt16) :
    ∃ reg', Registers.write reg 7 v = some reg' := by
  unfold Registers.write Registers.uint16_to_reg; simp

end validity

section read_write

-- read after write to the same GP register index returns the written value
theorem read_write_same_0 (reg : Register) (v : UInt16) (reg' : Register) :
    Registers.write reg 0 v = some reg' → Registers.read reg' 0 = some v := by
  unfold Registers.write Registers.read Registers.uint16_to_reg; simp; intro h; rw [← h]; simp

theorem read_write_same_1 (reg : Register) (v : UInt16) (reg' : Register) :
    Registers.write reg 1 v = some reg' → Registers.read reg' 1 = some v := by
  unfold Registers.write Registers.read Registers.uint16_to_reg; simp; intro h; rw [← h]; simp

theorem read_write_same_2 (reg : Register) (v : UInt16) (reg' : Register) :
    Registers.write reg 2 v = some reg' → Registers.read reg' 2 = some v := by
  unfold Registers.write Registers.read Registers.uint16_to_reg; simp; intro h; rw [← h]; simp

theorem read_write_same_3 (reg : Register) (v : UInt16) (reg' : Register) :
    Registers.write reg 3 v = some reg' → Registers.read reg' 3 = some v := by
  unfold Registers.write Registers.read Registers.uint16_to_reg; simp; intro h; rw [← h]; simp

theorem read_write_same_4 (reg : Register) (v : UInt16) (reg' : Register) :
    Registers.write reg 4 v = some reg' → Registers.read reg' 4 = some v := by
  unfold Registers.write Registers.read Registers.uint16_to_reg; simp; intro h; rw [← h]; simp

theorem read_write_same_5 (reg : Register) (v : UInt16) (reg' : Register) :
    Registers.write reg 5 v = some reg' → Registers.read reg' 5 = some v := by
  unfold Registers.write Registers.read Registers.uint16_to_reg; simp; intro h; rw [← h]; simp

theorem read_write_same_6 (reg : Register) (v : UInt16) (reg' : Register) :
    Registers.write reg 6 v = some reg' → Registers.read reg' 6 = some v := by
  unfold Registers.write Registers.read Registers.uint16_to_reg; simp; intro h; rw [← h]; simp

theorem read_write_same_7 (reg : Register) (v : UInt16) (reg' : Register) :
    Registers.write reg 7 v = some reg' → Registers.read reg' 7 = some v := by
  unfold Registers.write Registers.read Registers.uint16_to_reg; simp; intro h; rw [← h]; simp

end read_write
