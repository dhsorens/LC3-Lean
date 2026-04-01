import LC3Lean.Memory

open Memory

-- Helper lemmas for reasoning about memory read/write operations.

section read_write

theorem mem_read_write_same (mem : Memory) (addr : UInt16) (val : UInt16) :
    Memory.read (Memory.write mem addr val) addr = val := by
  unfold Memory.read Memory.write
  simp

theorem mem_read_write_other (mem : Memory) (addr1 addr2 : UInt16) (val : UInt16)
    (h : addr1 ≠ addr2) :
    Memory.read (Memory.write mem addr1 val) addr2 = Memory.read mem addr2 := by
  unfold Memory.read Memory.write
  simp
  sorry -- requires Array.get_set_ne lemma with UInt16.toFin

end read_write
