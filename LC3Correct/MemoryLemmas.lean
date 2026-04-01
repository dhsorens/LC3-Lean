import LC3Lean.Memory

open Memory

-- Helper lemmas for reasoning about memory read/write operations.

-- UInt16 values are always in bounds for our 2^16-sized memory array.
theorem addr_lt_size (mem : Memory) (addr : UInt16) :
    addr.toNat < mem.data.size := by
  have h_size := mem.data_size
  simp [MEMORY_MAX] at h_size; rw [h_size]; exact UInt16.toNat_lt addr

section read_write

theorem mem_read_write_same (mem : Memory) (addr : UInt16) (val : UInt16) :
    Memory.read (Memory.write mem addr val) addr = val := by
  simp only [Memory.read, Memory.write]
  have h := addr_lt_size mem addr
  simp_all [Array.set!_eq_setIfInBounds]

theorem mem_read_write_other (mem : Memory) (addr1 addr2 : UInt16) (val : UInt16)
    (h : addr1 ≠ addr2) :
    Memory.read (Memory.write mem addr1 val) addr2 = Memory.read mem addr2 := by
  simp only [Memory.read, Memory.write]
  have h1 := addr_lt_size mem addr1
  have h2 := addr_lt_size mem addr2
  have h_ne : addr1.toNat ≠ addr2.toNat := by
    intro heq; exact h (UInt16.toNat_inj.mp heq)
  simp_all [Array.set!_eq_setIfInBounds]

end read_write
