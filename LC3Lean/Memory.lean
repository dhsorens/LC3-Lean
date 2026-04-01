-- memory model defined by an array (or a map)
namespace Memory

def MEMORY_MAX := 2 ^ 16

structure Memory where
  data : Array UInt16 := Array.replicate MEMORY_MAX (0 : UInt16)
  data_size : data.size = MEMORY_MAX := by simp [Array.size_replicate]

def init : Memory :=
  { data := Array.replicate (2 ^ 16) (0 : UInt16)
    data_size := by simp [Array.size_replicate, MEMORY_MAX] }

def read (mem : Memory) (addr : UInt16) : UInt16 :=
  mem.data[addr.toFin]!

def write (mem : Memory) (addr : UInt16) (val : UInt16) : Memory :=
  { data := mem.data.set! addr.toFin val
    data_size := by simp [mem.data_size] }

end Memory
