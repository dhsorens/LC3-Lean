-- memory model defined by an array (or a map)
namespace VM.Memory

structure Memory where
  data : Array UInt16 := Array.mkArray (2 ^ 16) (0 : UInt16)
  deriving Repr, DecidableEq, BEq

def init : Memory :=
  { data := Array.mkArray (2 ^ 16) (0 : UInt16) }

def read (mem : Memory) (addr : UInt16) : UInt16 :=
  mem.data.get! addr.val

def write (mem : Memory) (addr : UInt16) (val : UInt16) : Memory :=
  { data := mem.data.set! addr.val val }

end VM.Memory
