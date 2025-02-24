-- Implement general-purpose registers, program counter, and condition flags.

namespace VM.Registers

inductive ConditionFlag
| P -- positive
| Z -- zero
| N -- negative
deriving Repr, DecidableEq, BEq

structure Register where
  r : Array UInt16 := #[0, 0, 0, 0, 0, 0, 0, 0]  -- 8 general-purpose registers
  pc : UInt16 -- program counter
  cond : ConditionFlag -- condition flags register (N, Z, P)
  deriving Repr, DecidableEq, BEq

def init : Register :=
  { r := #[0, 0, 0, 0, 0, 0, 0, 0]
    pc := 0
    cond := ConditionFlag.Z }

def read (regs : Register) (index : Nat) : Option UInt16 :=
  if index < 8 then
    some (regs.r.get! index)  -- get general-purpose registers
  else if index = 8 then
    some regs.pc  -- get the program counter
  else if index = 9 then
    match regs.cond with -- get the condition flags
    | ConditionFlag.N => some 0 -- negative
    | ConditionFlag.Z => some 1 -- zero
    | ConditionFlag.P => some 2 -- positive
  else
    none  -- invalid register index

def write (regs : Register) (index : Nat) (value : UInt16) : Option Register :=
  if index < 8 then
    some { regs with r := regs.r.set! index value }  -- update general-purpose register
  else if index = 8 then
    some { regs with pc := value }  -- update program counter
  else if index = 9 then
    if value = 0 then
      some { regs with cond := ConditionFlag.N }  -- negative
    else if value = 1 then
      some { regs with cond := ConditionFlag.Z }  -- zero
    else if value = 2 then
      some { regs with cond := ConditionFlag.P }  -- positive
    else none
  else
    regs  -- invalid register, return unchanged

end VM.Registers
