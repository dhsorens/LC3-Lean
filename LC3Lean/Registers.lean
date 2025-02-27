-- Implement general-purpose registers, program counter, and condition flags.

namespace Registers

inductive Registers :=
| R_R0
| R_R1
| R_R2
| R_R3
| R_R4
| R_R5
| R_R6
| R_R7
| R_PC -- program counter
| R_COND
| R_COUNT
deriving Repr, DecidableEq, BEq

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

def init : Register := {
    r := #[0, 0, 0, 0, 0, 0, 0, 0]
    pc := 0x3000 -- lower memory slots used for trap routines
    cond := ConditionFlag.Z -- condition flag set initially to zero
  }

def uint16_to_reg (i : UInt16) : Option Registers :=
  if (i == 0) then some .R_R0 else
  if (i == 1) then some .R_R1 else
  if (i == 2) then some .R_R2 else
  if (i == 3) then some .R_R3 else
  if (i == 4) then some .R_R4 else
  if (i == 5) then some .R_R5 else
  if (i == 6) then some .R_R6 else
  if (i == 7) then some .R_R7 else
  if (i == 8) then some .R_PC else
  if (i == 9) then some .R_COND else
  if (i == 10) then some .R_COUNT else
  none

def read (reg : Register) (index : UInt16) : Option UInt16 := do
  let index' ← uint16_to_reg index
  match index' with
  | .R_R0 => some (reg.r.get! 0)  -- get general-purpose registers
  | .R_R1 => some (reg.r.get! 1)
  | .R_R2 => some (reg.r.get! 2)
  | .R_R3 => some (reg.r.get! 3)
  | .R_R4 => some (reg.r.get! 4)
  | .R_R5 => some (reg.r.get! 5)
  | .R_R6 => some (reg.r.get! 6)
  | .R_R7 => some (reg.r.get! 7)
  | .R_PC => -- program counter
      some reg.pc  -- get the program counter
  | .R_COND =>
      match reg.cond with -- get the condition flags
      | ConditionFlag.N => some 0 -- negative
      | ConditionFlag.Z => some 1 -- zero
      | ConditionFlag.P => some 2 -- positive
  | .R_COUNT => none

def write (reg : Register) (index : UInt16) (value : UInt16) : Option Register := do
  let index' ← uint16_to_reg index
  match index' with
  -- update general-purpose register
  | .R_R0 => some { reg with r := reg.r.set! 0 value }
  | .R_R1 => some { reg with r := reg.r.set! 1 value }
  | .R_R2 => some { reg with r := reg.r.set! 2 value }
  | .R_R3 => some { reg with r := reg.r.set! 3 value }
  | .R_R4 => some { reg with r := reg.r.set! 4 value }
  | .R_R5 => some { reg with r := reg.r.set! 5 value }
  | .R_R6 => some { reg with r := reg.r.set! 6 value }
  | .R_R7 => some { reg with r := reg.r.set! 7 value }
  | .R_PC => -- program counter
      some { reg with pc := value }  -- update program counter
  | .R_COND =>
      if value = 0 then
        some { reg with cond := ConditionFlag.N }  -- negative
      else if value = 1 then
        some { reg with cond := ConditionFlag.Z }  -- zero
      else if value = 2 then
        some { reg with cond := ConditionFlag.P }  -- positive
      else none
  | .R_COUNT => none

end Registers
