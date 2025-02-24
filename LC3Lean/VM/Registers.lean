-- Implement general-purpose registers, program counter, and condition flags.

namespace VM.Registers

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

def init : Register :=
  { r := #[0, 0, 0, 0, 0, 0, 0, 0]
    pc := 0
    cond := ConditionFlag.Z }

def read (regs : Register) (index : Registers) : Option UInt16 :=
  match index with
  | .R_R0 => some (regs.r.get! 0)  -- get general-purpose registers
  | .R_R1 => some (regs.r.get! 1)
  | .R_R2 => some (regs.r.get! 2)
  | .R_R3 => some (regs.r.get! 3)
  | .R_R4 => some (regs.r.get! 4)
  | .R_R5 => some (regs.r.get! 5)
  | .R_R6 => some (regs.r.get! 6)
  | .R_R7 => some (regs.r.get! 7)
  | .R_PC => -- program counter
      some regs.pc  -- get the program counter
  | .R_COND =>
      match regs.cond with -- get the condition flags
      | ConditionFlag.N => some 0 -- negative
      | ConditionFlag.Z => some 1 -- zero
      | ConditionFlag.P => some 2 -- positive
  | .R_COUNT => none

def write (regs : Register) (index : Registers) (value : UInt16) : Option Register :=
  match index with
  -- update general-purpose register
  | .R_R0 => some { regs with r := regs.r.set! 0 value }
  | .R_R1 => some { regs with r := regs.r.set! 1 value }
  | .R_R2 => some { regs with r := regs.r.set! 2 value }
  | .R_R3 => some { regs with r := regs.r.set! 3 value }
  | .R_R4 => some { regs with r := regs.r.set! 4 value }
  | .R_R5 => some { regs with r := regs.r.set! 5 value }
  | .R_R6 => some { regs with r := regs.r.set! 6 value }
  | .R_R7 => some { regs with r := regs.r.set! 7 value }
  | .R_PC => -- program counter
      some { regs with pc := value }  -- update program counter
  | .R_COND =>
      if value = 0 then
        some { regs with cond := ConditionFlag.N }  -- negative
      else if value = 1 then
        some { regs with cond := ConditionFlag.Z }  -- zero
      else if value = 2 then
        some { regs with cond := ConditionFlag.P }  -- positive
      else none
  | .R_COUNT => none

def read2 (regs : Register) (index : Nat) : Option UInt16 :=
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

def write2 (regs : Register) (index : Nat) (value : UInt16) : Option Register :=
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
