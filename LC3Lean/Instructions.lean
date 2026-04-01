-- Define opcodes and their behavior.

namespace Instructions

-- opcodes
inductive Opcodes
| OP_ADD  -- add
| OP_AND  -- bitwise and
| OP_BR   -- branch
| OP_JMP  -- jump
| OP_JSR  -- jump register
| OP_LD   -- load
| OP_LDI  -- load indirect
| OP_LDR  -- load register
| OP_LEA  -- load effective address
| OP_NOT  -- bitwise not
| OP_RTI  -- unused
| OP_ST   -- store
| OP_STI  -- store indirect
| OP_STR  -- store register
| OP_TRAP -- execute trap
| OP_RES  -- reserved (unused)
deriving Repr, DecidableEq, BEq

-- auxiliary functions for op codes

--   takes
def instr_to_op (instr : UInt16) : Option Instructions.Opcodes :=
  match (instr.shiftRight 12).land 0xF with
  | 0  => some .OP_BR
  | 1  => some .OP_ADD
  | 2  => some .OP_LD
  | 3  => some .OP_ST
  | 4  => some .OP_JSR
  | 5  => some .OP_AND
  | 6  => some .OP_LDR
  | 7  => some .OP_STR
  | 8  => some .OP_RTI
  | 9  => some .OP_NOT
  | 10 => some .OP_LDI
  | 11 => some .OP_STI
  | 12 => some .OP_JMP
  | 13 => some .OP_RES
  | 14 => some .OP_LEA
  | 15 => some .OP_TRAP
  | _  => none


end Instructions
