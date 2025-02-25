-- Define opcodes and their behavior.

namespace VM.Instructions

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
def instr_to_op (instr : UInt16) : Option VM.Instructions.Opcodes :=
  if      ((instr.shiftRight 12).land 0xF) == 0x0001 then some .OP_ADD
  else if ((instr.shiftRight 12).land 0xF) == 0x0101 then some .OP_AND
  else if ((instr.shiftRight 12).land 0xF) == 0x0000 then some .OP_BR
  else if ((instr.shiftRight 12).land 0xF) == 0x1100 then some .OP_JMP -- .OP_RET
  else if ((instr.shiftRight 12).land 0xF) == 0x0100 then some .OP_JSR
  else if ((instr.shiftRight 12).land 0xF) == 0x0010 then some .OP_LD
  else if ((instr.shiftRight 12).land 0xF) == 0x1010 then some .OP_LDI
  else if ((instr.shiftRight 12).land 0xF) == 0x0110 then some .OP_LDR
  else if ((instr.shiftRight 12).land 0xF) == 0x1110 then some .OP_LEA
  else if ((instr.shiftRight 12).land 0xF) == 0x1001 then some .OP_NOT
  else if ((instr.shiftRight 12).land 0xF) == 0x1000 then some .OP_RTI
  else if ((instr.shiftRight 12).land 0xF) == 0x0011 then some .OP_ST
  else if ((instr.shiftRight 12).land 0xF) == 0x1011 then some .OP_STI
  else if ((instr.shiftRight 12).land 0xF) == 0x0111 then some .OP_STR
  else if ((instr.shiftRight 12).land 0xF) == 0x1111 then some .OP_TRAP
  else if ((instr.shiftRight 12).land 0xF) == 0x1101 then some .OP_RES -- unused
  else none


end VM.Instructions
