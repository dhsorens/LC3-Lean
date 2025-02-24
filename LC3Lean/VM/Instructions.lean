-- Define opcodes and their behavior.

namespace VM.Instructions

-- opcodes
inductive Opcodes
| OP_BR   -- branch
| OP_ADD  -- add
| OP_LD   -- load
| OP_ST   -- store
| OP_JSR  -- jump register
| OP_AND  -- bitwise and
| OP_LDR  -- load register
| OP_STR  -- store register
| OP_RTI  -- unused
| OP_NOT  -- bitwise not
| OP_LDI  -- load indirect
| OP_STI  -- store indirect
| OP_JMP  -- jump
| OP_RES  -- reserved (unused)
| OP_LEA  -- load effective address
| OP_TRAP -- execute trap
deriving Repr, DecidableEq, BEq

-- auxiliary functions for op codes
def todo {A : Type} : A := sorry

def instr_op (instr : UInt16) : VM.Instructions.Opcodes := todo

end VM.Instructions
