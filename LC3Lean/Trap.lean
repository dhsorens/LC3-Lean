import LC3Lean.Memory
import LC3Lean.Registers

namespace Trap
open Memory
open Registers


section aux

  def get_char_from_terminal : IO Char := do
    let handle ← IO.getStdin
    let input ← handle.getLine -- read one 32-bit character
    match input.toList with
    | c :: _ => pure c  -- Return the first character
    | [] => throw $ IO.userError "Error: No input provided."

  def char_to_uint16 (c : Char) : UInt16 := UInt16.ofNat (c.val.toNat)
  def uint16_to_char (u : UInt16) : Char := Char.ofNat (u.toNat)

  def clear_high_bits (b : UInt16) : UInt16 := b.land 0x00FF
  def clear_low_bits (b : UInt16) : UInt16 := b.land 0xFF00

end aux

-- Write a string of ASCII characters to the console display.
-- The characters are contained in consecutive memory locations,
--  one character per memory location, starting with the address specified in R0.
-- Writing terminates with the occurrence of x0000 in a memory location.
def trap_puts (reg : Register) (mem : Memory) : IO Unit := do
  let addr ← match (Registers.read reg 0) with
    | some a => pure a
    | none => throw $ IO.userError "Nothing in storage"
  let mut c := Memory.read mem addr
  let mut index := addr
  while c != 0 do
    IO.print (uint16_to_char c)
    index := index + 1
    c := Memory.read mem index

  -- Read a single character from the keyboard.
  -- The character is not echoed onto the console.
  -- Its ASCII code is copied into R0.
  -- The high eight bits of R0 are cleared.
def trap_getc (reg : Register) : IO Register := do
  let c ← get_char_from_terminal
  let reg := Registers.write reg 0 (clear_high_bits (char_to_uint16 c))
  match reg with
  | some r =>  pure r
  | none => throw $ IO.userError "Error: No input provided."

-- Write a character in R0[7:0] to the console display.
def trap_out (reg : Register) : IO Unit := do
  match Registers.read reg 0 with
  | some char => IO.print (uint16_to_char (clear_high_bits char))
  | none => throw $ IO.userError "Error: Unable to access storage."

-- Print a prompt on the screen and read a single character from the keyboard.
-- The character is echoed onto the console monitor, and its ASCII code is copied into R0.
-- The high eight bits of R0 are cleared
def trap_in (reg : Register) : IO Register := do
  IO.print "Enter a character: "
  let c ← get_char_from_terminal
  IO.print c
  match Registers.write reg 0 (clear_high_bits (char_to_uint16 c)) with
  | some r => pure r
  | none => throw $ IO.userError "Error: Unable to access storage."

-- Write a string of ASCII characters to the console.
-- The characters are contained in consecutive memory locations, two characters per memory location, starting with the address specified in R0.
-- The ASCII code contained in bits [7:0] of a memory location is written to the console first.
-- Then the ASCII code contained in bits [15:8] of that memory location is written to the console.
-- (A character string consisting of an odd number of characters to be written will have x00 in bits [15:8] of the memory location containing the last character to be written.)
-- Writing terminates with the occurrence of x0000 in a memory location.
def trap_putsp (mem : Memory) (reg : Register) : IO Unit := do
  let addr ← match (Registers.read reg 0) with
    | some a => pure a
    | none => throw $ IO.userError "Nothing in storage"
  let mut c := Memory.read mem addr
  let mut index := addr
  while c != 0 do
    let c1 := clear_high_bits c
    IO.print (uint16_to_char c1)
    let c2 := clear_low_bits c
    if c2 == 0 then c := 0 else
    IO.print (uint16_to_char c2)
    index := index + 1
    c := Memory.read mem index

-- Halt execution and print a message on the console.
def trap_halt (running : IO.Ref Bool) : IO Unit := do
  IO.println "HALT"
  running.set false


-- an IO version of the trap function
inductive Trapcodes
| TRAP_PUTS -- 0x22  -- output a word string
| TRAP_GETC -- 0x20  -- get character from keyboard, not echoed onto the terminal
| TRAP_OUT -- 0x21   -- output a character
| TRAP_IN -- 0x23    -- get character from keyboard, echoed onto the terminal
| TRAP_PUTSP -- 0x24 -- output a byte string
| TRAP_HALT -- 0x25   -- halt the program
deriving Repr, DecidableEq, BEq

def op_trap_io (instr : Trapcodes) (reg : Register) (mem : Memory) :
  IO (Register × Memory) := do
  match instr with
  | .TRAP_PUTS =>
    trap_puts reg mem
    pure (reg,mem)
  | .TRAP_GETC =>
    let reg ← trap_getc reg
    pure (reg,mem)
  | .TRAP_OUT =>
    trap_out reg
    pure (reg,mem)
  | .TRAP_IN =>
    let reg ← trap_in reg
    pure (reg,mem)
  | .TRAP_PUTSP =>
    trap_putsp mem reg
    pure (reg,mem)
  | .TRAP_HALT =>
    let running ← IO.mkRef true
    trap_halt running
    pure (reg,mem)



end Trap
