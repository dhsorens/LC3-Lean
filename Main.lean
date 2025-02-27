-- entrypoint for running the VM
import LC3Lean.Memory
import LC3Lean.Registers
import LC3Lean.Instructions
import LC3Lean.Trap
import LC3Lean.Execution

open Memory
open Trap

section aux

  def swap16 (x : UInt16) : UInt16 :=
    (x <<< 8) ||| (x >>> 8)

  def stringToUInt16 (s : String) : Option UInt16 :=
    match s.toNat? with
    | some n =>
      if n <= Memory.MEMORY_MAX then
        some (UInt16.ofNat n)
      else
        none
    | none => none

end aux

def load_into_mem (file : String) (mem : Memory) : Option Memory := do
  let o ← stringToUInt16 (file.take 2)
  let origin := swap16 o
  --
  let mut memory := mem
  let mut addr := origin
  let mut idx := 2
  while idx < (Memory.MEMORY_MAX - origin.toNat) do
    let byte1 := file.toList.get! idx
    let byte2 := file.toList.get! (idx + 1)
    let word := swap16 (Trap.char_to_uint16 byte1) <<< 8 ||| swap16 (Trap.char_to_uint16 byte2)
    memory := Memory.write memory addr word
    addr := addr + 1
    idx := idx + 2
  --
  some mem

def load_file (file_path : String) (mem : Memory) : IO Memory := do
  let file ← IO.FS.readFile file_path
  match load_into_mem file mem with
  | some m => pure m
  | none => throw $ IO.userError "Error: Input could not be processed"

-- the main function
def main (file_paths : List String) : IO Unit := do
  -- initialize register and memory
  let mut reg := Registers.init
  let mut mem := Memory.init
  -- load the code into memory to execute
  let file_path := file_paths.get! 0
  mem ← load_file file_path mem
  -- loop until
  while true do
    match Execution.execute_step reg mem with
    | some (new_reg, new_mem) =>
      reg := new_reg
      mem := new_mem
    | none =>
      break
