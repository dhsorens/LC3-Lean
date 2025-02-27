-- entrypoint for running the VM
import LC3Lean.Memory
import LC3Lean.Registers
import LC3Lean.Instructions
import LC3Lean.Execution
import LC3Lean.Trap

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

def load_into_mem (file : ByteArray) (mem : Memory) : Option Memory := do
  -- Need at least 2 bytes for origin
  if file.size < 2 then none else
  -- Get origin from first two bytes and swap endianness
  let o := UInt16.ofNat (file.get! 0).toNat <<< 8 ||| UInt16.ofNat (file.get! 1).toNat
  let origin := swap16 o
  --
  let mut memory := mem
  let mut addr := origin
  let mut idx := 2
  -- Process two bytes at a time until end of file
  while idx + 1 < file.size do
    -- Read two bytes and combine into a word
    let byte1 := file.get! idx
    let byte2 := file.get! (idx + 1)
    let word := UInt16.ofNat byte1.toNat <<< 8 ||| UInt16.ofNat byte2.toNat
    -- Write the word to memory (already in correct endianness)
    memory := Memory.write memory addr word
    addr := addr + 1
    idx := idx + 2
  --
  some memory

def load_file (file_path : String) (mem : Memory) : IO Memory := do
  let file ← IO.FS.readBinFile file_path
  match load_into_mem file mem with
  | some m => pure m
  | none => throw $ IO.userError "Error: Input file could not be processed"

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
