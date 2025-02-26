-- entrypoint for running the VM
import LC3Lean.VM.Memory
import LC3Lean.VM.Registers
import LC3Lean.VM.Instructions
import LC3Lean.VM.Trap
import LC3Lean.VM.Execution

open VM.Memory
open VM.Trap

section aux

  def swap16 (x : UInt16) : UInt16 :=
    (x <<< 8) ||| (x >>> 8)

  def stringToUInt16 (s : String) : Option UInt16 :=
    match s.toNat? with
    | some n =>
      if n <= VM.Memory.MEMORY_MAX then
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
  while idx < (VM.Memory.MEMORY_MAX - origin.toNat) do
    let byte1 := file.toList.get! idx
    let byte2 := file.toList.get! (idx + 1)
    let word := swap16 (VM.Trap.char_to_uint16 byte1) <<< 8 ||| swap16 (VM.Trap.char_to_uint16 byte2)
    memory := VM.Memory.write memory addr word
    addr := addr + 1
    idx := idx + 2
  --
  some mem

def load_file (file_name : String) (mem : Memory) : IO Memory := do
  let file ← IO.FS.readFile file_name
  match load_into_mem file mem with
  | some m => pure m
  | none => throw $ IO.userError "Error: Input could not be processed"

def execute_loop : IO Unit := do
  -- initialize register and memory
  let mut reg := VM.Registers.init
  let mut mem := VM.Memory.init
  -- loop until
  while true do
    match VM.Execution.execute_step reg mem with
    | some (new_reg, new_mem) =>
      reg := new_reg
      mem := new_mem
    | none =>
      break
