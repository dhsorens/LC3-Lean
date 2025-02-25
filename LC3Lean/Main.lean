-- entrypoint for running the VM
import LC3Lean.VM.Memory
import LC3Lean.VM.Registers
import LC3Lean.VM.Instructions
import LC3Lean.VM.Execution


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
