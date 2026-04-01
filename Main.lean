-- entrypoint for running the VM
import LC3Lean.Memory
import LC3Lean.Registers
import LC3Lean.Instructions
import LC3Lean.Execution
import LC3Lean.Trap
import LC3Lean.Terminal

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
  -- Get origin from first two bytes (big-endian)
  let origin := UInt16.ofNat (file.get! 0).toNat <<< 8 ||| UInt16.ofNat (file.get! 1).toNat
  --
  let mut memory := mem
  let mut addr := origin
  let mut idx := 2
  -- Process two bytes at a time until end of file
  while idx + 1 < file.size do
    let byte1 := file.get! idx
    let byte2 := file.get! (idx + 1)
    let word := UInt16.ofNat byte1.toNat <<< 8 ||| UInt16.ofNat byte2.toNat
    memory := Memory.write memory addr word
    addr := addr + 1
    idx := idx + 2
  some memory

def load_file (file_path : String) (mem : Memory) : IO Memory := do
  let file ← IO.FS.readBinFile file_path
  match load_into_mem file mem with
  | some m => pure m
  | none => throw $ IO.userError "Error: Input file could not be processed"

-- decode trap vector to trap code
def decode_trap (instr : UInt16) : Option Trap.Trapcodes :=
  let vector := instr.land 0xFF
  if vector == 0x20 then some .TRAP_GETC
  else if vector == 0x21 then some .TRAP_OUT
  else if vector == 0x22 then some .TRAP_PUTS
  else if vector == 0x23 then some .TRAP_IN
  else if vector == 0x24 then some .TRAP_PUTSP
  else if vector == 0x25 then some .TRAP_HALT
  else none

-- Memory-mapped I/O addresses
def KBSR : UInt16 := 0xFE00 -- keyboard status register
def KBDR : UInt16 := 0xFE02 -- keyboard data register

-- IO-aware memory read: intercepts MMIO addresses
def mem_read_io (mem : Memory) (addr : UInt16) (pending_key : IO.Ref (Option UInt16)) :
    IO (UInt16 × Memory) := do
  if addr == KBSR then
    -- Check if we have a pending key
    match ← pending_key.get with
    | some _ =>
      -- Key is available: set KBSR bit 15
      let mem := Memory.write mem KBSR 0x8000
      pure (0x8000, mem)
    | none =>
      -- Non-blocking check for key input
      let hasKey ← Terminal.checkKey
      if hasKey then
        let c ← Terminal.readChar
        let key := Trap.clear_high_bits (Trap.char_to_uint16 c)
        pending_key.set (some key)
        let mem := Memory.write mem KBSR 0x8000
        let mem := Memory.write mem KBDR key
        pure (0x8000, mem)
      else
        pure (0, mem)
  else if addr == KBDR then
    -- Read keyboard data and clear pending key
    match ← pending_key.get with
    | some key =>
      pending_key.set none
      pure (key, mem)
    | none =>
      pure (Memory.read mem addr, mem)
  else
    pure (Memory.read mem addr, mem)

-- IO-aware execution step: handles MMIO for memory reads
-- We need to intercept memory reads that the instruction will perform.
-- For LDI/LD/LDR that might access MMIO addresses, we pre-populate memory.
def execute_step_io (reg : Registers.Register) (mem : Memory)
    (pending_key : IO.Ref (Option UInt16)) : IO (Option (Registers.Register × Memory)) := do
  let instr := Memory.read mem reg.pc
  let reg := { reg with pc := reg.pc + 1 }
  match Instructions.instr_to_op instr with
  | none => pure none
  | some opcode =>
    -- For instructions that read memory, check if the address is MMIO
    match opcode with
    | .OP_LDI =>
      -- LDI: DR = mem[mem[PC + offset]]
      let offset := Execution.sign_extend (instr.land 0x1FF) 9
      let addr1 := reg.pc + offset
      let addr2 := Memory.read mem addr1 -- the indirect address
      -- Check if addr2 is MMIO
      let (value, mem) ← mem_read_io mem addr2 pending_key
      let dr := (instr >>> 9).land 0x7
      match Registers.write reg dr value with
      | none => pure none
      | some reg' =>
        let reg'' := Execution.set_condition_codes reg' value
        pure (some (reg'', mem))
    | .OP_LD =>
      let offset := Execution.sign_extend (instr.land 0x1FF) 9
      let addr := reg.pc + offset
      let (value, mem) ← mem_read_io mem addr pending_key
      let dr := (instr >>> 9).land 0x7
      match Registers.write reg dr value with
      | none => pure none
      | some reg' =>
        let reg'' := Execution.set_condition_codes reg' value
        pure (some (reg'', mem))
    | .OP_ST =>
      -- ST: mem[PC + offset] = SR — uses pure version but check for MMIO write to DSR
      pure (Execution.op_st instr reg mem)
    | .OP_STI =>
      -- STI might write to MMIO (display), handle with pure for now
      pure (Execution.op_sti instr reg mem)
    | _ =>
      -- All other instructions: use pure execute_step logic
      -- Re-construct what execute_step does (without re-fetching/incrementing PC)
      let result := match opcode with
        | .OP_BR   => Execution.op_br   instr reg mem
        | .OP_ADD  => Execution.op_add  instr reg mem
        | .OP_JSR  => Execution.op_jsr  instr reg mem
        | .OP_AND  => Execution.op_and  instr reg mem
        | .OP_LDR  => Execution.op_ldr  instr reg mem
        | .OP_STR  => Execution.op_str  instr reg mem
        | .OP_RTI  => Execution.op_rti  instr reg mem
        | .OP_NOT  => Execution.op_not  instr reg mem
        | .OP_JMP  => Execution.op_jmp  instr reg mem
        | .OP_RES  => Execution.op_res  instr reg mem
        | .OP_LEA  => Execution.op_lea  instr reg mem
        | .OP_TRAP => Execution.op_trap instr reg mem
        | _        => none -- LD, LDI, ST, STI handled above
      pure result

-- the main function
def main (file_paths : List String) : IO Unit := do
  -- initialize register and memory
  let mut reg := Registers.init
  let mut mem := Memory.init
  -- load the code into memory to execute
  let file_path := file_paths[0]!
  mem ← load_file file_path mem
  -- pending key buffer for MMIO
  let pending_key ← IO.mkRef (none : Option UInt16)
  -- enable raw terminal mode for character-at-a-time input
  Terminal.enableRawMode
  try
    -- execution loop
    while true do
      let instr := Memory.read mem reg.pc
      let opcode := Instructions.instr_to_op instr
      -- intercept TRAP instructions for IO
      if opcode == some .OP_TRAP then
        let reg_stepped := { reg with pc := reg.pc + 1 }
        match Registers.write reg_stepped 7 reg_stepped.pc with
        | none => break
        | some reg_with_r7 =>
          match decode_trap instr with
          | some .TRAP_HALT =>
            IO.println "HALT"
            break
          | some trapcode =>
            let (new_reg, new_mem) ← Trap.op_trap_io trapcode reg_with_r7 mem
            reg := new_reg
            mem := new_mem
          | none => break
      else
        -- IO-aware execution step (handles MMIO)
        match ← execute_step_io reg mem pending_key with
        | some (new_reg, new_mem) =>
          reg := new_reg
          mem := new_mem
        | none =>
          break
  finally
    Terminal.disableRawMode
