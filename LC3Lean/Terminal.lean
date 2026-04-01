namespace Terminal

@[extern "lc3_enable_raw_mode"]
opaque enableRawMode : IO Unit

@[extern "lc3_disable_raw_mode"]
opaque disableRawMode : IO Unit

@[extern "lc3_read_char"]
private opaque readCharCode : IO UInt32

@[extern "lc3_check_key"]
opaque checkKey : IO Bool

def readChar : IO Char := do
  let code ← readCharCode
  pure (Char.ofNat code.toNat)

end Terminal
