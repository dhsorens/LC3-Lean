import Lake
open Lake DSL

package «LC3-Lean» where
  version := v!"0.1.0"

lean_lib «LC3Lean» where

lean_lib «LC3Correct» where
  roots := #[`LC3Correct.LC3Correct, `LC3Correct.ExecutionCorrect,
             `LC3Correct.RegisterLemmas, `LC3Correct.MemoryLemmas]

extern_lib «terminal» pkg := do
  let name := nameToStaticLib "terminal"
  let srcFile := pkg.dir / "c" / "terminal.c"
  let oFile := pkg.buildDir / "c" / "terminal.o"
  let srcJob ← inputTextFile srcFile
  let incDir ← getLeanIncludeDir
  let oJob ← buildO oFile srcJob #["-I", incDir.toString] #["-fPIC"]
  buildStaticLib (pkg.staticLibDir / name) #[oJob]

@[default_target]
lean_exe «lc3-lean» where
  root := `Main
