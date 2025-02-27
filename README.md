# LC3-Lean

A proof system in Lean for LC3 Programs, following the article [Write your Own Virtual Machine](https://www.jmeiners.com/lc3-vm/#what-is-a-virtual-machine-).

The purpose of this is to build formal verification tooling for a low-level Lean-based VM, in Lean.

## Installation 

Clone this repository and build the project.

```
lake build
```

This will give you a binary `.lake/build/bin/lc3-lean`

Run an assembled program, e.g. 2048:

```
.lake/build/bin/lc3-lean programs/2048.obj
```

If you add `path-to-dir/.lake/build/bin/` to your PATH you can simply run 

```
lc3-lean programs/2048.obj
```
