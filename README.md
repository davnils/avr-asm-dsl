AVR EDSL With Verification
==========================


This is a very much work-in-progress project that deals with modelling the AVR (8 bit) instruction set in Haskell.
A deep embedding is achieved (as demonstrated below) and verification can be performed using the Z3 SMT solver.

The goal is to eventually reach a stable interface providing more generic functionality, e.g. capable of:

* Given reasonable register values, can the subroutine write above the stack?
* Are the side-effects constructed by two subroutines equivalent?

Termination is achieved by using a bounded call stack. At the moment only a subset of the instruction set is implemented.

Example
-------

This very simple example demonstrates equivalence of two methods.


```haskell
prog1 = do
  label "loop"
  rcall "addReg"
  subi R2 1
  brne "loop"
  rjmp "done"

  label "addReg"
  add R1 R1
  ret

  label "done"

prog2 = do
  label "loop"
  lsl R1
  subi R2 1
  brne "loop"

  label "done"

freeRegisters = do
  state ← initialState
  let ar0 = memory state
  r1 ← sWord8 "r1"
  r2 ← sWord8 "r2"
  let ar1 = writeArray ar0 (lookupAddress R1) r1
  let ar2 = writeArray ar1 (lookupAddress R2) r2
  return $ state { memory = ar2 }

proveEquivalence :: AVR → AVR → IO ThmResult
proveEquivalence proga progb = prove statement
  where
  statement = do
    state ← freeRegisters
    let eval prog = readArray (memory $ program prog state) (lookupAddress R1)
    return $ eval proga .≡ eval progb

main :: IO ∅
main = proveEquivalence prog1 prog2 ⤜ print
```

.. which results in:

```haskell
λ :l Test.hs
[3 of 3] Compiling Main             ( Test.hs, interpreted )
Ok, modules loaded: Main, Shared, Verification.
λ proveEquivalence prog1 prog2
Q.E.D.
```
