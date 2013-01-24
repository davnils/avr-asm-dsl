{-# Language NoMonomorphismRestriction #-}
-- TODO: consider the RebindableSyntax extension

module Main where

-- import Assembler
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.State
import Data.Array
import qualified Data.Map as M
import Data.SBV
import Shared
import Verification

-- Two versions of a program that doubles an integer in 'r1' a total of 'r2' times.
-- Proofs are currently done over the register file, hence stack etc is ignored.
-- Using GHC interpreter:
--
--  \ :l Test.hs
--  [3 of 3] Compiling Main             ( Test.hs, interpreted )
--  Ok, modules loaded: Main, Shared, Verification.
--  \ proveEquivalence prog1 prog2
--  Q.E.D.
--

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
  let regs = registers initialState
  r1 <- sWord8 "r1"
  r2 <- sWord8 "r2"
  let arr' = regs // [(R1, r1), (R2, r2)]
  return $ initialState { registers = arr' }

proveEquivalence :: AVR -> AVR -> IO ThmResult
proveEquivalence proga progb = prove statement
  where
  statement = do
    state <- freeRegisters
    let eval prog = registers $ program prog state
    return $ (eval proga ! R1) .== (eval progb ! R1)

main :: IO ()
main = proveEquivalence prog1 prog2 >>= print
