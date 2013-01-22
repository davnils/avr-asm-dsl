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

prog1 = do
  label "loop"
  subi R1 32
  subi R2 64
  brne "loop"
  subi R2 64

prog2 = do
  subi R1 128

preserveRegisters = do
  let regs = registers initialState
  r1 <- sWord8 "r1"
  let arr' = regs // [(R1, r1)]
  return $ initialState { registers = arr' }

verify :: Symbolic SBool
verify = do
  state <- preserveRegisters
  let eval prog = registers $ program prog state
  return $ (eval prog1 ! R1) .== (eval prog2 ! R1)

main :: IO ()
main = prove verify >>= print
