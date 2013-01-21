{-# Language NoMonomorphismRestriction #-}
-- TODO: consider the RebindableSyntax extension

module Main where

-- import Assembler
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as M
import Shared
import Verification

prog1 = do
  subi R2 4
  label "loop"
  subi R2 3
  subi R1 1
  brne "loop"

prog2 = do
  subi R2 4

main :: IO ()
main = print res1 >> print res2
  where
    res1 = program prog1 initialState
    res2 = program prog2 initialState
