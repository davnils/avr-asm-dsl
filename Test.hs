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

entryPoint = do
  label "loop"
  add R1 R1
  brne "loop"

  add R2 R2

main :: IO ()
main = print res
  where res = program entryPoint initialState
