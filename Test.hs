{-# Language NoMonomorphismRestriction #-}
-- TODO: consider the RebindableSyntax extension

module Main where

import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.State

data Machine = Machine Int
  deriving Show

data Mode = Assembler
          | Symbolic
  deriving (Eq, Show)

data ExecutionState = ExecutionState [AVR] Mode

type Program = Machine -> Machine
type Instruction = Program -> Program
type AVR = ContT () (StateT ExecutionState (StateT Machine Identity)) ()

-- needs labels!
-- a label is a marker
-- it can be used when executing either 'call' or 'ret'
-- can be created using callCC but must be referred to by name
-- problem: only forward visibility
-- cLABEL code = callCC $ code
-- cLABEL $ \subx -> do
-- what is the actual problem of having multiple high-level declarations, for each subroutine?


-- TODO:
-- Split up state between shared state (like callCC stack) and evaluation-specific state
-- make generic evaluator-functions
-- implement instructions for both cases: SBV and DSL
-- fix polymorphic return types and experiment with suitable state in both cases
-- think about SAT-solving compilers (in regard to optimizations)

lF f g = do
  ExecutionState _ mode <- lift get
  if mode == Assembler then f else g

lF1 f g a1 = do
  ExecutionState _ mode <- lift get
  if mode == Assembler then f a1 else g a1

lF2 f g a1 a2 = do
  ExecutionState _ mode <- lift get
  if mode == Assembler then f a1 a2 else g a1 a2

lF3 f g a1 a2 a3 = do
  ExecutionState _ mode <- lift get
  if mode == Assembler then f a1 a2 a3 else g a1 a2 a3

addA :: Int -> Int -> Int -> AVR
addA _ _ _ = return ()

addS :: Int -> Int -> Int -> AVR
addS _ _ _ = lift . lift . modify $ \(Machine m) -> (Machine (m + 1))

add = lF3 addA addS

program :: Mode -> Program
program mode m = runIdentity $ execStateT (execStateT (runContT entryPoint (return . id)) $ ExecutionState [] mode) m

jmp = id

jmpif cond p1 p2 = if cond then p1 else p2

call next = callCC $ \loc -> do
  lift (modify $ \(ExecutionState p m) -> ExecutionState (loc ():p) m)
  next

ret = do
  ExecutionState (next:t) m <- lift get
  lift . put $ ExecutionState t m
  next

entryPoint = do
  call sub1
  add 0 0 1
  call sub2

sub1 = do
  add 0 0 1
  ret

sub2 = do
  add 0 0 1
  ret

sub3 = do
  add 0 0 1
  jmp sub3

main :: IO ()
main = print res
  where (Machine res) = program Symbolic $ Machine 0

{- 
data SInstruction = SADD Int
                  | SSUB Int
-}
