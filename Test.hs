{-# Language NoMonomorphismRestriction #-}
-- TODO: consider the RebindableSyntax extension

module Main where

import Assembler
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Tardis
import Shared

-- TODO:
-- Split up state between shared state (like callCC stack) and evaluation-specific state *DONE*
-- make generic evaluator-functions
-- implement instructions for both cases: SBV and DSL
-- fix polymorphic return types and experiment with suitable state in both cases
-- think about SAT-solving compilers (in regard to optimizations)

{- addS :: Int -> Int -> AVR
addS _ _ = lift . lift . modify $ \(Machine m) -> (Machine (m + 1)) -}

-- program :: Program
program m = runIdentity $ execStateT (execStateT (execStateT (runContT entryPoint (return . id)) (JumpState Nothing Nothing)) $ ExecutionState []) m

jmp = id

jmpif cond p1 p2 = if cond then p1 else p2

-- should execute until the label has been found
{- call next = callCC $ \loc -> do
  lift . lift . modify $ \(ExecutionState p) -> ExecutionState (loc ():p)
  next -}

-- should check if the current label exists in the dictionary, else request it from the future
-- begin by assuming that the requested label has not yet been seen
-- also ignore any duplication of state to begin with
call lbl = do
  callCC $ \loc -> lift $ modify $ \(JumpState _ _) -> (JumpState (Just $ loc ()) Nothing) 
  JumpState _ target <- lift get
  case target of
    Just lbl -> lbl
    Nothing -> return ()

-- should check if not-exected continuation exists, in
-- which case it should register a continuation and call the supplied one
-- perhaps this could be written such that no execution is wasted (ignored)
{- label lbl = undefined callCC $ \loc -> lift $ do
  target <- getPast
  when (target == lbl) $ sendPast (FutureLabel $ loc ()) -}
-- label _ = lift $ sendPast $ FutureLabel (add R3 R3)


-- given _ Nothing -> fill in loc, target continuation
-- must cleanup sometime to ensure non-infinite recursion
label _ = do
  callCC $ \loc -> do
    JumpState callee target <- lift get
    case (callee, target) of
      -- Just bla -> lift . put $ JumpState Nothing Nothing
      (Just callee', Nothing) -> lift (put (JumpState Nothing (Just $ loc ()))) >> callee'

{- ret = do
  ExecutionState (next:t) <- lift get
  lift . lift . put $ ExecutionState t
  next -}

entryPoint = do
  add R1 R1
  call "sub2"

  add R2 R2

  label "sub2"
  add R5 R5
  {- FutureLabel l <- -}
  -- l

  -- label "sub1"
  -- add R0 R2


main :: IO ()
main = print res
  where res = program initialState

{- 
data SInstruction = SADD Int
                  | SSUB Int
-}
