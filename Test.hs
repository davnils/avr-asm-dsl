{-# Language NoMonomorphismRestriction #-}
-- TODO: consider the RebindableSyntax extension

module Main where

import Assembler
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Tardis
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace
import Shared
import System.IO.Unsafe

-- TODO:
-- implement instructions for both cases: SBV and DSL
-- fix polymorphic return types and experiment with suitable state in both cases
-- think about SAT-solving compilers (in regard to optimizations)



-- OVERALL PLAN FOR OUTPUT and VERIFIER:
-- output can traverse the program, instruction by instruction, and only translate to NASM-format (SIMPLE!)
--
-- verifier needs to handle jumps specificially
-- jmpz = ite (getFlag Z)

program m = runIdentity $ execStateT (execStateT (runContT entryPoint (return . id))
                            (LabelState M.empty Nothing [] Nothing Nothing)) m

jmp = id

jmpif cond p1 p2 = if cond then p1 else p2

call lbl = do
  -- retrieve retry-continuation, wrapping the whole function
  callCC $ \retry -> lift . modify $ \labelState -> labelState { tmp = Just (retry ()) }
  Just retry <- lift . gets $ tmp
  labelState <- lift get
  -- retrieve escape continuation, jumping to the next instruction
  callCC $ \escape -> do
    case M.lookup lbl (labelMap labelState) of
      -- Nothing ==> label not registered
      Nothing -> do
        lift . modify $ \labelState -> labelState { labelTarget = Just (lbl, retry) }
        lift (lift (get)) >>= \s -> lift $ modify $ \state -> state { stateSnapshot = Just s }
      -- Just ==> label has been registered
      Just goto -> do
        -- push instruction to be executed upon return
        lift . modify $ \state -> state { callStack = escape () : callStack state }
        -- check if there is a state snapshot to be restored
        snapshot <- lift . gets $ stateSnapshot
        case snapshot of
          Nothing -> return ()
          Just s -> do
            -- upload and reset snapshot
            lift . lift $ put s
            lift . modify $ \state -> state { stateSnapshot = Nothing }
        goto

label lbl = do
  callCC $ \loc -> do
    -- add label to map
    lift . modify $ \labelState -> let map' = M.insert lbl (loc ()) (labelMap labelState) in
                                  labelState { labelMap = map' }
    -- check if there is any request active (unmatched call has been processed)
    labelState <- lift . gets $ labelTarget
    case labelState of
      Nothing -> return ()
      -- check if match on current label
      Just (lbl', source) -> when (lbl' == lbl) $ do
        -- then reset target and jump to corresponding call
        -- with the map updated
        lift . modify $ \labelState -> labelState { labelTarget = Nothing }
        source

ret = do
  stack <- lift . gets $ callStack
  when (not . null $ stack) $ do
    -- extract instruction to be executed upon return
    let (loc:rest) = stack
    lift . modify $ \state -> state { callStack = rest }
    loc

entryPoint = do
  label "sub0"
  add R0 R0
  ret

  add R1 R1
  call "sub1"

  add R2 R2

  label "sub1"
  add R3 R3
  ret
  call "sub0"

main :: IO ()
main = print res
  where res = program initialState
