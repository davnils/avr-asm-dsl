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
  callCC $ \retry -> lift . modify $ \labelState -> labelState { tmp = Just (retry ()) }
  Just retry <- lift . gets $ tmp
  labelState <- lift get
  callCC $ \escape -> do
    case M.lookup lbl (labelMap labelState) of
      Nothing -> do
        lift . modify $ \labelState -> labelState { labelTarget = Just (lbl, retry) }
        lift (lift (get)) >>= \s -> lift $ modify $ \state -> state { stateSnapshot = Just s }
      Just goto -> do
        lift . modify $ \state -> state { callStack = escape () : callStack state }
        snapshot <- lift . gets $ stateSnapshot
        case snapshot of
          Nothing -> return ()
          Just s -> lift . lift $ put s
        goto

label lbl = do
  callCC $ \loc -> do
    lift . modify $ \labelState -> let map' = M.insert lbl (loc ()) (labelMap labelState) in
                                  labelState { labelMap = map' }
    labelState <- lift . gets $ labelTarget
    case labelState of
      Nothing -> return ()
      Just (lbl', source) -> when (lbl' == lbl) $ do
        lift . modify $ \labelState -> labelState { labelTarget = Nothing }
        source

ret = do
  stack <- lift . gets $ callStack
  when (not . null $ stack) $ do
    let (loc:rest) = stack
    lift . modify $ \state -> state { callStack = rest }
    loc

entryPoint = do
  add R1 R1
  call "subr"

  add R2 R2

  label "subr"
  add R3 R3
  ret

main :: IO ()
main = print res
  where res = program initialState
