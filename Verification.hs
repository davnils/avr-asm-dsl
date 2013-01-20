{-# Language TypeSynonymInstances, FlexibleInstances #-}

module Verification where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import qualified Data.Map as M
import Data.SBV
import Shared

--------------------------------------------
--         Verification backend           --
--------------------------------------------

type AVR = AVRBackend MachineState

data MachineState = MachineState SWord8
  deriving (Eq, Show)

instance Mergeable AVR where
  symbolicMerge b m1 m2 = do
    s <- lift get
    s' <- lift . lift $ get

    let MachineState m1' = programInternal m1 s' s
    let MachineState m2' = programInternal m2 s' s
    let m' = MachineState $ symbolicMerge b m1' m2'

    lift . lift $ put m'

initialState = MachineState 0

jmpLabel = processLabel False

jmpLabelCond cond target = callCC $ \noJump ->
  ite cond (processLabel False target) (noJump ())

callLabel = processLabel True 

processLabel saveRet lbl = do
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
        when saveRet $
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

rcall :: String -> AVR
rcall = callLabel

breq :: String -> AVR
breq target = do
  MachineState s <- lift . lift $ get
  jmpLabelCond (s .== 1) target

brne :: String -> AVR
brne target = do
  MachineState s <- lift . lift $ get
  jmpLabelCond (s ./= 5) target

addCount :: SWord8 -> AVR
addCount i = lift . lift . modify $ \(MachineState c) -> MachineState (c + i)

add :: Register -> Register -> AVR
add _ _ = addCount 1
