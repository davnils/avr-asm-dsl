module Shared where

import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.State
import Data.Array (Ix(..))
import Data.SBV
import qualified Data.Map as M

data Register = R0
           		| R1
           		| R2
           		| R3
           		| R4
           		| R5
           		| R6
           		| R7
           		| R8
           		| R9
           		| R10
           		| R11
           		| R12
           		| R13
           		| R14
           		| R15
           		| R16
           		| R17
           		| R18
           		| R19
           		| R20
           		| R21
           		| R22
           		| R23
           		| R24
           		| R25
           		| R26
           		| R27
           		| R28
           		| R29
           		| R30
           		| R31
           		| X
           		| INCX
           		| SUBX
           		| XINC
           		| XSUB
           		| Y
           		| INCY
           		| SUBY
           		| YINC
           		| YSUB
           		| Z
           		| INCZ
           		| SUBZ
           		| ZINC
           		| ZSUB
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

data StatusFlag = SC | SZ | SN | SV | SS | SH | ST | SI
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

data Mode = Assembler
          | Symbolic
  deriving (Eq, Show)

data ExecutionState a = ExecutionState a
data LabelState a = LabelState {
  labelMap :: M.Map String (AVRBackend a),
  labelTarget :: Maybe (String, AVRBackend a),
  callStack :: [AVRBackend a],
  stateSnapshot :: Maybe a,
  tmp :: Maybe (AVRBackend a),
  callCount :: SInt16
}

type AVRBackend a = ContT () (AVRBackendBase a) ()
type AVRBackendBase a = StateT (LabelState a) (StateT a Identity)

programInternal start machineState labelState = runIdentity $ execStateT (
                                                                execStateT (runContT start (return . id)) labelState
                                                              ) machineState

program start machineState = programInternal start machineState $ LabelState M.empty Nothing [] Nothing Nothing 0
