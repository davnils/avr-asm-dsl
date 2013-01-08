module Shared where

import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Tardis

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
  deriving (Eq, Show)

-- symbolic backend
data Machine = Machine Int
  deriving Show
type BackendState = Machine
-- 

data Mode = Assembler
          | Symbolic
  deriving (Eq, Show)

data ExecutionState a = ExecutionState [AVRBackend a]
data FutureLabel a = FutureLabel (AVRBackend a)
data JumpState a = JumpState (Maybe (AVRBackend a)) (Maybe (AVRBackend a))

-- type Program = Machine -> Machine
-- type Instruction = Program -> Program
type AVRBackend a = ContT () (StateT (JumpState a) (StateT (ExecutionState a) (StateT a Identity))) ()

-- an alternative to modules might be to add an upper layer in the transformer stack
-- which only defines a monad instance which handles setup and lifts the ContT bind