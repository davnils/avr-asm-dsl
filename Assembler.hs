module Assembler where

import Control.Monad.State
import Shared

--------------------------------------------
--           Assembler backend            --
--------------------------------------------

type Constant = Int
type Port = Int
type Label = Int
type Bit = Int
type StatusFlag = Int
type RegisterLow = Register

type AVR = AVRBackend AssemblerState

data AssemblerState = AssemblerState [AVRInstruction]
  deriving (Eq, Show)

data AVRInstruction = ADD Register Register
                    | ADIW RegisterLow Constant
                    | SUB Register Register
                    | SUBI Register Constant
                    | SBC Register Register
                    | SBCI Register Constant
                    | SBIW RegisterLow Constant
                    | AND Register Register
                    | ANDI Register Constant
                    | OR Register Register
                    | ORI Register Constant
                    | EOR Register Register
                    | COM Register
                    | NEG Register
                    | SBR Register Constant
                    | CBR Register Constant
                    | INC Register
                    | DEC Register
                    | TST Register
                    | CLR Register
                    | SER Register
                    | RJMP Port Label
                    | IJMP
                    | EIJMP
                    | RCALL Label
                    | ICALL
                    | RET
                    | RETI
                    | CPSE Register Register
                    | CP Register Register
                    | CPC Register Register
                    | CPI Register Constant
                    | SBRC Register Bit
                    | SBRS Register Bit
                    | SBIC Port Bit
                    | SBIS Port Bit
                    | BRBC StatusFlag Label
                    | BRBS StatusFlag Label
                    | BREQ Label
                    | BRNE Label
                    | BRCS Label
                    | BRCC Label
                    | BRSH Label
                    | BRLO Label
                    | BRMI Label
                    | BRPL Label
                    | BRGE Label
                    | BRLT Label
                    | BRHS Label
                    | BRHC Label
  deriving (Eq, Show)

initialState = AssemblerState []

-- maintain state of previously generated stablenames.
-- perform a lookup, if value exists ==> append to the existing list of instructions (if tail is empty)
--                            else   ==> create an empty vector and append to the list, together with the stablename
-- this is weird - the function should not be evaluated.
-- all branches should rather be sorted out separetely, Overlapping control flows might still exist though.

rec :: AVRInstruction -> AVR
rec instruction = lift . lift . lift . modify $ \(AssemblerState l) -> AssemblerState (instruction:l)

add :: Register -> Register -> AVR
add r1 r2 = do
  JumpState callee _ <- lift get
  case callee of
    Just _ -> return ()
    Nothing -> rec $ ADD r1 r2
