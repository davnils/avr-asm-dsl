-- läs http://www.reddit.com/r/programming/comments/15q6lw/the_continuation_monad/
{-# Language TypeSynonymInstances, FlexibleInstances #-}

module Verification where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Data.Array (Array, Ix(..), (!), (//), array)
import qualified Data.Map as M
import Data.SBV
import Prelude hiding (and)
import Shared

--------------------------------------------
--         Verification backend           --
--------------------------------------------

maxCallCount = 10

type AVR = AVRBackend MachineState

data MachineState = MachineState {
    memory :: SFunArray Word16 Word8,
    registers :: Array Register SWord8,
    sreg :: Array StatusFlag SBool,
    stackL :: SWord8,
    stackH :: SWord8
} deriving Show

instance Mergeable AVR where
  symbolicMerge b m1 m2 = do
    s <- lift get
    s' <- lift . lift $ get

    let MachineState a1 a2 a3 a4 a5 = programInternal m1 s' s
    let MachineState b1 b2 b3 b4 b5 = programInternal m2 s' s
    let m' = MachineState (symbolicMerge b a1 b1) (symbolicMerge b a2 b2) (symbolicMerge b a3 b3) (symbolicMerge b a4 b4) (symbolicMerge b a5 b5)

    lift . lift $ put m'

initialState = MachineState mem reg status 0 0
  where
  mem = resetArray (mkSFunArray $ const 0) 0
  reg = array (minBound, maxBound) $ zip [minBound..maxBound] $ cycle [0]
  status = array (minBound, maxBound) $ zip [minBound..maxBound] $ cycle [true]

jmpLabel = processLabel False

jmpLabelCond cond target = callCC $ \noJump -> do
  count <- lift $ gets callCount
  lift . modify $ \state -> state { callCount = count + 1 }
  ite (count .< maxCallCount &&& cond) (processLabel False target) (noJump ())

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
  unless (null stack) $ do
    -- extract instruction to be executed upon return
    let (loc:rest) = stack
    lift . modify $ \state -> state { callStack = rest }
    loc

writeRegister reg val = lift . modify $
  \s -> s { registers = registers s // [(reg, val)] }

readRegister reg = do
  regs <- lift $ gets registers
  return $ regs ! reg

readStatusReg bit = do
  sreg' <- lift $ gets sreg
  return $ sreg' ! bit

readStatusRegWord bit = do
  sreg' <- lift $ gets sreg
  return $ oneIf $ sreg' ! bit

writeStatusReg bit val = lift . modify $
  \s -> s { sreg = sreg s // [(bit, val)] }

-- Artihmetic instructions

addInternal :: Register -> Register -> SWord8 -> AVR
addInternal regd regr k = lift $ do
  vd <- readRegister regd
  vr <- readRegister regr
  let r = vd + vr + k
  writeRegister regd r

  writeStatusReg SN $ msb r

  writeStatusReg SH $
    (sbvTestBit vd 3 &&& sbvTestBit vr 3) |||
    (sbvTestBit vr 3 &&& (bnot $ sbvTestBit r 3)) |||
    ((bnot $ sbvTestBit r 3) &&& sbvTestBit vd 3)

  writeStatusReg SZ $ r .== 0

  writeStatusReg SV $
    (sbvTestBit vd 7 &&& sbvTestBit vr 7 &&& (bnot $ sbvTestBit r 7)) |||
    ((bnot $ sbvTestBit vd 7) &&& (bnot $ sbvTestBit vr 7) &&& sbvTestBit r 7)

  writeStatusReg SC $
    (sbvTestBit vd 7 &&& sbvTestBit vr 7) |||
    (sbvTestBit vr 7 &&& (bnot $ sbvTestBit r 7)) |||
    ((bnot $ sbvTestBit r 7) &&& sbvTestBit vd 7)

  ss <- liftM2 (+) (readStatusRegWord SN) (readStatusRegWord SV)
  writeStatusReg SS $ ss .== (1 :: SWord8)

subInternal :: Register -> Maybe Register -> SWord8 -> AVR
subInternal regd regr k = lift $ do
  vd <- readRegister regd

  vr <- case regr of
    Nothing -> return 0
    Just regr' -> readRegister regr'

  let r = vd - vr - k
  writeRegister regd r

  writeStatusReg SN $ msb r

  writeStatusReg SH $
    ((bnot $ sbvTestBit vd 3) &&& sbvTestBit vr 3) |||
    (sbvTestBit vr 3 &&& sbvTestBit r 3) |||
    ((bnot $ sbvTestBit r 3) &&& sbvTestBit vd 3)

  writeStatusReg SZ $ r .== 0

  writeStatusReg SV $
    (sbvTestBit vd 7 &&& (bnot $ sbvTestBit vr 7) &&& (bnot $ sbvTestBit r 7)) |||
    ((bnot $ sbvTestBit vd 7) &&& sbvTestBit vr 7 &&& sbvTestBit r 7)

  writeStatusReg SC $
    ((bnot $ sbvTestBit vd 7) &&& sbvTestBit vr 7) |||
    (sbvTestBit vr 7 &&& sbvTestBit r 7) |||
    (sbvTestBit r 7 &&& (bnot $ sbvTestBit vd 7))

  ss <- liftM2 (+) (readStatusRegWord SN) (readStatusRegWord SV)
  writeStatusReg SS $ ss .== (1 :: SWord8)

add :: Register -> Register -> AVR
add r1 r2 = addInternal r1 r2 0

adc :: Register -> Register -> AVR
adc r1 r2 = do
  carry <- lift $ readStatusRegWord SC
  addInternal r1 r2 carry

adiw :: Register -> SWord8 -> AVR
adiw = error "Currently unsupported"

sub :: Register -> Register -> AVR
sub r1 r2 = subInternal r1 (Just r2) 0

subi :: Register -> SWord8 -> AVR
subi reg = subInternal reg Nothing

subc :: Register -> Register -> AVR
subc r1 r2 = do
  carry <- lift $ readStatusRegWord SC
  subInternal r1 (Just r2) carry

subci :: Register -> SWord8 -> AVR
subci reg k1 = do
  carry <- lift $ readStatusRegWord SC
  subInternal reg Nothing carry

subiw :: Register -> SWord8 -> AVR
subiw = error "Currently unsupported"

updateLogicFlags :: SWord8 -> AVR
updateLogicFlags r = lift $ do
  writeStatusReg SN $ msb r
  writeStatusReg SZ $ r .== 0
  writeStatusReg SV false
  ss <- liftM2 (+) (readStatusRegWord SN) (readStatusRegWord SV)
  writeStatusReg SS $ ss .== (1 :: SWord8)

and :: Register -> Register -> AVR
and r1 r2 = do
  v1 <- lift $ readRegister r1
  v2 <- lift $ readRegister r2
  let result = v1 .&. v2
  lift $ writeRegister r1 $ result
  updateLogicFlags result

andi :: Register -> SWord8 -> AVR
andi r1 k = do
  v1 <- lift $ readRegister r1
  let result = v1 .&. k
  lift $ writeRegister r1 $ result
  updateLogicFlags result

or :: Register -> Register -> AVR
or r1 r2 = do
  v1 <- lift $ readRegister r1
  v2 <- lift $ readRegister r2
  let result = v1 .|. v2
  lift $ writeRegister r1 $ result
  updateLogicFlags result

ori :: Register -> SWord8 -> AVR
ori r1 k = do
  v1 <- lift $ readRegister r1
  let result = v1 .|. k
  lift $ writeRegister r1 $ result
  updateLogicFlags result

eor :: Register -> Register -> AVR
eor r1 r2 = do
  v1 <- lift $ readRegister r1
  v2 <- lift $ readRegister r2
  let result = xor v1 v2
  lift $ writeRegister r1 $ result
  updateLogicFlags result

com =  error "Currently unsupported"

neg =  error "Currently unsupported"

sbr :: Register -> SWord8 -> AVR
sbr r1 k = ori r1 k

cbr :: Register -> SWord8 -> AVR
cbr r1 k = andi r1 (0xFF - k)

inc :: Register -> AVR
inc = error "Currently unsupported"

dec :: Register -> AVR
dec = error "Currently unsupported"

tst :: Register -> AVR
tst r1 = and r1 r1

clr :: Register -> AVR
clr r1 = eor r1 r1

set :: Register -> AVR
set r1 = lift $ writeRegister r1 0xFF

-- Branch instructions

breq :: String -> AVR
breq target = do
  s <- lift $ readStatusReg SZ
  jmpLabelCond s target

brne :: String -> AVR
brne target = do
  s <- lift $ readStatusReg SZ
  jmpLabelCond (bnot s) target

rcall :: String -> AVR
rcall = callLabel


