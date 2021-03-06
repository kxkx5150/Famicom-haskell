module Emulator.CPU
  ( reset,
    runCpu,
    runCpuT,
  )
where

import Control.Monad
import Data.Bits hiding (bit)
import Data.Word
import Emulator.Mem
import Emulator.Nes
import Emulator.Opcode
import Emulator.Util.Trace
  ( Trace,
    mkTrace,
    renderTrace,
  )
import Emulator.Util.Util
import Prelude hiding
  ( and,
    compare,
  )

data Flag
  = Negative
  | Overflow
  | Unused
  | Break
  | Decimal
  | InterruptDisable
  | Zero
  | Carry
  deriving (Enum)

runCpu :: Emulator Int
runCpu = do
  startingCycles <- loadReg cpuCycles
  handleIrq
  opcode <- loadOp
  (pageCrossed, addr) <- getAddr (mode opcode)
  incPc opcode
  modReg cpuCycles (+ getCycles opcode pageCrossed)
  execOp opcode addr
  endingCycles <- loadReg cpuCycles
  pure $ endingCycles - startingCycles

runCpuT :: Emulator (Int, Trace)
runCpuT = do
  startingCycles <- loadReg cpuCycles
  handleIrq
  opcode <- loadOp
  trace <- mkTrace opcode
  (pageCrossed, addr) <- getAddr (mode opcode)
  incPc opcode
  modReg cpuCycles (+ getCycles opcode pageCrossed)
  execOp opcode addr
  endingCycles <- loadReg cpuCycles
  pure (endingCycles - startingCycles, trace)

reset :: Emulator ()
reset = do
  v <- readMemW 0xFFFC
  storeReg pc v
  storeReg sp 0xFD
  storeReg p 0x24

loadOp :: Emulator Opcode
loadOp = do
  pcv <- loadReg pc
  av <- readMemPort pcv
  pure $ decodeOpcode av

incPc :: Opcode -> Emulator ()
incPc opcode = modReg pc (+ instrLength)
  where
    instrLength = fromIntegral $ len opcode

getCycles :: Opcode -> Bool -> Int
getCycles opcode pageCrossed =
  if pageCrossed then pageCrossCycles opcode + cycles opcode else cycles opcode

getAddr :: AddressMode -> Emulator (Bool, Word16)
getAddr mode = case mode of
  IMM -> do
    pcv <- loadReg pc
    pure (False, pcv + 1)
  IMP -> pure (False, 0)
  ABS -> do
    pcv <- loadReg pc
    addrV <- readMemW (pcv + 1)
    pure (False, addrV)
  ABX -> do
    pcv <- loadReg pc
    xv <- loadReg x
    v <- readMemW (pcv + 1)
    let addrV = v + toWord16 xv
    let pageCrossed = ((addrV - toWord16 xv) .&. 0xFF00) /= (addrV .&. 0xFF00)
    pure (pageCrossed, addrV)
  ABY -> do
    pcv <- loadReg pc
    yv <- loadReg y
    v <- readMemW (pcv + 1)
    let addrV = v + toWord16 yv
    let pageCrossed = ((addrV - toWord16 yv) .&. 0xFF00) /= (addrV .&. 0xFF00)
    pure (pageCrossed, addrV)
  ACC -> pure (False, 0)
  IND -> do
    pcv <- loadReg pc
    addr <- readMemW (pcv + 1)
    lo <- readMemPort addr
    hi <- readMemPort $ (addr .&. 0xFF00) .|. toWord16 (toWord8 addr + 1)
    pure (False, makeW16 lo hi)
  IZY -> do
    pcv <- loadReg pc
    yv <- loadReg y
    v <- readMemPort $ pcv + 1
    lo <- readMemPort (toWord16 v)
    hi <-
      readMemPort $
        ((toWord16 v) .&. 0xFF00)
          .|. toWord16
            (toWord8 (toWord16 v) + 1)
    let addrV = (makeW16 lo hi) + toWord16 yv
    let pageCrossed = ((addrV - toWord16 yv) .&. 0xFF00) /= (addrV .&. 0xFF00)
    pure (pageCrossed, addrV)
  IZX -> do
    pcv <- loadReg pc
    xv <- loadReg x
    v <- readMemPort $ pcv + 1
    lo <- readMemPort (toWord16 (v + xv))
    hi <-
      readMemPort $
        ((toWord16 (v + xv)) .&. 0xFF00)
          .|. toWord16
            (toWord8 (toWord16 (v + xv)) + 1)
    pure (False, (makeW16 lo hi))
  REL -> do
    pcv <- loadReg pc
    offset16 <- readMemW (pcv + 1)
    let offset8 = firstNibble offset16
    let diff = if offset8 < 0x80 then 0 else 0x100
    pure (False, pcv + 2 + offset8 - diff)
  ZP -> do
    pcv <- loadReg pc
    v <- readMemPort (pcv + 1)
    pure (False, toWord16 v)
  ZPX -> do
    pcv <- loadReg pc
    xv <- loadReg x
    v <- readMemPort (pcv + 1)
    pure (False, toWord16 $ v + xv)
  ZPY -> do
    pcv <- loadReg pc
    yv <- loadReg y
    v <- readMemPort (pcv + 1)
    pure (False, toWord16 $ v + yv)

execOp :: Opcode -> Word16 -> Emulator ()
execOp (Opcode _ mnemonic mode _ _ _) addr = case mnemonic of
  ADC -> do
    av <- loadReg a
    bv <- readMemPort addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    storeReg a (av + bv + cv)
    av' <- loadReg a
    setZN av'
    let shouldCarry = toInt av + toInt bv + toInt cv > 0xFF
    let doesOverflow =
          ((av `xor` bv) .&. 0x80) == 0 && ((av `xor` av') .&. 0x80) /= 0
    setFlag Carry shouldCarry
    setFlag Overflow doesOverflow
  AND -> do
    av <- loadReg a
    v <- readMemPort addr
    storeReg a (av .&. v)
    av' <- loadReg a
    setZN av'
  ASL -> do
    v <- case mode of
      ACC -> loadReg a
      _ -> readMemPort addr
    let i = (v `shiftR` 7) .&. 1
    setFlag Carry (toEnum . fromIntegral $ i)
    let shiftedV = v `shiftL` 1
    case mode of
      ACC -> storeReg a shiftedV
      _ -> writeMemPort addr shiftedV
    setZN shiftedV
  BCC -> do
    branch (not <$> getFlag Carry) addr
  BCS -> do
    branch (getFlag Carry) addr
  BEQ -> do
    branch (getFlag Zero) addr
  BIT -> do
    v <- readMemPort addr
    -- setV $ (v `shiftR` 6) .&. 1
    setV v
    av <- loadReg a
    setZ (v .&. av)
    setN v
  BMI -> do
    branch (getFlag Negative) addr
  BNE -> do
    branch (not <$> getFlag Zero) addr
  BPL -> do
    branch (not <$> getFlag Negative) addr
  BRK -> do
    pcv <- loadReg pc
    pushW $ pcv + 1
    p <- loadReg p
    push $ p .|. 0x10
    setFlag InterruptDisable True
    av <- readMemW 0xFFFE
    storeReg pc av
  BVC -> do
    branch (not <$> getFlag Overflow) addr
  BVS -> do
    branch (getFlag Overflow) addr
  CLC -> do
    setFlag Carry False
  CLD -> do
    setFlag Decimal False
  CLI -> do
    setFlag InterruptDisable False
  CLV -> do
    setFlag Overflow False
  CMP -> do
    v <- readMemPort addr
    av <- loadReg a
    compare av v
  CPX -> do
    v <- readMemPort addr
    xv <- loadReg x
    compare xv v
  CPY -> do
    v <- readMemPort addr
    yv <- loadReg y
    compare yv v
  DEC -> do
    v <- readMemPort addr
    let value = v - 1
    writeMemPort addr value
    setZN value
  DEX -> do
    v <- loadReg x
    let value = v - 1
    storeReg x value
    setZN value
  DEY -> do
    v <- loadReg y
    let value = v - 1
    storeReg y value
    setZN value
  EOR -> do
    v <- readMemPort addr
    av <- loadReg a
    let newAv = av `xor` v
    storeReg a newAv
    setZN newAv
  INC -> do
    v <- readMemPort addr
    let value = v + 1
    writeMemPort addr value
    setZN value
  INX -> do
    v <- loadReg x
    let value = v + 1
    storeReg x value
    setZN value
  INY -> do
    v <- loadReg y
    let value = v + 1
    storeReg y value
    setZN value
  JMP -> do
    storeReg pc addr
  JSR -> do
    pcv <- loadReg pc
    pushW $ pcv - 1
    storeReg pc addr
  LDA -> do
    v <- readMemPort addr
    storeReg a v
    setZN v
  LDX -> do
    v <- readMemPort addr
    storeReg x v
    setZN v
  LDY -> do
    v <- readMemPort addr
    storeReg y v
    setZN v
  LSR -> do
    v <- case mode of
      ACC -> loadReg a
      _ -> readMemPort addr

    setFlag Carry (toEnum . fromIntegral $ v .&. 1)
    let shiftedV = v `shiftR` 1
    case mode of
      ACC -> storeReg a shiftedV
      _ -> writeMemPort addr shiftedV
    setZN shiftedV
  NOP -> do
    pure ()
  PHA -> do
    av <- loadReg a
    push av
  PHP -> do
    p <- loadReg p
    push $ p .|. 0x10
  PLA -> do
    v <- pull
    storeReg a v
    setZN v
  PLP -> do
    v <- pull
    storeReg p ((v .&. 0xEF) .|. 0x20)
  ORA -> do
    v <- readMemPort addr
    av <- loadReg a
    let newAv = av .|. v
    storeReg a newAv
    setZN newAv
  RTI -> do
    addr <- pull
    storeReg p (addr .&. 0xEf .|. 0x20)
    addr' <- pullW
    storeReg pc addr'
  RTS -> do
    paddr <- pullW
    storeReg pc (paddr + 1)
  ROR -> do
    v <- case mode of
      ACC -> loadReg a
      _ -> readMemPort addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    setFlag Carry (toEnum $ fromIntegral $ v .&. 1)
    let shiftedV = (v `shiftR` 1) .|. (cv `shiftL` 7)
    case mode of
      ACC -> storeReg a shiftedV
      _ -> writeMemPort addr shiftedV
    setZN shiftedV
  ROL -> do
    v <- case mode of
      ACC -> loadReg a
      _ -> readMemPort addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    setFlag Carry (toEnum $ fromIntegral $ (v `shiftR` 7) .&. 1)
    let shiftedV = (v `shiftL` 1) .|. cv
    case mode of
      ACC -> storeReg a shiftedV
      _ -> writeMemPort addr shiftedV
    setZN shiftedV
  SBC -> do
    av <- loadReg a
    bv <- readMemPort addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    storeReg a (av - bv - (1 - cv))
    av' <- loadReg a
    setZN av'
    let shouldCarry = toInt av - toInt bv - toInt (1 - cv) >= 0
    let doesOverflow =
          ((av `xor` bv) .&. 0x80) /= 0 && ((av `xor` av') .&. 0x80) /= 0
    setFlag Carry shouldCarry
    setFlag Overflow doesOverflow
  SEC -> do
    setFlag Carry True
  SED -> do
    setFlag Decimal True
  SEI -> do
    setFlag InterruptDisable True
  STA -> do
    av <- loadReg a
    writeMemPort addr av
  STX -> do
    loadReg x >>= writeMemPort addr
  STY -> do
    loadReg y >>= writeMemPort addr
  TAX -> do
    av <- loadReg a
    storeReg x av
    xv <- loadReg x
    setZN xv
  TAY -> do
    av <- loadReg a
    storeReg y av
    yv <- loadReg y
    setZN yv
  TSX -> do
    spv <- loadReg sp
    storeReg x spv
    xv <- loadReg x
    setZN xv
  TXA -> do
    xv <- loadReg x
    storeReg a xv
    av <- loadReg a
    setZN av
  TXS -> do
    xv <- loadReg x
    storeReg sp xv
  TYA -> do
    yv <- loadReg y
    storeReg a yv
    av <- loadReg a
    setZN av
  KIL -> do
    pure ()
  LAX -> do
    v <- readMemPort addr
    storeReg a v
    storeReg x v
    setZN v
  SAX -> do
    av <- loadReg a
    xv <- loadReg x
    writeMemPort addr (av .&. xv)
  DCP -> do
    v <- readMemPort addr
    let value = v - 1
    writeMemPort addr value
    setZN value

    v <- readMemPort addr
    av <- loadReg a
    compare av v
  ISC -> do
    v <- readMemPort addr
    let value = v + 1
    writeMemPort addr value
    setZN value

    av <- loadReg a
    bv <- readMemPort addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    storeReg a (av - bv - (1 - cv))
    av' <- loadReg a
    setZN av'
    let shouldCarry = toInt av - toInt bv - toInt (1 - cv) >= 0
    let doesOverflow =
          ((av `xor` bv) .&. 0x80) /= 0 && ((av `xor` av') .&. 0x80) /= 0
    setFlag Carry shouldCarry
    setFlag Overflow doesOverflow
  RLA -> do
    v <- case mode of
      ACC -> loadReg a
      _ -> readMemPort addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    setFlag Carry (toEnum $ fromIntegral $ (v `shiftR` 7) .&. 1)
    let shiftedV = (v `shiftL` 1) .|. cv
    case mode of
      ACC -> storeReg a shiftedV
      _ -> writeMemPort addr shiftedV
    setZN shiftedV

    av <- loadReg a
    v <- readMemPort addr
    storeReg a (av .&. v)
    av' <- loadReg a
    setZN av'
  RRA -> do
    v <- case mode of
      ACC -> loadReg a
      _ -> readMemPort addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    setFlag Carry (toEnum $ fromIntegral $ v .&. 1)
    let shiftedV = (v `shiftR` 1) .|. (cv `shiftL` 7)
    case mode of
      ACC -> storeReg a shiftedV
      _ -> writeMemPort addr shiftedV
    setZN shiftedV

    av <- loadReg a
    bv <- readMemPort addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    storeReg a (av + bv + cv)
    av' <- loadReg a
    setZN av'
    let shouldCarry = toInt av + toInt bv + toInt cv > 0xFF
    let doesOverflow =
          ((av `xor` bv) .&. 0x80) == 0 && ((av `xor` av') .&. 0x80) /= 0
    setFlag Carry shouldCarry
    setFlag Overflow doesOverflow
  SLO -> do
    v <- case mode of
      ACC -> loadReg a
      _ -> readMemPort addr
    let i = (v `shiftR` 7) .&. 1
    setFlag Carry (toEnum . fromIntegral $ i)
    let shiftedV = v `shiftL` 1
    case mode of
      ACC -> storeReg a shiftedV
      _ -> writeMemPort addr shiftedV
    setZN shiftedV

    v <- readMemPort addr
    av <- loadReg a
    let newAv = av .|. v
    storeReg a newAv
    setZN newAv
  SRE -> do
    v <- case mode of
      ACC -> loadReg a
      _ -> readMemPort addr

    setFlag Carry (toEnum . fromIntegral $ v .&. 1)
    let shiftedV = v `shiftR` 1
    case mode of
      ACC -> storeReg a shiftedV
      _ -> writeMemPort addr shiftedV
    setZN shiftedV

    v <- readMemPort addr
    av <- loadReg a
    let newAv = av `xor` v
    storeReg a newAv
    setZN newAv
  ANC -> do
    av <- loadReg a
    v <- readMemPort addr
    storeReg a (av .&. v)
    av' <- loadReg a
    setZN av'
    z <- getFlag Negative
    setFlag Carry z
  ALR -> do
    pure ()
  ARR -> do
    pure ()
  XAA -> do
    pure ()
  AHX -> do
    pure ()
  TAS -> do
    pure ()
  SHX -> do
    xv <- loadReg x
    yv <- loadReg y
    let result = xv .&. (toWord8 addr + 1)
    let temp = (toWord8 addr - yv) .&. 255
    if yv + temp <= 255
      then writeMemPort addr result
      else do
        v <- readMemPort addr
        writeMemPort addr v
  SHY -> do
    pure ()
  LAS -> do
    pure ()
  AXS -> do
    av <- loadReg a
    xv <- loadReg x
    v <- readMemPort addr
    let anded = av .&. xv
    let newXv = anded - v
    storeReg x newXv
    setFlag Carry (anded >= v)
    setZN newXv

handleIrq :: Emulator ()
handleIrq = do
  int <- loadReg interrupt
  case int of
    Just NMI -> do
      pcv <- loadReg pc
      pushW pcv
      p <- loadReg p
      push $ p .|. 0x10
      v <- readMemW 0xFFFA
      storeReg pc v
      modReg cpuCycles (+ 7)
    Just IRQ -> error "not handling IRQ yet"
    Nothing  -> pure ()
  storeReg interrupt Nothing

branch :: Emulator Bool -> Word16 -> Emulator ()
branch cond addr = do
  cv <- cond
  pcv <- loadReg pc
  when cv $ do
    storeReg pc addr
    let cycles = if (pcv .&. 0xFF00) /= (addr .&. 0xFF00) then 2 else 1
    modReg cpuCycles (+ cycles)

pull :: Emulator Word8
pull = do
  spv <- loadReg sp
  storeReg sp (spv + 1)
  let i = 0x100 .|. (toWord16 spv + 1)
  readMemPort i

pullW :: Emulator Word16
pullW = do
  lo <- pull
  hi <- pull
  pure $ makeW16 lo hi

push :: Word8 -> Emulator ()
push v = do
  spv <- loadReg sp
  let i = 0x100 .|. toWord16 spv
  writeMemPort i v
  storeReg sp (spv - 1)

pushW :: Word16 -> Emulator ()
pushW v = do
  let (lo, hi) = splitW16 v
  push hi
  push lo

getFlag :: Flag -> Emulator Bool
getFlag flag = do
  v <- loadReg p
  pure $ testBit v (7 - fromEnum flag)

setFlag :: Flag -> Bool -> Emulator ()
setFlag flag b = do
  v <- loadReg p
  storeReg p (opBit v (7 - fromEnum flag))
  where
    opBit = if b then setBit else clearBit

setZ :: Word8 -> Emulator ()
setZ v = setFlag Zero (v == 0)

setN :: Word8 -> Emulator ()
setN v = setFlag Negative (v .&. 0x80 /= 0)

setV :: Word8 -> Emulator ()
setV v = setFlag Overflow (v .&. 0x40 /= 0)

setZN :: Word8 -> Emulator ()
setZN v = setZ v >> setN v

compare :: Word8 -> Word8 -> Emulator ()
compare a b = do
  setZN $ a - b
  setFlag Carry (a >= b)
