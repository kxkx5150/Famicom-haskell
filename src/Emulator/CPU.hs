module Emulator.CPU
  ( reset
  , runCpu
  , runCpuT
  ) where

import           Control.Monad
import           Data.Bits               hiding ( bit )
import           Data.Word
import           Emulator.Mem
import           Emulator.Opcode
import           Emulator.Trace                 ( Trace
                                                , mkTrace
                                                , renderTrace
                                                )
import           Emulator.Util
import           Prelude                 hiding ( and
                                                , compare
                                                )


runCpu :: Emulator Int
runCpu = do
  startingCycles      <- loadCpu cpuCycles
  opcode              <- loadOp
  (pageCrossed, addr) <- getAddr (mode opcode)
  incPc opcode
  modifyCpu cpuCycles (+ getCycles opcode pageCrossed)
  execOp opcode addr
  endingCycles <- loadCpu cpuCycles
  pure $ endingCycles - startingCycles

runCpuT :: Emulator (Int, Trace)
runCpuT = do
  startingCycles      <- loadCpu cpuCycles
  opcode              <- loadOp
  trace               <- mkTrace opcode
  (pageCrossed, addr) <- getAddr (mode opcode)
  incPc opcode
  modifyCpu cpuCycles (+ getCycles opcode pageCrossed)
  execOp opcode addr
  endingCycles <- loadCpu cpuCycles
  pure (endingCycles - startingCycles, trace)

reset :: Emulator ()
reset = do
  v <- readCpuMemory16 0xFFFC
  storeCpu pc v
  storeCpu sp 0xFD
  storeCpu p  0x24

loadOp :: Emulator Opcode
loadOp = do
  pcv <- loadCpu pc
  av  <- readCpuMemory8 pcv
  pure $ decodeOpcode av

getAddr :: AddressMode -> Emulator (Bool, Word16)
getAddr mode = case mode of
  IMM -> do
    pcv <- loadCpu pc
    pure (False, pcv + 1)
  IMP -> pure (False, 0)
  ABS -> do
    pcv   <- loadCpu pc
    addrV <- readCpuMemory16 (pcv + 1)
    pure (False, addrV)
  ABX -> do
    pcv <- loadCpu pc
    xv  <- loadCpu x
    v   <- readCpuMemory16 (pcv + 1)
    let addrV       = v + toWord16 xv
    let pageCrossed = ((addrV - toWord16 xv) .&. 0xFF00) /= (addrV .&. 0xFF00)
    pure (pageCrossed, addrV)
  ABY -> do
    pcv <- loadCpu pc
    yv  <- loadCpu y
    v   <- readCpuMemory16 (pcv + 1)
    let addrV       = v + toWord16 yv
    let pageCrossed = ((addrV - toWord16 yv) .&. 0xFF00) /= (addrV .&. 0xFF00)
    pure (pageCrossed, addrV)
  ACC -> pure (False, 0)

  IND         -> do
    pcv  <- loadCpu pc
    addr <- readCpuMemory16 (pcv + 1)
    lo   <- readCpuMemory8 addr
    hi   <- readCpuMemory8 $ (addr .&. 0xFF00) .|. toWord16 (toWord8 addr + 1)
    pure (False, makeW16 lo hi)
  IZY -> do
    pcv <- loadCpu pc
    yv  <- loadCpu y
    v   <- readCpuMemory8 $ pcv + 1
    lo  <- readCpuMemory8 (toWord16 v)
    hi  <- readCpuMemory8 $ ((toWord16 v) .&. 0xFF00) .|. toWord16
      (toWord8 (toWord16 v) + 1)
    let addrV       = (makeW16 lo hi) + toWord16 yv
    let pageCrossed = ((addrV - toWord16 yv) .&. 0xFF00) /= (addrV .&. 0xFF00)
    pure (pageCrossed, addrV)
  IZX -> do
    pcv <- loadCpu pc
    xv  <- loadCpu x
    v   <- readCpuMemory8 $ pcv + 1
    lo  <- readCpuMemory8 (toWord16 (v + xv))
    hi  <- readCpuMemory8 $ ((toWord16 (v + xv)) .&. 0xFF00) .|. toWord16
      (toWord8 (toWord16 (v + xv)) + 1)
    pure (False, (makeW16 lo hi))
  REL -> do
    pcv      <- loadCpu pc
    offset16 <- readCpuMemory16 (pcv + 1)
    let offset8 = firstNibble offset16
    let diff    = if offset8 < 0x80 then 0 else 0x100
    pure (False, pcv + 2 + offset8 - diff)
  ZP -> do
    pcv <- loadCpu pc
    v   <- readCpuMemory8 (pcv + 1)
    pure (False, toWord16 v)
  ZPX -> do
    pcv <- loadCpu pc
    xv  <- loadCpu x
    v   <- readCpuMemory8 (pcv + 1)
    pure (False, toWord16 $ v + xv)
  ZPY -> do
    pcv <- loadCpu pc
    yv  <- loadCpu y
    v   <- readCpuMemory8 (pcv + 1)
    pure (False, toWord16 $ v + yv)

incPc :: Opcode -> Emulator ()
incPc opcode = modifyCpu pc (+ instrLength)
  where instrLength = fromIntegral $ len opcode

getCycles :: Opcode -> Bool -> Int
getCycles opcode pageCrossed =
  if pageCrossed then pageCrossCycles opcode + cycles opcode else cycles opcode

execOp :: Opcode -> Word16 -> Emulator ()
execOp (Opcode _ mnemonic mode _ _ _) addr = case mnemonic of
  ADC -> do
    av <- loadCpu a
    bv <- readCpuMemory8 addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    storeCpu a (av + bv + cv)
    av' <- loadCpu a
    setZN av'
    let shouldCarry = toInt av + toInt bv + toInt cv > 0xFF
    let doesOverflow =
          ((av `xor` bv) .&. 0x80) == 0 && ((av `xor` av') .&. 0x80) /= 0
    setFlag Carry    shouldCarry
    setFlag Overflow doesOverflow
  AND -> do
    av <- loadCpu a
    v  <- readCpuMemory8 addr
    storeCpu a (av .&. v)
    av' <- loadCpu a
    setZN av'
  ASL -> do
    v <- case mode of
      ACC -> loadCpu a
      _           -> readCpuMemory8 addr
    let i = (v `shiftR` 7) .&. 1
    setFlag Carry (toEnum . fromIntegral $ i)
    let shiftedV = v `shiftL` 1
    case mode of
      ACC -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
    setZN shiftedV
  BCC -> do
    branch (not <$> getFlag Carry) addr
  BCS -> do
    branch (getFlag Carry) addr
  BEQ -> do
    branch (getFlag Zero) addr
  BIT -> do
    v <- readCpuMemory8 addr
    -- setV $ (v `shiftR` 6) .&. 1
    setV v
    av <- loadCpu a
    setZ (v .&. av)
    setN v
  BMI -> do
    branch (getFlag Negative) addr
  BNE -> do
    branch (not <$> getFlag Zero) addr
  BPL -> do
    branch (not <$> getFlag Negative) addr
  BRK -> do
    pcv <- loadCpu pc
    pushW $ pcv + 1
    p <- loadCpu p
    push $ p .|. 0x10
    setFlag InterruptDisable True
    av <- readCpuMemory16 0xFFFE
    storeCpu pc av
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
    v  <- readCpuMemory8 addr
    av <- loadCpu a
    compare av v
  CPX -> do
    v  <- readCpuMemory8 addr
    xv <- loadCpu x
    compare xv v
  CPY -> do
    v  <- readCpuMemory8 addr
    yv <- loadCpu y
    compare yv v
  DEC -> do
    v <- readCpuMemory8 addr
    let value = v - 1
    writeCpuMemory8 addr value
    setZN value
  DEX -> do
    v <- loadCpu x
    let value = v - 1
    storeCpu x value
    setZN value
  DEY -> do
    v <- loadCpu y
    let value = v - 1
    storeCpu y value
    setZN value
  EOR -> do
    v  <- readCpuMemory8 addr
    av <- loadCpu a
    let newAv = av `xor` v
    storeCpu a newAv
    setZN newAv
  INC -> do
    v <- readCpuMemory8 addr
    let value = v + 1
    writeCpuMemory8 addr value
    setZN value
  INX -> do
    v <- loadCpu x
    let value = v + 1
    storeCpu x value
    setZN value
  INY -> do
    v <- loadCpu y
    let value = v + 1
    storeCpu y value
    setZN value
  JMP -> do
    storeCpu pc addr
  JSR -> do
    pcv <- loadCpu pc
    pushW $ pcv - 1
    storeCpu pc addr
  LDA -> do
    v <- readCpuMemory8 addr
    storeCpu a v
    setZN v
  LDX -> do
    v <- readCpuMemory8 addr
    storeCpu x v
    setZN v
  LDY -> do
    v <- readCpuMemory8 addr
    storeCpu y v
    setZN v
  LSR -> do
    v <- case mode of
      ACC -> loadCpu a
      _           -> readCpuMemory8 addr

    setFlag Carry (toEnum . fromIntegral $ v .&. 1)
    let shiftedV = v `shiftR` 1
    case mode of
      ACC -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
    setZN shiftedV
  NOP -> do
    pure ()
  PHA -> do
    av <- loadCpu a
    push av
  PHP -> do
    p <- loadCpu p
    push $ p .|. 0x10
  PLA -> do
    v <- pull
    storeCpu a v
    setZN v
  PLP -> do
    v <- pull
    storeCpu p ((v .&. 0xEF) .|. 0x20)
  ORA -> do
    v  <- readCpuMemory8 addr
    av <- loadCpu a
    let newAv = av .|. v
    storeCpu a newAv
    setZN newAv
  RTI -> do
    addr <- pull
    storeCpu p (addr .&. 0xEf .|. 0x20)
    addr' <- pullW
    storeCpu pc addr'
  RTS -> do
    paddr <- pullW
    storeCpu pc (paddr + 1)
  ROR -> do
    v <- case mode of
      ACC -> loadCpu a
      _           -> readCpuMemory8 addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    setFlag Carry (toEnum $ fromIntegral $ v .&. 1)
    let shiftedV = (v `shiftR` 1) .|. (cv `shiftL` 7)
    case mode of
      ACC -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
    setZN shiftedV
  ROL -> do
    v <- case mode of
      ACC -> loadCpu a
      _           -> readCpuMemory8 addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    setFlag Carry (toEnum $ fromIntegral $ (v `shiftR` 7) .&. 1)
    let shiftedV = (v `shiftL` 1) .|. cv
    case mode of
      ACC -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
    setZN shiftedV
  SBC -> do
    av <- loadCpu a
    bv <- readCpuMemory8 addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    storeCpu a (av - bv - (1 - cv))
    av' <- loadCpu a
    setZN av'
    let shouldCarry = toInt av - toInt bv - toInt (1 - cv) >= 0
    let doesOverflow =
          ((av `xor` bv) .&. 0x80) /= 0 && ((av `xor` av') .&. 0x80) /= 0
    setFlag Carry    shouldCarry
    setFlag Overflow doesOverflow
  SEC -> do
    setFlag Carry True
  SED -> do
    setFlag Decimal True
  SEI -> do
    setFlag InterruptDisable True
  STA -> do
    av <- loadCpu a
    writeCpuMemory8 addr av
  STX -> do
    loadCpu x >>= writeCpuMemory8 addr
  STY -> do
    loadCpu y >>= writeCpuMemory8 addr
  TAX -> do
    av <- loadCpu a
    storeCpu x av
    xv <- loadCpu x
    setZN xv
  TAY -> do
    av <- loadCpu a
    storeCpu y av
    yv <- loadCpu y
    setZN yv
  TSX -> do
    spv <- loadCpu sp
    storeCpu x spv
    xv <- loadCpu x
    setZN xv
  TXA -> do
    xv <- loadCpu x
    storeCpu a xv
    av <- loadCpu a
    setZN av
  TXS -> do
    xv <- loadCpu x
    storeCpu sp xv
  TYA -> do
    yv <- loadCpu y
    storeCpu a yv
    av <- loadCpu a
    setZN av
  KIL -> do
    pure ()
  LAX -> do
    v <- readCpuMemory8 addr
    storeCpu a v
    storeCpu x v
    setZN v
  SAX -> do
    av <- loadCpu a
    xv <- loadCpu x
    writeCpuMemory8 addr (av .&. xv)
  DCP -> do
    v <- readCpuMemory8 addr
    let value = v - 1
    writeCpuMemory8 addr value
    setZN value

    v  <- readCpuMemory8 addr
    av <- loadCpu a
    compare av v
  ISC -> do
    v <- readCpuMemory8 addr
    let value = v + 1
    writeCpuMemory8 addr value
    setZN value

    av <- loadCpu a
    bv <- readCpuMemory8 addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    storeCpu a (av - bv - (1 - cv))
    av' <- loadCpu a
    setZN av'
    let shouldCarry = toInt av - toInt bv - toInt (1 - cv) >= 0
    let doesOverflow =
          ((av `xor` bv) .&. 0x80) /= 0 && ((av `xor` av') .&. 0x80) /= 0
    setFlag Carry    shouldCarry
    setFlag Overflow doesOverflow
  RLA -> do
    v <- case mode of
      ACC -> loadCpu a
      _           -> readCpuMemory8 addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    setFlag Carry (toEnum $ fromIntegral $ (v `shiftR` 7) .&. 1)
    let shiftedV = (v `shiftL` 1) .|. cv
    case mode of
      ACC -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
    setZN shiftedV

    av <- loadCpu a
    v  <- readCpuMemory8 addr
    storeCpu a (av .&. v)
    av' <- loadCpu a
    setZN av'
  RRA -> do
    v <- case mode of
      ACC -> loadCpu a
      _           -> readCpuMemory8 addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    setFlag Carry (toEnum $ fromIntegral $ v .&. 1)
    let shiftedV = (v `shiftR` 1) .|. (cv `shiftL` 7)
    case mode of
      ACC -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
    setZN shiftedV

    av <- loadCpu a
    bv <- readCpuMemory8 addr
    cv <- (fromIntegral . fromEnum) <$> getFlag Carry
    storeCpu a (av + bv + cv)
    av' <- loadCpu a
    setZN av'
    let shouldCarry = toInt av + toInt bv + toInt cv > 0xFF
    let doesOverflow =
          ((av `xor` bv) .&. 0x80) == 0 && ((av `xor` av') .&. 0x80) /= 0
    setFlag Carry    shouldCarry
    setFlag Overflow doesOverflow
  SLO -> do
    v <- case mode of
      ACC -> loadCpu a
      _           -> readCpuMemory8 addr
    let i = (v `shiftR` 7) .&. 1
    setFlag Carry (toEnum . fromIntegral $ i)
    let shiftedV = v `shiftL` 1
    case mode of
      ACC -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
    setZN shiftedV

    v  <- readCpuMemory8 addr
    av <- loadCpu a
    let newAv = av .|. v
    storeCpu a newAv
    setZN newAv
  SRE -> do
    v <- case mode of
      ACC -> loadCpu a
      _           -> readCpuMemory8 addr

    setFlag Carry (toEnum . fromIntegral $ v .&. 1)
    let shiftedV = v `shiftR` 1
    case mode of
      ACC -> storeCpu a shiftedV
      _           -> writeCpuMemory8 addr shiftedV
    setZN shiftedV

    v  <- readCpuMemory8 addr
    av <- loadCpu a
    let newAv = av `xor` v
    storeCpu a newAv
    setZN newAv
  ANC -> do
    av <- loadCpu a
    v  <- readCpuMemory8 addr
    storeCpu a (av .&. v)
    av' <- loadCpu a
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
    xv <- loadCpu x
    yv <- loadCpu y
    let result = xv .&. (toWord8 addr + 1)
    let temp   = (toWord8 addr - yv) .&. 255
    if yv + temp <= 255
      then writeCpuMemory8 addr result
      else do
        v <- readCpuMemory8 addr
        writeCpuMemory8 addr v
  SHY -> do
    pure ()
  LAS -> do
    pure ()
  AXS -> do
    av <- loadCpu a
    xv <- loadCpu x
    v  <- readCpuMemory8 addr
    let anded = av .&. xv
    let newXv = anded - v
    storeCpu x newXv
    setFlag Carry (anded >= v)
    setZN newXv

-- handleInterrupts :: Emulator ()
-- handleInterrupts = do
--   int <- loadCpu interrupt
--   case int of
--     Just NMI -> do
--       pcv <- loadCpu pc
--       pushW pcv
--       p <- loadCpu p
--       push $ p .|. 0x10
--       v <- readCpuMemory16 0xFFFA
--       storeCpu pc v
--       modifyCpu cpuCycles (+ 7)
--     Just IRQ -> error "not handling IRQ yet"
--     Nothing  -> pure ()
--   storeCpu interrupt Nothing

branch :: Emulator Bool -> Word16 -> Emulator ()
branch cond addr = do
  cv  <- cond
  pcv <- loadCpu pc
  when cv $ do
    storeCpu pc addr
    let cycles = if (pcv .&. 0xFF00) /= (addr .&. 0xFF00) then 2 else 1
    modifyCpu cpuCycles (+ cycles)

pull :: Emulator Word8
pull = do
  spv <- loadCpu sp
  storeCpu sp (spv + 1)
  let i = 0x100 .|. (toWord16 spv + 1)
  readCpuMemory8 i

pullW :: Emulator Word16
pullW = do
  lo <- pull
  hi <- pull
  pure $ makeW16 lo hi

push :: Word8 -> Emulator ()
push v = do
  spv <- loadCpu sp
  let i = 0x100 .|. toWord16 spv
  writeCpuMemory8 i v
  storeCpu sp (spv - 1)

pushW :: Word16 -> Emulator ()
pushW v = do
  let (lo, hi) = splitW16 v
  push hi
  push lo

getFlag :: Flag -> Emulator Bool
getFlag flag = do
  v <- loadCpu p
  pure $ testBit v (7 - fromEnum flag)

setFlag :: Flag -> Bool -> Emulator ()
setFlag flag b = do
  v <- loadCpu p
  storeCpu p (opBit v (7 - fromEnum flag))
  where opBit = if b then setBit else clearBit

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



