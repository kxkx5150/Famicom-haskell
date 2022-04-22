module Emulator.Mem
  ( readCpuMemory8,
    readMemW,
    loadPpu,
    storePpu,
    modifyPpu,
    readPpuMemory,
    writeControl,
    writeMask,
    readPalette,
    writeScreen,
    loadScreen,
    toggleNmi,
    writeCpuMemory8,
    writeDMA,
    writeOAMData,
    writeOAMAddress,
    readOAMData,
  )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    ask,
    runReaderT,
  )
import Control.Monad.Trans (MonadIO)
import Data.Bits
  ( shiftL,
    shiftR,
    testBit,
    (.&.),
    (.|.),
  )
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as VUM
import Data.Word
import qualified Emulator.Mapper as Mapper
import Emulator.Nes
import Emulator.Rom as Cartridge
import Emulator.Util.Util

readCpuMemory8 :: Word16 -> Emulator Word8
readCpuMemory8 addr
  | addr < 0x2000 = readMem addr
  | addr < 0x4000 = readPPURegister $ 0x2000 + addr `rem` 8
  | addr == 0x4014 = readPPURegister addr
  | addr == 0x4015 = pure 0
  | addr == 0x4016 = pure 0
  | addr == 0x4017 = pure 0
  | addr < 0x6000 = pure 0
  | addr >= 0x6000 = readMapper addr
  | otherwise = error $ "memory read error at " ++ show addr ++ "!"

writeCpuMemory8 :: Word16 -> Word8 -> Emulator ()
writeCpuMemory8 addr value
  | addr < 0x2000 = writeMem addr value
  | addr < 0x4000 = writePPURegister (0x2000 + addr `rem` 8) value
  | addr == 0x4014 = writeDMA value
  | addr == 0x4016 = pure ()
  | addr >= 0x4000 && addr <= 0x4017 = pure ()
  | addr >= 0x4018 && addr <= 0x401F = pure ()
  | addr >= 0x6000 = writeMapper addr value
  | otherwise = error $ "memory write error at " ++ show addr ++ "!"

readMem :: Word16 -> Emulator Word8
readMem addr =
  with cpu $ \cpu -> VUM.read (ram cpu) (fromIntegral addr `rem` 0x0800)

readMemW :: Word16 -> Emulator Word16
readMemW addr = do
  lo <- readCpuMemory8 addr
  hi <- readCpuMemory8 (addr + 1)
  pure $ makeW16 lo hi

writeMem :: Word16 -> Word8 -> Emulator ()
writeMem addr v =
  with cpu $ \cpu -> VUM.write (ram cpu) (fromIntegral addr `rem` 0x0800) v

loadPpu :: (PPU -> IORef b) -> Emulator b
loadPpu field = with (field . ppu) readIORef

storePpu :: (PPU -> IORef b) -> b -> Emulator ()
storePpu field v = with (field . ppu) (`modifyIORef'` const v)

modifyPpu :: (PPU -> IORef b) -> (b -> b) -> Emulator ()
modifyPpu field v = with (field . ppu) (`modifyIORef'` v)








writeDMA :: Word8 -> Emulator ()
writeDMA v = do
  let startingAddr = toWord16 v `shiftL` 8
  let addresses = fmap (+ startingAddr) [0 .. 255]
  forM_
    addresses
    ( \addr -> do
        oamA <- loadPpu oamAddress
        oamV <- readCpuMemory8 addr
        with ppu $ \ppu -> VUM.write (oamData ppu) (toInt oamA) oamV
        modifyPpu oamAddress (+ 1)
    )

readOAMData' :: Emulator Word8
readOAMData' = do
  addr <- loadPpu oamAddress
  with ppu $ \ppu -> VUM.read (oamData ppu) (fromIntegral addr `rem` 0x0800)

readOAMData :: Word16 -> Emulator Word8
readOAMData addr =
  with ppu $ \ppu -> VUM.read (oamData ppu) (fromIntegral addr)

writeOAMAddress :: Word8 -> Emulator ()
writeOAMAddress = storePpu oamAddress

writeOAMData :: Word8 -> Emulator ()
writeOAMData v = do
  addr <- loadPpu oamAddress
  with ppu $ \ppu -> VUM.write (oamData ppu) (toInt addr) v
  modifyPpu oamAddress (+ 1)

readMapper :: Word16 -> Emulator Word8
readMapper addr = with mapper $ \mapper -> Mapper.read mapper addr

writeMapper :: Word16 -> Word8 -> Emulator ()
writeMapper addr value =
  with mapper $ \mapper -> Mapper.write mapper addr value

toggleNmi :: Bool -> Emulator ()
toggleNmi occurred = do
  storePpu nmiOccurred occurred
  output <- loadPpu nmiOutput
  occurred <- loadPpu nmiOccurred
  previous <- loadPpu nmiPrevious
  let nmi = output && occurred
  when (nmi && not previous) $ storePpu nmiDelay 15
  storePpu nmiPrevious nmi











readPpuMemory :: Word16 -> Emulator Word8
readPpuMemory addr
  | addr' < 0x2000 = readMapper addr'
  | addr' < 0x3F00 = readNametableData addr'
  | addr' < 0x4000 = readPalette addr'
  | otherwise = error $ "Erroneous read detected at " ++ show addr ++ "!"
  where
    addr' = addr `rem` 0x4000

writePPUMemory :: Word16 -> Word8 -> Emulator ()
writePPUMemory addr v
  | addr' < 0x2000 = writeMapper addr' v
  | addr' < 0x3F00 = writeNametableData addr' v
  | addr' < 0x4000 = writePalette addr' v
  | otherwise = error $ "Erroneous write detected at " ++ show addr ++ "!"
  where
    addr' = addr `rem` 0x4000






readPPURegister :: Word16 -> Emulator Word8
readPPURegister addr = case addr of
  0x2002 -> readStatus
  0x2004 -> readOAMData'
  0x2007 -> readData
  other -> pure 0

writePPURegister :: Word16 -> Word8 -> Emulator ()
writePPURegister addr v = do
  storePpu ppuRegister v
  case addr of
    0x2000 -> writeControl v
    0x2001 -> writeMask v
    0x2003 -> writeOAMAddress v
    0x2004 -> writeOAMData v
    0x2005 -> writeScroll v
    0x2006 -> writeAddress v
    0x2007 -> writeData v
    _ -> error $ "Erroneous write detected at " ++ show addr ++ "!"

writeNametableData :: Word16 -> Word8 -> Emulator ()
writeNametableData addr v = do
  mirror <- with cart $ \cart -> readIORef $ Cartridge.mirror cart
  let addr' = fromIntegral (mirroredNametableAddr addr mirror) `rem` 0x800
  with ppu $ \ppu -> VUM.write (nameTableData ppu) addr' v

readNametableData :: Word16 -> Emulator Word8
readNametableData addr = do
  mirror <- with cart $ \cart -> readIORef $ Cartridge.mirror cart
  let addr' = fromIntegral (mirroredNametableAddr addr mirror) `rem` 0x800
  with ppu $ \ppu -> VUM.read (nameTableData ppu) addr'

writePalette :: Word16 -> Word8 -> Emulator ()
writePalette addr value = with ppu $ \ppu ->
  VUM.write (paletteData ppu) (fromIntegral $ mirroredPaletteAddr addr) value

readPalette :: Word16 -> Emulator Word8
readPalette addr = with ppu $ \ppu ->
  VUM.read (paletteData ppu) (fromIntegral $ mirroredPaletteAddr addr)

readStatus :: Emulator Word8
readStatus = do
  registerV <- loadPpu ppuRegister
  spriteOverflowV <- loadPpu spriteOverflow
  spriteZeroHitV <- loadPpu spriteZeroHit
  nmiOccurred' <- loadPpu nmiOccurred
  let r = registerV .&. 0x1F
  let r' = r .|. fromIntegral (fromEnum spriteOverflowV `shiftL` 5)
  let r'' = r' .|. fromIntegral (fromEnum spriteZeroHitV `shiftL` 6)
  let rFinal = r'' .|. fromIntegral (fromEnum nmiOccurred' `shiftL` 7)
  toggleNmi False
  storePpu writeToggle False
  pure $ fromIntegral rFinal

readData :: Emulator Word8
readData = do
  addr <- loadPpu currentVramAddress

  rv <-
    if (addr `rem` 0x4000) < 0x3F00
      then do
        v <- readPpuMemory addr
        buffered <- loadPpu dataV
        storePpu dataV v
        pure buffered
      else do
        v <- readPpuMemory (addr - 0x1000)
        storePpu dataV v
        readPpuMemory addr

  incMode <- loadPpu incrementMode
  let inc = case incMode of
        Horizontal -> 1
        Vertical -> 32
  modifyPpu currentVramAddress (+ inc)
  pure rv

writeData :: Word8 -> Emulator ()
writeData v = do
  addr <- loadPpu currentVramAddress
  writePPUMemory addr v
  incMode <- loadPpu incrementMode
  let inc = case incMode of
        Horizontal -> 1
        Vertical -> 32
  modifyPpu currentVramAddress (+ inc)

writeControl :: Word8 -> Emulator ()
writeControl v = do
  storePpu nameTable $ case (v `shiftR` 0) .&. 3 of
    0 -> 0x2000
    1 -> 0x2400
    2 -> 0x2800
    3 -> 0x2C00
  storePpu incrementMode $ if testBit v 2 then Vertical else Horizontal
  storePpu spriteTable $ if testBit v 3 then 0x1000 else 0x0000
  storePpu bgTable $ if testBit v 4 then 0x1000 else 0x0000
  storePpu spriteSize $ if testBit v 5 then Double else Normal

  storePpu nmiOutput $ testBit v 7
  nmiOccurred' <- loadPpu nmiOccurred
  toggleNmi nmiOccurred'
  tv <- loadPpu tempVramAddress
  storePpu
    tempVramAddress
    ((tv .&. 0xF3FF) .|. (toWord16 v .&. 0x03) `shiftL` 10)

writeMask :: Word8 -> Emulator ()
writeMask v = do
  storePpu colorMode $ if testBit v 0 then Grayscale else Color
  storePpu leftBgVisibility $ if testBit v 1 then Shown else Hidden
  storePpu leftSpritesVisibility $ if testBit v 2 then Shown else Hidden
  storePpu bgVisibility $ testBit v 3
  storePpu spriteVisibility $ testBit v 4
  storePpu intensifyReds $ testBit v 5
  storePpu intensifyGreens $ testBit v 6
  storePpu intensifyBlues $ testBit v 7

writeScroll :: Word8 -> Emulator ()
writeScroll v = do
  wv <- loadPpu writeToggle
  tv <- loadPpu tempVramAddress
  if wv
    then do
      let tv' = (tv .&. 0x8FFF) .|. ((toWord16 v .&. 0x07) `shiftL` 12)
      let tv'' = (tv' .&. 0xFC1F) .|. ((toWord16 v .&. 0xF8) `shiftL` 2)
      storePpu tempVramAddress tv''
      storePpu writeToggle False
    else do
      let tv' = (tv .&. 0xFFE0) .|. (toWord16 v `shiftR` 3)
      storePpu tempVramAddress tv'
      storePpu fineX $ v .&. 0x07
      storePpu writeToggle True

writeAddress :: Word8 -> Emulator ()
writeAddress v = do
  wv <- loadPpu writeToggle
  tv <- loadPpu tempVramAddress
  if wv
    then do
      let tv' = (tv .&. 0xFF00) .|. toWord16 v
      storePpu tempVramAddress tv'
      storePpu currentVramAddress tv'
      storePpu writeToggle False
    else do
      let tv' = (tv .&. 0x80FF) .|. ((toWord16 v .&. 0x3F) `shiftL` 8)
      storePpu tempVramAddress tv'
      storePpu writeToggle True

mirroredPaletteAddr :: Word16 -> Word16
mirroredPaletteAddr addr =
  if addr' >= 16 && addr' `rem` 4 == 0
    then addr' - 16
    else addr'
  where
    addr' = addr `rem` 32

mirroredNametableAddr :: Word16 -> Mirror -> Word16
mirroredNametableAddr addr mirror = 0x2000 + lookup + offset
  where
    addr' = (addr - 0x2000) `rem` 0x1000
    tableIndex = fromIntegral $ addr' `div` 0x0400
    lookup =
      ((nameTableMirrorLookup V.! fromEnum mirror) V.! tableIndex) * 0x0400
    offset = fromIntegral $ addr' `rem` 0x0400
    nameTableMirrorLookup =
      V.fromList
        ( fmap
            V.fromList
            [[0, 0, 1, 1], [0, 1, 0, 1], [0, 0, 0, 0], [1, 1, 1, 1], [0, 1, 2, 3]]
        )

loadScreen :: Emulator (VUM.IOVector Word8)
loadScreen = with ppu $ \ppu -> pure $ screen ppu

writeScreen :: Coords -> Color -> Emulator ()
writeScreen (x, y) (r, g, b) = with ppu $ \ppu -> do
  let offset = (x + (y * 256)) * 3
  VUM.write (screen ppu) (offset + 0) r
  VUM.write (screen ppu) (offset + 1) g
  VUM.write (screen ppu) (offset + 2) b



