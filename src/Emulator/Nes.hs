{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator.Nes
  ( Nes (..),
    CPU (..),
    PPU (..),
    Emulator (..),
    Sprite (..),
    Coords,
    Color,
    IncrementMode (..),
    ColorMode (..),
    Visibility (..),
    SpriteSize (..),
    Interrupt (..),
    newNes,
    initNes,
    with,
    loadReg,
    storeReg,
    modReg,
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
import Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as VUM
import Data.Word
import qualified Emulator.Mapper as Mapper
import Emulator.Rom as Cartridge
import Emulator.Util
import Prelude hiding
  ( read,
    replicate,
  )

newtype Emulator a = Emulator {unNes :: ReaderT Nes IO a}
  deriving (Monad, Applicative, Functor, MonadIO, MonadReader Nes)

data Sprite = Sprite
  { sIndex :: Int,
    sCoords :: Coords,
    sTileIndexByte :: Word8,
    sAttributeByte :: Word8,
    sPattern :: Word32,
    sPriority :: Word8
  }
  deriving (Show, Eq)

type Coords = (Int, Int)

type Color = (Word8, Word8, Word8)

data IncrementMode = Horizontal | Vertical

data SpriteSize = Normal | Double

data ColorMode = Color | Grayscale

data Visibility = Hidden | Shown

data Interrupt
  = IRQ
  | NMI
  deriving (Eq, Show)

data Nes = Nes
  { cpu :: CPU,
    ppu :: PPU,
    cart :: Cartridge,
    mapper :: Mapper.Mapper
  }

data CPU = CPU
  { pc :: IORef Word16,
    sp :: IORef Word8,
    a :: IORef Word8,
    x :: IORef Word8,
    y :: IORef Word8,
    p :: IORef Word8,
    cpuCycles :: IORef Int,
    interrupt :: IORef (Maybe Interrupt),
    ram :: VUM.IOVector Word8
  }

data PPU = PPU
  { -- Misc
    ppuCycles :: IORef Int,
    scanline :: IORef Int,
    frameCount :: IORef Int,
    writeToggle :: IORef Bool,
    ppuRegister :: IORef Word8,
    oddFrame :: IORef Bool,
    -- Data
    oamData :: VUM.IOVector Word8,
    nameTableData :: VUM.IOVector Word8,
    paletteData :: VUM.IOVector Word8,
    screen :: VUM.IOVector Word8,
    -- Addresses
    currentVramAddress :: IORef Word16,
    tempVramAddress :: IORef Word16,
    oamAddress :: IORef Word8,
    -- NMI
    nmiOutput :: IORef Bool,
    nmiOccurred :: IORef Bool,
    nmiDelay :: IORef Word8,
    nmiPrevious :: IORef Bool,
    -- Control register bits
    nameTable :: IORef Word16,
    incrementMode :: IORef IncrementMode,
    spriteTable :: IORef Word16,
    bgTable :: IORef Word16,
    spriteSize :: IORef SpriteSize,
    -- Mask register bits
    colorMode :: IORef ColorMode,
    leftBgVisibility :: IORef Visibility,
    leftSpritesVisibility :: IORef Visibility,
    bgVisibility :: IORef Bool,
    spriteVisibility :: IORef Bool,
    intensifyReds :: IORef Bool,
    intensifyGreens :: IORef Bool,
    intensifyBlues :: IORef Bool,
    -- Status register bits
    spriteOverflow :: IORef Bool,
    spriteZeroHit :: IORef Bool,
    -- verticalBlank         :: IORef Bool,
    -- Scroll register
    fineX :: IORef Word8,
    -- Data register
    dataV :: IORef Word8,
    -- Temp vars
    nameTableByte :: IORef Word8,
    attrTableByte :: IORef Word8,
    loTileByte :: IORef Word8,
    hiTileByte :: IORef Word8,
    tileData :: IORef Word64,
    sprites :: IORef (V.Vector Sprite)
  }

initNes :: BS.ByteString -> Emulator a -> IO a
initNes bs (Emulator reader) = do
  cart <- Cartridge.parse bs
  nes <- newNes cart
  runReaderT reader nes

newNes :: Cartridge -> IO Nes
newNes cart = do
  cpu <- newCPU
  ppu <- newPPU
  mapper <- Mapper.new cart
  pure $ Nes cpu ppu cart mapper

newCPU :: IO CPU
newCPU = do
  pc <- newIORef 0x0
  sp <- newIORef 0xFD
  a <- newIORef 0x0
  x <- newIORef 0x0
  y <- newIORef 0x0
  p <- newIORef 0x24 -- should this be 0x34?
  cycles <- newIORef 0
  interrupt <- newIORef Nothing
  ram <- VUM.replicate 65536 0x0
  pure $ CPU pc sp a x y p cycles interrupt ram

newPPU :: IO PPU
newPPU = do
  -- Misc
  cycles <- newIORef 0
  scanline <- newIORef 0
  frameCount <- newIORef 0
  writeToggle <- newIORef False
  ppuRegister <- newIORef 0x0
  oddFrame <- newIORef False
  -- Data
  oamData <- VUM.replicate 0x100 0x0
  nameTableData <- VUM.replicate 0x800 0x0
  paletteData <- VUM.replicate 0x20 0x0
  screen <- VUM.replicate (256 * 240 * 3) 255
  -- Addresses
  currentVramAddress <- newIORef 0x0
  tempVramAddress <- newIORef 0x0
  oamAddress <- newIORef 0x0
  -- NMI
  nmiOutput <- newIORef False
  nmiOccurred <- newIORef False
  nmiDelay <- newIORef 0
  nmiPrevious <- newIORef False
  -- Control register
  nameTable <- newIORef 0x2000
  incrementMode <- newIORef Horizontal
  spriteTable <- newIORef 0x0000
  bgTable <- newIORef 0x0000
  spriteSize <- newIORef Normal
  -- Mask register
  colorMode <- newIORef Color
  leftBgVis <- newIORef Hidden
  leftSpritesVis <- newIORef Hidden
  bgVis <- newIORef False
  spriteVis <- newIORef False
  intensifyReds <- newIORef False
  intensifyGreens <- newIORef False
  intensifyBlues <- newIORef False
  -- Status register
  spriteOverflow <- newIORef False
  spriteZeroHit <- newIORef False
  -- Scroll register
  fineX <- newIORef 0x0
  -- Data register
  dataV <- newIORef 0x0
  -- Temp vars
  nameTableByte <- newIORef 0x0
  attrTableByte <- newIORef 0x0
  loTileByte <- newIORef 0x0
  hiTileByte <- newIORef 0x0
  tileData <- newIORef 0x0
  sprites <- newIORef V.empty

  pure $
    PPU
      -- Misc
      cycles
      scanline
      frameCount
      writeToggle
      ppuRegister
      oddFrame
      -- Data
      oamData
      nameTableData
      paletteData
      screen
      -- Addresses
      currentVramAddress
      tempVramAddress
      oamAddress
      -- NMI
      nmiOutput
      nmiOccurred
      nmiDelay
      nmiPrevious
      -- Control register
      nameTable
      incrementMode
      spriteTable
      bgTable
      spriteSize
      -- Mask register
      colorMode
      leftBgVis
      leftSpritesVis
      bgVis
      spriteVis
      intensifyReds
      intensifyGreens
      intensifyBlues
      -- Status register
      spriteOverflow
      spriteZeroHit
      -- Scroll register
      fineX
      -- Data register
      dataV
      -- Temp vars
      nameTableByte
      attrTableByte
      loTileByte
      hiTileByte
      tileData
      sprites

with :: (Nes -> b) -> (b -> IO a) -> Emulator a
with field f = do
  nes <- ask
  liftIO $ f (field nes)

loadReg :: (CPU -> IORef b) -> Emulator b
loadReg field = with (field . cpu) readIORef

storeReg :: (CPU -> IORef b) -> b -> Emulator ()
storeReg field v = with (field . cpu) (`modifyIORef'` const v)

modReg :: (CPU -> IORef b) -> (b -> b) -> Emulator ()
modReg field v = with (field . cpu) (`modifyIORef'` v)

