module Emulator.Rom
  ( Cartridge(..)
  , Mirror(..)
  , parse
  ) where

import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString               as BS
import           Data.IORef
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Data.Word
import           Emulator.Util                  ( sliceBS )
import           Prelude                 hiding ( read )

data INesFileHeader = INesFileHeader
  { format   :: Word8
  , numPrg   :: Int
  , numChr   :: Int
  , control1 :: Int
  , control2 :: Int
  , numRam   :: Int
  }
  deriving (Eq, Show)

data Mirror
  = MirrorHorizontal
  | MirrorVertical
  | MirrorSingle0
  | MirrorSingle1
  | MirrorFour
  deriving (Eq, Show, Enum)

data Cartridge = Cartridge
  { chrRom     :: VUM.MVector RealWorld Word8
  , prgRom     :: VUM.MVector RealWorld Word8
  , sram       :: VUM.MVector RealWorld Word8
  , mirror     :: IORef Mirror
  , mapperType :: Int
  }

parseHeader :: BS.ByteString -> INesFileHeader
parseHeader bs = INesFileHeader (fromIntegral $ BS.index bs 3)
                                (fromIntegral $ BS.index bs 4)
                                (fromIntegral $ BS.index bs 5)
                                (fromIntegral $ BS.index bs 6)
                                (fromIntegral $ BS.index bs 7)
                                (fromIntegral $ BS.index bs 8)

parse :: BS.ByteString -> IO Cartridge
parse bs = do
  let (INesFileHeader _ numPrg numChr ctrl1 ctrl2 _) = parseHeader bs
  let prgOffset = numPrg * 0x4000
  let prgRom    = sliceBS 0x10 (0x10 + prgOffset) bs
  let chrOffset = numChr * 0x2000
  let chrRom = if numChr == 0
        then (BS.replicate 0x2000 0)
        else sliceBS (0x10 + prgOffset) (0x10 + prgOffset + chrOffset) bs

  chr  <- VU.thaw $ VU.fromList $ BS.unpack chrRom
  prg  <- VU.thaw $ VU.fromList $ BS.unpack prgRom
  sram <- VUM.replicate 0x2000 0

  let mirrorV = (ctrl1 .&. 1) .|. (((ctrl1 `shiftR` 3) .&. 1) `shiftR` 1)
  mirror <- newIORef $ toEnum mirrorV
  let mapper = (ctrl1 `shiftR` 4) .|. ((ctrl2 `shiftR` 4) `shiftL` 4)
  pure $ Cartridge chr prg sram mirror mapper


