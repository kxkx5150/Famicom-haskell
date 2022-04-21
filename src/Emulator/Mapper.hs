module Emulator.Mapper
  ( Mapper (..),
    new,
  )
where

import Data.Word
import qualified Emulator.Mapper.Mapper2 as Mapper2
import Emulator.Rom

data Mapper = Mapper
  { read :: Word16 -> IO Word8,
    write :: Word16 -> Word8 -> IO ()
  }

new :: Cartridge -> IO Mapper
new cart = case mapperType cart of
  0 -> mapper2 cart
  2 -> mapper2 cart
  66 -> mapper2 cart
  other -> error $ "Unsupported mapper type " ++ show other

mapper2 :: Cartridge -> IO Mapper
mapper2 cart = do
  m2 <- Mapper2.new cart
  pure $ Mapper (Mapper2.read m2) (Mapper2.write m2)
