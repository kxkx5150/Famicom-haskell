
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator.Mem
  ( Nes(..)
  , CPU(..)
  , Emulator(..)
  , new
  , runEmulator
  , debug
  , loadCpu
  , storeCpu
  , modifyCpu
  , readCpuMemory8
  , readCpuMemory16
  , writeCpuMemory8
  , writeCpuMemory16
  ) where

import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , ask
                                                , runReaderT
                                                )
import           Control.Monad.Trans            ( MonadIO )
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , shiftL
                                                , shiftR
                                                , testBit
                                                )
import qualified Data.ByteString               as BS
import           Data.IORef
import           Data.Set                      as Set
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable.Mutable  as VUM
import           Data.Word
import qualified Emulator.Mapper               as Mapper
import           Emulator.Rom                  as Cartridge
import           Emulator.Util
import           Prelude                 hiding ( read
                                                , replicate
                                                )

newtype Emulator a = Emulator { unNes :: ReaderT Nes IO a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadReader Nes)

data Nes = Nes
  { cpu    :: CPU
  , cart   :: Cartridge
  , mapper :: Mapper.Mapper
  }

data CPU = CPU
  { pc        :: IORef Word16
  , sp        :: IORef Word8
  , a         :: IORef Word8
  , x         :: IORef Word8
  , y         :: IORef Word8
  , p         :: IORef Word8
  , cpuCycles :: IORef Int
  , ram       :: VUM.IOVector Word8
  }


{-# INLINE with #-}
with :: (Nes -> b) -> (b -> IO a) -> Emulator a
with field f = do
  nes <- ask
  liftIO $ f (field nes)

{-# INLINE loadCpu #-}
loadCpu :: (CPU -> IORef b) -> Emulator b
loadCpu field = with (field . cpu) readIORef

{-# INLINE storeCpu #-}
storeCpu :: (CPU -> IORef b) -> b -> Emulator ()
storeCpu field v = with (field . cpu) (`modifyIORef'` const v)

{-# INLINE modifyCpu #-}
modifyCpu :: (CPU -> IORef b) -> (b -> b) -> Emulator ()
modifyCpu field v = with (field . cpu) (`modifyIORef'` v)


runEmulator :: BS.ByteString -> Emulator a -> IO a
runEmulator bs (Emulator reader) = do
  cart <- Cartridge.parse bs
  nes  <- new cart
  runReaderT reader nes

debug :: String -> Emulator ()
debug = liftIO . putStrLn

readCpuMemory8 :: Word16 -> Emulator Word8
readCpuMemory8 addr
  | addr < 0x2000  = readCPURam addr
  | addr < 0x4000  = pure 0
  | addr == 0x4014 = pure 0
  | addr == 0x4015 = pure 0
  | addr == 0x4016 = pure 0
  | addr == 0x4017 = pure 0
  | addr < 0x6000  = pure 0
  | addr >= 0x6000 = readMapper addr
  | otherwise      = error $ "Erroneous read detected at " ++ show addr ++ "!"

readCpuMemory16 :: Word16 -> Emulator Word16
readCpuMemory16 addr = do
  lo <- readCpuMemory8 addr
  hi <- readCpuMemory8 (addr + 1)
  pure $ makeW16 lo hi

writeCpuMemory8 :: Word16 -> Word8 -> Emulator ()
writeCpuMemory8 addr value
  | addr < 0x2000                    = writeCPURam addr value
  | addr < 0x4000                    = pure ()
  | addr == 0x4014                   = pure ()
  | addr == 0x4016                   = pure ()
  | addr >= 0x4000 && addr <= 0x4017 = pure ()
  | addr >= 0x4018 && addr <= 0x401F = pure ()
  | addr >= 0x6000                   = writeMapper addr value
  | otherwise = error $ "Erroneous write detected at " ++ show addr ++ "!"

writeCpuMemory16 :: Word16 -> Word16 -> Emulator ()
writeCpuMemory16 addr value = do
  let (lo, hi) = splitW16 value
  writeCpuMemory8 addr       lo
  writeCpuMemory8 (addr + 1) hi

new :: Cartridge -> IO Nes
new cart = do
  cpu    <- newCPU
  mapper <- Mapper.new cart
  pure $ Nes cpu cart mapper

newCPU :: IO CPU
newCPU = do
  pc     <- newIORef 0x0
  sp     <- newIORef 0xFD
  a      <- newIORef 0x0
  x      <- newIORef 0x0
  y      <- newIORef 0x0
  p      <- newIORef 0x24 -- should this be 0x34?
  cycles <- newIORef 0
  ram    <- VUM.replicate 65536 0x0
  pure $ CPU pc sp a x y p cycles ram

readCPURam :: Word16 -> Emulator Word8
readCPURam addr =
  with cpu $ \cpu -> VUM.read (ram cpu) (fromIntegral addr `rem` 0x0800)

writeCPURam :: Word16 -> Word8 -> Emulator ()
writeCPURam addr v =
  with cpu $ \cpu -> VUM.write (ram cpu) (fromIntegral addr `rem` 0x0800) v

readMapper :: Word16 -> Emulator Word8
readMapper addr = with mapper $ \mapper -> Mapper.read mapper addr

writeMapper :: Word16 -> Word8 -> Emulator ()
writeMapper addr value =
  with mapper $ \mapper -> Mapper.write mapper addr value

