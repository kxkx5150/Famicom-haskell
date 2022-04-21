
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Emulator.Nes
  ( Nes(..)
  , CPU(..)
  , Emulator(..)
  , newNes
  , initNes
  , with
  , loadReg
  , storeReg
  , modReg
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

initNes :: BS.ByteString -> Emulator a -> IO a
initNes bs (Emulator reader) = do
  cart <- Cartridge.parse bs
  nes  <- newNes cart
  runReaderT reader nes

newNes :: Cartridge -> IO Nes
newNes cart = do
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