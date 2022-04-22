module Emulator.PPU
  ( reset,
    step,
  )
where

import Control.Monad
-- import           Control.Monad.IO.Class (liftIO)
import Data.Bits
  ( shiftL,
    shiftR,
    xor,
    (.&.),
    (.|.),
  )
import qualified Data.Vector as V
import Data.Word
import Emulator.Mem
import Emulator.Nes
import Emulator.Util.Util
import Prelude hiding (cycle)

reset :: Emulator ()
reset = do
  storePpu ppuCycles 340
  storePpu scanline 240
  writeControl 0
  writeMask 0
  writeOAMAddress 0

step :: Emulator ()
step = do
  (scanline, cycle) <- tick
  handleLinePhase scanline cycle

tick :: Emulator Coords
tick = do
  handleIrq

  rendering <- renderingEnabled
  scanline' <- loadPpu scanline
  cycles <- loadPpu ppuCycles
  oddFrame' <- loadPpu oddFrame

  if rendering && oddFrame' && scanline' == 261 && cycles == 339
    then do
      storePpu ppuCycles 0
      storePpu scanline 0
      modifyPpu frameCount (+ 1)
      modifyPpu oddFrame not
    else do
      modifyPpu ppuCycles (+ 1)
      when (cycles + 1 > 340) $ do
        storePpu ppuCycles 0
        modifyPpu scanline (+ 1)
        when (scanline' + 1 > 261) $ do
          storePpu scanline 0
          modifyPpu frameCount (+ 1)
          modifyPpu oddFrame not

  sc <- loadPpu scanline
  cy <- loadPpu ppuCycles

  pure (sc, cy)

handleIrq :: Emulator ()
handleIrq = do
  delay <- loadPpu nmiDelay
  when (delay > 0) $ do
    modifyPpu nmiDelay (subtract 1)
    output <- loadPpu nmiOutput
    occurred <- loadPpu nmiOccurred
    when (delay - 1 == 0 && output && occurred) $ storeReg interrupt (Just NMI)

handleLinePhase :: Int -> Int -> Emulator ()
handleLinePhase scanline cycle = do
  rendering <- renderingEnabled
  let preLine = scanline == 261
  let visibleLine = scanline < 240
  let renderLine = preLine || visibleLine

  let preFetchCycle = cycle >= 321 && cycle <= 336
  let visibleCycle = cycle >= 1 && cycle <= 256
  let fetchCycle = preFetchCycle || visibleCycle

  when rendering $ do
    when (visibleLine && visibleCycle) $ renderPixel scanline cycle

    when (renderLine && fetchCycle) $ fetch scanline cycle

    when (preLine && cycle >= 280 && cycle <= 304) copyY

    when (preLine || visibleLine) $ do
      when ((preFetchCycle || visibleCycle) && cycle `rem` 8 == 0) incrementX

      when (cycle == 256) incrementY

      when (cycle == 257) copyX

    when (cycle == 257) $ when visibleLine $ evaluateSprites scanline

  when (scanline == 241 && cycle == 1) enterVBlank

  when (preLine && cycle == 1) $ do
    exitVBlank
    storePpu spriteZeroHit False
    storePpu spriteOverflow False

renderPixel :: Int -> Int -> Emulator ()
renderPixel scanline cycle = do
  let coords = (cycle - 1, scanline)
  bgColor <- getBackgroundPixel coords
  spriteColor <- getSpritePixel coords
  finalColor <- getComposedColor coords bgColor spriteColor
  writeScreen coords finalColor

getBackgroundPixel :: Coords -> Emulator Word8
getBackgroundPixel coords = do
  tileData <- fetchTileData
  fineX <- loadPpu fineX
  let scrolled = tileData `shiftR` fromIntegral ((7 - fineX) * 4)
  pure $ fromIntegral (scrolled .&. 0x0F)

getSpritePixel :: Coords -> Emulator (Maybe (Sprite, Word8))
getSpritePixel coords = do
  sprites' <- loadPpu sprites
  let colors = V.map getColor sprites'
  pure $ msum colors
  where
    getColor :: Sprite -> Maybe (Sprite, Word8)
    getColor sprite@(Sprite _ (x, y) _ _ sPattern _) = do
      let offset = fst coords - x
      if offset >= 0 && offset <= 7
        then do
          let nextOffset = fromIntegral ((7 - offset) * 4) :: Word8
          let shifted =
                fromIntegral (sPattern `shiftR` fromIntegral nextOffset) :: Word8
          let color = shifted .&. 0x0F
          if color `rem` 4 /= 0 then Just (sprite, color) else Nothing
        else Nothing

getComposedColor :: Coords -> Word8 -> Maybe (Sprite, Word8) -> Emulator Color
getComposedColor (x, y) bg sprite = do
  color <- getColor
  index <- readPpuMemory $ 0x3F00 + fromIntegral color
  pure $ getPaletteColor (index `rem` 64)
  where
    b = bg `rem` 4 /= 0
    (sc, ind, priority) = case sprite of
      Just (s, c) -> (c, sIndex s, sPriority s)
      Nothing -> (0, 0, 0)
    s = sc `rem` 4 /= 0
    getColor
      | not b && not s = pure 0
      | not b && s = pure $ sc .|. 0x10
      | b && not s = pure bg
      | otherwise = do
          when (ind == 0 && x < 255) $ storePpu spriteZeroHit True
          if priority == 0 then pure $ sc .|. 0x10 else pure bg

fetch :: Int -> Int -> Emulator ()
fetch scanline cycle = do
  modifyPpu tileData (`shiftL` 4)
  case cycle `rem` 8 of
    1 -> fetchNameTableValue
    3 -> fetchAttributeTableValue
    5 -> fetchLowTileValue
    7 -> fetchHighTileValue
    0 -> storeTileData
    _ -> idle

fetchNameTableValue :: Emulator ()
fetchNameTableValue = do
  v <- loadPpu currentVramAddress
  ntv <- readPpuMemory $ 0x2000 .|. (v .&. 0x0FFF)
  storePpu nameTableByte ntv

fetchAttributeTableValue :: Emulator ()
fetchAttributeTableValue = do
  v <- loadPpu currentVramAddress
  v' <-
    readPpuMemory $
      0x23C0
        .|. (v .&. 0x0C00)
        .|. ((v `shiftR` 4) .&. 0x38)
        .|. ((v `shiftR` 2) .&. 0x07)
  let shift = fromIntegral $ ((v `shiftR` 4) .&. 4) .|. (v .&. 2)
  let atv = ((v' `shiftR` shift) .&. 3) `shiftL` 2
  storePpu attrTableByte atv

fetchLowTileValue :: Emulator ()
fetchLowTileValue = do
  v <- loadPpu currentVramAddress
  let fineY = (v `shiftR` 12) .&. 7
  bt <- loadPpu bgTable
  ntv <- loadPpu nameTableByte
  ltv <- readPpuMemory $ bt + fromIntegral ntv * 16 + fineY
  storePpu loTileByte ltv

fetchHighTileValue :: Emulator ()
fetchHighTileValue = do
  ntv <- loadPpu nameTableByte
  v <- loadPpu currentVramAddress
  bt <- loadPpu bgTable
  let fineY = (v `shiftR` 12) .&. 7
  htv <- readPpuMemory $ bt + fromIntegral ntv * 16 + fineY + 8
  storePpu hiTileByte htv

fetchTileData :: Emulator Word32
fetchTileData = do
  td <- loadPpu tileData
  pure $ fromIntegral $ td `shiftR` 32

storeTileData :: Emulator ()
storeTileData = do
  lotv <- loadPpu loTileByte
  hitv <- loadPpu hiTileByte
  atv <- loadPpu attrTableByte

  let td = do
        i <- V.fromList [0 .. 7]
        let p1 = ((lotv `shiftL` i) .&. 0x80) `shiftR` 7
        let p2 = ((hitv `shiftL` i) .&. 0x80) `shiftR` 6
        pure $ fromIntegral $ atv .|. p1 .|. p2 :: V.Vector Word32

  let td' = V.foldl' op 0 td where op acc i = (acc `shiftL` 4) .|. i

  modifyPpu tileData (\x -> x .|. fromIntegral td')

copyY :: Emulator ()
copyY = do
  tv <- loadPpu tempVramAddress
  cv <- loadPpu currentVramAddress
  storePpu currentVramAddress ((cv .&. 0x841F) .|. (tv .&. 0x7BE0))

copyX :: Emulator ()
copyX = do
  tv <- loadPpu tempVramAddress
  cv <- loadPpu currentVramAddress
  storePpu currentVramAddress ((cv .&. 0xFBE0) .|. (tv .&. 0x041F))

incrementX :: Emulator ()
incrementX = do
  v <- loadPpu currentVramAddress
  if v .&. 0x001F == 31
    then do
      modifyPpu currentVramAddress (.&. 0xFFE0)
      modifyPpu currentVramAddress (`xor` 0x0400)
    else modifyPpu currentVramAddress (+ 1)

incrementY :: Emulator ()
incrementY = do
  v <- loadPpu currentVramAddress
  if v .&. 0x7000 /= 0x7000
    then modifyPpu currentVramAddress (+ 0x1000)
    else do
      modifyPpu currentVramAddress (.&. 0x8FFF)
      let y = (v .&. 0x03E0) `shiftR` 5

      y' <-
        if y == 29
          then do
            modifyPpu currentVramAddress (`xor` 0x0800)
            pure 0
          else if y == 31 then pure 0 else pure $ y + 1

      v' <- loadPpu currentVramAddress
      storePpu currentVramAddress ((v' .&. 0xFC1F) .|. (y' `shiftL` 5))

evaluateSprites :: Int -> Emulator ()
evaluateSprites scanline = do
  spriteSize <- loadPpu spriteSize
  allSprites <-
    traverse
      (getSpriteAt scanline spriteSize)
      (V.fromList [0 .. 63])
  let visibleSprites = V.take 8 (catMaybesV allSprites)
  storePpu sprites visibleSprites

getSpriteAt :: Int -> SpriteSize -> Int -> Emulator (Maybe Sprite)
getSpriteAt scanline size i = do
  let baseOffset = fromIntegral $ i * 4
  y <- readOAMData $ baseOffset + 0
  let row = scanline - fromIntegral y

  if isSpriteVisible row size
    then do
      tileIndexByte <- readOAMData $ baseOffset + 1
      attrByte <- readOAMData $ baseOffset + 2
      x <- readOAMData $ baseOffset + 3
      addr <- getSpriteAddress row size attrByte tileIndexByte
      loTileByte <- readPpuMemory addr
      hiTileByte <- readPpuMemory $ addr + 8
      let spritePattern = decodeSpritePattern attrByte loTileByte hiTileByte
      let priority = (attrByte `shiftR` 5) .&. 1
      pure $
        Just $
          Sprite
            i
            (fromIntegral x, fromIntegral y)
            tileIndexByte
            attrByte
            spritePattern
            priority
    else pure Nothing

decodeSpritePattern :: Word8 -> Word8 -> Word8 -> Word32
decodeSpritePattern attr lo hi = tileData'
  where
    atv = (attr .&. 3) `shiftL` 2
    tileData = do
      i <- V.fromList [0 .. 7]
      let (p1, p2) =
            if attr .&. 0x40 == 0x40
              then do
                let p1 = ((lo `shiftR` i) .&. 0x1) `shiftL` 0
                let p2 = ((hi `shiftR` i) .&. 0x1) `shiftL` 1
                (p1, p2)
              else do
                let p1 = ((lo `shiftL` i) .&. 0x80) `shiftR` 7
                let p2 = ((hi `shiftL` i) .&. 0x80) `shiftR` 6
                (p1, p2)

      pure $ fromIntegral $ atv .|. p1 .|. p2 :: V.Vector Word32
    tileData' = V.foldl' op 0 tileData where op acc i = (acc `shiftL` 4) .|. i

getSpriteAddress :: Int -> SpriteSize -> Word8 -> Word8 -> Emulator Word16
getSpriteAddress row size attr tile = case size of
  Normal -> do
    let row' = if attr .&. 0x80 == 0x80 then 7 - row else row
    table <- loadPpu spriteTable
    pure $ table + fromIntegral tile * 16 + fromIntegral row'
  Double -> do
    let row' = if attr .&. 0x80 == 0x80 then 15 - row else row
    let table = tile .&. 1
    let tile' = tile .&. 0xFE
    let (tile'', row'') =
          if row' > 7 then (tile' + 1, row' - 8) else (tile', row')
    pure $
      (0x1000 * fromIntegral table)
        + fromIntegral tile''
        * 16
        + fromIntegral row''

isSpriteVisible :: Int -> SpriteSize -> Bool
isSpriteVisible row spriteSize = row >= 0 && row < h
  where
    h = case spriteSize of
      Normal -> 8
      Double -> 16

enterVBlank :: Emulator ()
enterVBlank = toggleNmi True

exitVBlank :: Emulator ()
exitVBlank = toggleNmi False

renderingEnabled :: Emulator Bool
renderingEnabled = do
  bg <- loadPpu bgVisibility
  sprites <- loadPpu spriteVisibility
  pure $ bg || sprites

idle :: Emulator ()
idle = pure ()


































getPaletteColor :: Word8 -> Color
getPaletteColor index = palette V.! fromIntegral index
  where
    palette =
      V.fromList
        [ (101, 101, 101),
          (0, 45, 105),
          (19, 31, 127),
          (60, 19, 124),
          (96, 11, 98),
          (115, 10, 55),
          (113, 15, 7),
          (90, 26, 0),
          (52, 40, 0),
          (11, 52, 0),
          (0, 60, 0),
          (0, 61, 16),
          (0, 56, 64),
          (0, 0, 0),
          (0, 0, 0),
          (0, 0, 0),
          (174, 174, 174),
          (15, 99, 179),
          (64, 81, 208),
          (120, 65, 204),
          (167, 54, 169),
          (192, 52, 112),
          (189, 60, 48),
          (159, 74, 0),
          (109, 92, 0),
          (54, 109, 0),
          (7, 119, 4),
          (0, 121, 61),
          (0, 114, 125),
          (0, 0, 0),
          (0, 0, 0),
          (0, 0, 0),
          (254, 254, 255),
          (93, 179, 255),
          (143, 161, 255),
          (200, 144, 255),
          (247, 133, 250),
          (255, 131, 192),
          (255, 139, 127),
          (239, 154, 73),
          (189, 172, 44),
          (133, 188, 47),
          (85, 199, 83),
          (60, 201, 140),
          (62, 194, 205),
          (78, 78, 78),
          (0, 0, 0),
          (0, 0, 0),
          (254, 254, 255),
          (188, 223, 255),
          (209, 216, 255),
          (232, 209, 255),
          (251, 205, 253),
          (255, 204, 229),
          (255, 207, 202),
          (248, 213, 180),
          (228, 220, 168),
          (204, 227, 169),
          (185, 232, 184),
          (174, 232, 208),
          (175, 229, 234),
          (182, 182, 182),
          (0, 0, 0),
          (0, 0, 0)]
