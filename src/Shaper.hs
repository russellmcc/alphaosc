module Shaper where
import CLaSH.Prelude

boolBits :: (BitPack a, KnownNat (BitSize a)) => Bool -> a
boolBits True = unpack $ complement zeroBits
boolBits False = unpack zeroBits

doubleFreq :: Unsigned 16 -> Unsigned 7
doubleFreq t = unpack $ slice d12 d6 t

fullFreq :: Unsigned 16 -> Unsigned 8
fullFreq t = unpack $ slice d13 d6 t

square :: Unsigned 2 -> Unsigned 16 -> Unsigned 8 -> Unsigned 8
square mode wave pw = case mode of
  1 -> boolBits $ testBit wave 13
  2 -> boolBits $ (testBit wave 13) && (testBit wave 12)
  3 -> boolBits $ (fullFreq wave) > pw
  _ -> unpack zeroBits

saw :: Unsigned 3 -> Unsigned 16 -> Unsigned 8 -> Unsigned 8
saw mode wave pw = case mode of
  1 -> fullFreq wave
  2 -> boolBits (testBit wave 12) .&. (fullFreq wave)
  3 -> boolBits (doubleFreq wave > (unpack $ slice d7 d1 pw)) .&. (fullFreq wave)
  4 -> boolBits (testBit wave 8) .&. (fullFreq wave)
  5 -> boolBits (testBit wave 12) .&. (boolBits (testBit wave 8)) .&. (fullFreq wave)
  _ -> unpack zeroBits

sub :: Unsigned 3 -> Unsigned 16 -> Unsigned 8
sub mode wave = case mode of
  0 -> boolBits $ testBit wave 14
  1 -> boolBits $ (testBit wave 14) && (testBit wave 13)
  2 -> boolBits $ (testBit wave 14) && (testBit wave 12)
  3 -> boolBits $ (testBit wave 14) && (testBit wave 8)
  4 -> boolBits $ (testBit wave 15)
  5 -> boolBits $ (testBit wave 15) && (testBit wave 14)
  _ -> unpack zeroBits