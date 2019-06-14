{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Test.Framework
import Prelude as P
import CLaSH.Prelude as C
import CLaSH.Prelude.Explicit
import AddrReg
import NCO
import Control.DeepSeq


makeTest :: (KnownNat n, Eq b, Show b, NFData b) =>
            Vec n a ->
            [b] ->
            (Signal a -> Signal b) ->
            IO ()
makeTest i o f = assertEqual o output where
  output = sampleN (1 + C.length i) $ (f testInput)
  testInput = stimuliGenerator i

test_AddrReg = makeTest i o (addrReg systemClock 3) where
  i = $(listToVecTH [ (3 :: Unsigned 8, (23 :: Unsigned 8, 0 :: Bit))
                    , (3, (47, 0))
                    , (3, (47, 1))
                    , (3, (48, 1))
                    , (3, (49, 0))
                    , (4, (51, 1))
                    , (3, (49, 0))
                    ])
  o = [ (0 :: Unsigned 8)
      , 0
      , 0
      , 47
      , 47 -- should be latched at 47
      , 47
      , 47 -- wrong address!
      , 47
      ]

test_nco = makeTest i o (nco systemClock) where
  i = $(listToVecTH [ (1 :: Unsigned 8, (0 :: Unsigned 8, low))
                    , (2, (1, low))
                    , (3, (1, low))
                    , (4, (1, low))
                    , (5, (0, low))
                    , (6, (0, low))
                    , (7, (1, low))
                    ])
  o = [ (0 :: Unsigned 8)
      , 1
      , 3
      , 6
      , 6
      , 11
      , 11
      , 18
      ]

main = htfMain htf_thisModulesTests