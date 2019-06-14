module MainR2R where
import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import AddrReg (addrReg)
import Oscillator

type MClk = Clk "CLK_100MHz" 1000
mclk :: SClock MClk
mclk = sclock

{-# ANN topEntity
  (defTop
    { t_name     = "topLevel"
    , t_inputs   = ["ADDR_WR", "DAT", "SAW", "SQR", "SUB", "RESET"]
    , t_outputs  = ["DAC_SAW", "DAC_SQR", "DAC_SUB", "LED"]
    }) #-}
topEntity :: Signal' MClk RegData ->
             Signal' MClk RegData ->
             Signal' MClk (BitVector 5) ->
             Signal' MClk (BitVector 3) ->
             Signal' MClk (BitVector 5) ->
             Signal' MClk Bit ->
             Signal' MClk (Unsigned 8, (Unsigned 8, (Unsigned 8, Unsigned 8)))
topEntity addr_wr dat sawM sqrM subM sync' =
  (,) <$>
        (saw <$> osc) <*>
        ((,) <$>
         (square <$> osc) <*>
         ((,) <$>
          (sub <$> osc) <*>
          leds)) where
  addr = unpack <$> (slice d3 d0 <$> addr_wr)
  wr = (!) <$> addr_wr <*> pure 4
  oscData = oscillator mclk addr dat wr sawM sqrM subM sync'
  osc = fst <$> oscData
  cfg = snd <$> oscData
  leds = (unpack . (slice d15 d8)) <$> (inc <$> cfg)
