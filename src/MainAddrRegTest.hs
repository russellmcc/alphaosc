module MainAddrRegTest where
import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import AddrReg (addrReg)
import Oscillator

type MClk = Clk "CLK_100MHz" 1000
mclk :: SClock MClk
mclk = sclock

pwm :: Signal' MClk (Unsigned 8) -> Signal' MClk Bit
pwm = mealy' mclk pwmT (0 :: Unsigned 8) where
  pwmT acc cmp = (acc + 1, if acc > cmp then high else low)

{-# ANN topEntity
  (defTop
    { t_name     = "topLevel"
    , t_inputs   = ["IO_P7", "IO_P6"]
    , t_outputs  = ["Audio1", "Audio2", "LED"]
    }) #-}
topEntity :: Signal' MClk RegData ->
             Signal' MClk RegData ->
             Signal' MClk (Bit, (Bit, Unsigned 8))
topEntity addr_wr dat = (,) <$>
                        pwm (square <$> osc) <*> (
                        (,) <$>
                        pwm (saw <$> osc) <*>
                        leds) where
  addr = unpack <$> (slice d3 d0 <$> addr_wr)
  wr = (!) <$> addr_wr <*> pure 4
  oscData = oscillator mclk addr dat wr
  osc = fst <$> oscData
  cfg = snd <$> oscData
  leds = (unpack . (slice d15 d8)) <$> (inc <$> cfg)
