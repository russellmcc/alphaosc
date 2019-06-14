module MainMimas where
import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import NCO as NCO
import Shaper as Shaper

type MClk = Clk "CLK_100MHz" 1000
mclk :: SClock MClk
mclk = sclock

pwm :: Signal' MClk (Unsigned 8) -> Signal' MClk Bit
pwm = mealy' mclk pwmT (0 :: Unsigned 8) where
  pwmT acc cmp = (acc + 1, if acc > cmp then high else low)

{-# ANN topEntity
  (defTop
    { t_name     = "topLevel"
    , t_inputs   = ["DPSwitch"]
    , t_outputs  = ["Audio1", "Audio2"]
    }) #-}
topEntity :: Signal' MClk (Unsigned 8) -> Signal' MClk (Bit, Bit)
topEntity modes = (,) <$> pwm sqr <*> pwm saw where
  nco = NCO.nco mclk (pure (1 :: Unsigned 16,
                            27 :: Unsigned 16))
  lfo = (unpack . (slice d15 d8)) <$> NCO.nco mclk (pure (1 :: Unsigned 16,
                                                          1525 :: Unsigned 16))
  saw = (Shaper.saw <$> ((unpack . slice d2 d0) <$> modes)) <*> nco <*> lfo
--  sqr = (Shaper.square <$> ((unpack . slice d5 d3) <$> modes)) <*> nco <*> lfo
  sqr = (Shaper.sub <$> ((unpack . slice d5 d3) <$> modes)) <*> nco
