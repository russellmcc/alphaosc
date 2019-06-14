module NCO where
import CLaSH.Prelude
import CLaSH.Prelude.Explicit

-- A Numerically Controller Oscillator, or NCO, is basically
-- a counter with two differences - one is that at each clock,
-- the counter is incremented by a programmable amount (here x),
-- and additionally, the input clock is divided before sent to
-- the accumulator (here the clock is divided by div + 1).
nco :: forall c n m. (KnownNat n, KnownNat m) =>
       SClock c ->
       Signal' c (Unsigned n, (Unsigned m, Bit)) ->
       Signal' c (Unsigned n)
nco clk = mealy' clk ncoT (0, 0, 0) where
  ncoT (acc,dacc,div) (x, (ndiv, reset)) = ((acc', dacc', div'), acc)
    where
      dacc' = if (dacc >= div || reset == high) then 0 else dacc + 1
      div' = if (dacc == 0 || reset == high) then ndiv else div
      acc' | (reset == high) = 0
           | (dacc == 0) = acc + x
           | otherwise = acc


