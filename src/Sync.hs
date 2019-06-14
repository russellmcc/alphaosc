module Sync where
import CLaSH.Prelude
import CLaSH.Prelude.Explicit

-- Logic that converts a "sync" input into a signal to reset the oscillator.
-- We first detect high to low transitions, with debounce high inputs needed
 -- in a row before a low will be triggered.
sync2reset :: forall c n m.
              (KnownNat n, KnownNat m) =>
              Unsigned n ->
              Unsigned m ->
              SClock c ->
              Signal' c Bit ->
              Signal' c Bit
sync2reset debounceH debounceL clk = mealy' clk syncT (debounceH, debounceL) where
  syncT (highs, lows) sync = ((highs', lows'), reset)
    where
      reset = if (highs == 0 && lows == 0 && sync == low) then high else low
      (highs', lows') | (sync == high && highs > 0) = (highs - 1, debounceL)
                      | (sync == high && highs == 0) = (0, debounceL)
                      | (sync == low && highs == 0 && lows > 0) = (0, lows - 1)
                      | otherwise = (debounceH, debounceL)