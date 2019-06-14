module AddrReg where
import CLaSH.Prelude
import CLaSH.Prelude.Explicit

mUpdate :: (KnownNat a, KnownNat d) =>
           Unsigned a ->
           (Unsigned d, Bit)  ->
           (Unsigned a, (Unsigned d, Bit)) ->
           ((Unsigned d, Bit), Unsigned d)
mUpdate a (s, lastInput) input = ((s', snd $ snd input), output) where
  s' = if (fst input == a) &&
          ((snd . snd) input == high) &&
          (lastInput == low) then
         (fst . snd) input
       else s
  output = s

addrReg :: (KnownNat a, KnownNat d) =>
           SClock c ->
           Unsigned a ->
           Signal' c (Unsigned a, (Unsigned d, Bit)) ->
           Signal' c (Unsigned d)
addrReg clk a = mealy' clk (mUpdate a) (0, low)

