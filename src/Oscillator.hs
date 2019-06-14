module Oscillator(OscOutput,
                  Addr,
                  RegData,
                  oscillator,
                  square,
                  saw,
                  sub,
                  OscConfig, inc, divider, pw, modes
                 ) where
import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import OscillatorCore
import AddrReg

type Addr = Unsigned 4
type RegData = Unsigned 8

firstHighBit3 :: BitVector 5 -> BitVector 3
firstHighBit3 t = (resize . pack . ((+) 1)) $ countLeadingZeros t

firstHighBit2 :: BitVector 3 -> BitVector 2
firstHighBit2 t = (resize . pack . ((+) 1)) $ countLeadingZeros t

oscillator :: SClock clk ->
              Signal' clk Addr ->
              Signal' clk RegData ->
              Signal' clk Bit ->
              Signal' clk (BitVector 5) ->
              Signal' clk (BitVector 3) ->
              Signal' clk (BitVector 5) ->
              Signal' clk Bit ->
              Signal' clk (OscOutput, OscConfig)
oscillator clk' addr dat wr sawM sqrM subM sync' = (,) <$> out <*> cfg where
  packedIn = (,) <$> addr <*> ((,) <$> dat <*> wr)
  lfo' = addrReg clk' 0 packedIn
  inc1 = addrReg clk' 1 packedIn
  inc2 = addrReg clk' 2 packedIn
  div1 = addrReg clk' 3 packedIn
  div2 = addrReg clk' 4 packedIn
  subM' = firstHighBit3 <$> subM
  sqrM' = firstHighBit2 <$> sqrM
  sawM' = firstHighBit3 <$> sawM
  modes' = unpack <$> ((++#) <$> subM' <*> ((++#) <$> sqrM' <*> sawM'))
  combine a b = unpack <$> ((++#) <$> (pack <$> a) <*> (pack <$> b))
  div' = combine div1 div2
  inc' = combine inc1 inc2
  cfg = mkOscConfig <$> inc' <*> div' <*> modes' <*> lfo' <*> sync'
  out = oscillatorCore clk' cfg
