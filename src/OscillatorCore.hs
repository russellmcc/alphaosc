module OscillatorCore where
import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import qualified NCO as NCO
import qualified Sync as Sync
import qualified Shaper as Shaper

data OscConfig = OscConfig { inc :: Unsigned 16
                           , divider :: Unsigned 16
                           , modes :: Unsigned 8
                           , pw :: Unsigned 8
                           , sync :: Bit
                           }

mkOscConfig inc' div' modes' pw' sync' =
  OscConfig { inc = inc'
            , divider = div'
            , modes = modes'
            , pw = pw'
            , sync = sync'
            }

data OscOutput = OscOutput { saw :: Unsigned 8
                           , square :: Unsigned 8
                           , sub ::Unsigned 8
                           }

mkOscOutput saw' sqr' sub' = OscOutput { saw = saw'
                                       , square = sqr'
                                       , sub = sub'
                                       }

makeNCO :: SClock clk ->
           Signal' clk (Unsigned 16) ->
           Signal' clk (Unsigned 16) ->
           Signal' clk Bit ->
           Signal' clk (Unsigned 24)
makeNCO clk' inc' div' reset'  =
  NCO.nco clk' $ (,) <$>
       ((unpack . resize . pack) <$> (inc')) <*>
        ((,) <$> div' <*> reset')

oscillatorCore :: SClock clk ->
                  Signal' clk OscConfig ->
                  Signal' clk OscOutput
oscillatorCore clk' cfg = mkOscOutput <$> saw' <*> sqr' <*> sub' where
  modes' = modes <$> cfg
  lfo' = pw <$> cfg
  div' = divider <$> cfg
  inc' = inc <$> cfg
  reset' = Sync.sync2reset (100 :: Unsigned 7) (20 :: Unsigned 5) clk' $ sync <$> cfg
  nco = makeNCO clk' inc' div' reset'
  nco' = (unpack . slice d23 d8) <$> nco
  saw' = (Shaper.saw <$> ((unpack . slice d2 d0) <$> modes')) <*> nco' <*> lfo'
  sqr' = (Shaper.square <$> ((unpack . slice d4 d3) <$> modes')) <*> nco' <*> lfo'
  sub' = (Shaper.sub <$> ((unpack . slice d7 d5) <$> modes')) <*> nco'
