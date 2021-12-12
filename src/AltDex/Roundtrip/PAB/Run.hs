module AltDex.Roundtrip.PAB.Run (
  AltDexDemoContracts,
  runRoundtripDemo
) where

import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Run (runWith)
import Prelude

import AltDex.Roundtrip.PAB.Contracts (AltSwapDemoContracts)

runRoundtripDemo :: IO ()
runRoundtripDemo = do
  runWith (Builtin.handleBuiltin @AltSwapDemoContracts)