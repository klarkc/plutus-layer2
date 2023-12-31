{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Main (main) where

import Prelude (IO)
import PlutusTx.Prelude (($), Maybe (Nothing))
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Cardano.Api.SerialiseTextEnvelope (textEnvelopeToJSON)
import qualified Codec.Serialise as CS
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Short as DBS
import Plutus.V2.Ledger.Api (Validator)
import Validator (validator)

serialise :: Validator -> PlutusScript PlutusScriptV2
serialise val = PlutusScriptSerialised $ DBS.toShort $ DBL.toStrict $ CS.serialise $ val

main :: IO ()
main = DBL.putStr $ textEnvelopeToJSON Nothing $ serialise validator
