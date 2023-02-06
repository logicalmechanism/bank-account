{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module BankAccountContract
  ( bankAccountContractScript
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley                             ( PlutusScript (..)
                                                                 , PlutusScriptV2 )
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.ByteString.Short                           as SBS
import qualified Plutus.V1.Ledger.Scripts                        as Scripts
import qualified Plutus.V2.Ledger.Contexts                       as V2
import qualified Plutus.V2.Ledger.Api                            as V2
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as Utils
import           BankAccountDatum
import           BankAccountRedeemer
import           ReducedFunctions
import           UsefulFuncs
-- import           Plutonomy
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType 
  = Withdraw LovelaceData
  | Deposit LovelaceData
  | Close
  | Update AccountInfo
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Withdraw, 0 )
                                                , ( 'Deposit,  1 )
                                                , ( 'Close,    2 )
                                                , ( 'Update,   3 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: BankAccountDatum -> CustomRedeemerType -> V2.ScriptContext -> Bool
mkValidator datum redeemer context =
  {- The Bank Account Contract

    This contract allows for micropayments to a user's bank account. A user may
    withdraw funds from their allow when the account is above their set threshold.
    The account must contain at least the minimum balance for a withdrawal. A user
    may always close their account, extracting all funds to the owner's address.
  -}
  case (datum, redeemer) of
    -- | Update the bank account
    (Bank oi _, Update ai) ->
      let !walletPkh  = oPkh oi
          !info       = V2.scriptContextTxInfo context
          !txSigners  = V2.txInfoSignatories info
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !thisValue  = V2.txOutValue validInput
          !scriptAddr = V2.txOutAddress validInput
          !contOuts   = getScriptOutputs txOuts scriptAddr
      in traceIfFalse "sig" (signedBy txSigners walletPkh)                    -- seller must sign it
      && traceIfFalse "own" (checkDatumByVal contOuts thisValue (Bank oi ai)) -- seller and info can't change
      && traceIfFalse "ins" (nInputs txIns scriptAddr 1)                      -- single script input
      && traceIfFalse "out" (nOutputs contOuts 1)                             -- single script output
      && traceIfFalse "val" (validAccountInfo ai)                             -- correct minimum and threshold

    -- | Close the bank account
    (Bank oi _, Close) ->
      let !walletPkh  = oPkh oi
          !walletAddr = createAddress walletPkh (oSc oi)
          !info       = V2.scriptContextTxInfo context
          !txSigners  = V2.txInfoSignatories info
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !thisValue  = V2.txOutValue validInput
          !scriptAddr = V2.txOutAddress validInput
      in traceIfFalse "sig" (signedBy txSigners walletPkh)            -- wallet must sign
      && traceIfFalse "pay" (findPayout txOuts walletAddr thisValue)  -- must pay to wallet
      && traceIfFalse "ins" (nInputs txIns scriptAddr 1)              -- singular script input

    -- | Withdraw from the bank account
    (Bank oi ai, Withdraw ld) ->
      let !walletPkh  = oPkh oi 
          !info       = V2.scriptContextTxInfo context
          !txSigners  = V2.txInfoSignatories info
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !thisValue  = V2.txOutValue validInput
          !scriptAddr = V2.txOutAddress validInput
          !contOuts   = getScriptOutputs txOuts scriptAddr
          !newValue   = thisValue - adaValue (increment ld)
      in traceIfFalse "sig" (signedBy txSigners walletPkh)                   -- seller must sign it
      && traceIfFalse "own" (checkDatumByVal contOuts newValue (Bank oi ai)) -- seller and info can't change
      && traceIfFalse "ins" (nInputs txIns scriptAddr 1)                     -- single script input
      && traceIfFalse "out" (nOutputs contOuts 1)                            -- single script output
      && traceIfFalse "wit" (checkMinBal thisValue ai (increment ld))        -- minimum and threshold check
      && traceIfFalse "low" (increment ld > 0)                               -- must withdraw something

    -- | Deposit from the bank account
    (Bank oi ai, Deposit ld) ->
      let !info       = V2.scriptContextTxInfo context
          !txIns      = V2.txInfoInputs info
          !txOuts     = V2.txInfoOutputs info
          !validInput = ownInput context
          !thisValue  = V2.txOutValue validInput
          !scriptAddr = V2.txOutAddress validInput
          !contOuts   = getScriptOutputs txOuts scriptAddr
          !newValue   = thisValue + adaValue (increment ld)
      in traceIfFalse "own" (checkDatumByVal contOuts newValue (Bank oi ai)) -- seller and info can't change
      && traceIfFalse "ins" (nInputs txIns scriptAddr 1)                     -- single script input
      && traceIfFalse "out" (nOutputs contOuts 1)                            -- single script outputd
      && traceIfFalse "low" (increment ld > 0)                               -- must deposit something
  where
    checkDatumByVal :: [V2.TxOut] -> V2.Value -> BankAccountDatum -> Bool
    checkDatumByVal txOuts val (Bank oi ai) = checkDatumByVal' txOuts
      where
        checkDatumByVal' :: [V2.TxOut] -> Bool
        checkDatumByVal' []     = traceError "Nothing Found"
        checkDatumByVal' (x:xs) =
          if V2.txOutValue x == val -- strict value continue
            then
              case V2.txOutDatum x of
                V2.NoOutputDatum       -> checkDatumByVal' xs -- skip datumless
                (V2.OutputDatumHash _) -> traceError "Embedded Datum"
                (V2.OutputDatum (V2.Datum d)) -> 
                  case PlutusTx.fromBuiltinData d of
                    Nothing     -> traceError "Bad Data"
                    Just inline ->
                      case PlutusTx.unsafeFromBuiltinData @BankAccountDatum inline of
                        (Bank oi' ai') -> 
                          if ((oi == oi') == True) && ((ai == ai') == True)
                            then True
                            else checkDatumByVal' xs
            else checkDatumByVal' xs
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: V2.Validator
validator' = V2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

bankAccountContractScriptShortBs :: SBS.ShortByteString
bankAccountContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

bankAccountContractScript :: PlutusScript PlutusScriptV2
bankAccountContractScript = PlutusScriptSerialised bankAccountContractScriptShortBs
