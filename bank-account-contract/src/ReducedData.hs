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
module ReducedData
  ( BankOutputDatum (..)
  , BankTxOut (..)
  , BankTxInInfo (..)
  , BankTxInfo (..)
  , BankScriptPurpose (..)
  , BankScriptContext (..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Contexts as V2
import qualified Plutus.V2.Ledger.Api      as V2
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Reduced Script Context
-------------------------------------------------------------------------------
data BankOutputDatum =  NoOutputDatum | OutputDatum V2.Datum
PlutusTx.makeIsDataIndexed ''BankOutputDatum [('NoOutputDatum, 0), ('OutputDatum, 2)]

data BankTxOut = BankTxOut
  { txOutAddress         :: V2.Address
  , txOutValue           :: V2.Value
  , txOutDatum           :: BankOutputDatum
  , txOutReferenceScript :: BuiltinData
  }
PlutusTx.unstableMakeIsData ''BankTxOut

data BankTxInInfo = BankTxInInfo
    { txInInfoOutRef   :: V2.TxOutRef
    , txInInfoResolved :: BankTxOut
    } 
PlutusTx.unstableMakeIsData ''BankTxInInfo

data BankTxInfo = BankTxInfo
    { txInfoInputs          :: [BankTxInInfo] -- Transaction inputs
    , txInfoReferenceInputs :: BuiltinData
    , txInfoOutputs         :: [BankTxOut]    -- Transaction outputs
    , txInfoFee             :: BuiltinData
    , txInfoMint            :: BuiltinData
    , txInfoDCert           :: BuiltinData
    , txInfoWdrl            :: BuiltinData
    , txInfoValidRange      :: BuiltinData
    , txInfoSignatories     :: [V2.PubKeyHash] -- Transaction signers
    , txInfoRedeemers       :: BuiltinData
    , txInfoData            :: BuiltinData
    , txInfoId              :: BuiltinData
    }
PlutusTx.unstableMakeIsData ''BankTxInfo

data BankScriptPurpose = Spending V2.TxOutRef
PlutusTx.makeIsDataIndexed ''BankScriptPurpose [('Spending, 1)]

data BankScriptContext = BankScriptContext
  { scriptContextTxInfo  :: BankTxInfo
  , scriptContextPurpose ::  BankScriptPurpose 
  }
PlutusTx.unstableMakeIsData ''BankScriptContext