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
module ReducedFunctions
  ( signedBy
  , txInFromTxRef
  , findPayout
  , nInputs
  , nOutputs
  , getScriptOutputs
  , ownInput
  , scriptOutAt
  ) where
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api      as V2
import qualified Plutus.V1.Ledger.Value    as Value
import           ReducedData

{-# INLINABLE scriptOutAt #-}
-- | Get the list of 'TxOut' outputs of the pending transaction at
--   a given script address.
scriptOutAt :: V2.ValidatorHash -> [V2.TxOut] -> V2.TxOut
scriptOutAt vHash outs = scriptOutAt' vHash outs
  where
    scriptOutAt' _     [] = traceError "Nothing Found"
    scriptOutAt' vHash (x:xs)
      | (V2.addressCredential $ V2.txOutAddress x) == (V2.ScriptCredential vHash) = x
      | otherwise = scriptOutAt' vHash xs

{-# INLINABLE getScriptOutputs #-}
getScriptOutputs :: [BankTxOut] -> V2.Address -> [BankTxOut]
getScriptOutputs txOuts addr' = getScriptOutputs' txOuts addr' []
  where
    getScriptOutputs' :: [BankTxOut] -> V2.Address -> [BankTxOut] -> [BankTxOut]
    getScriptOutputs' [] _ contOuts = contOuts
    getScriptOutputs' (x:xs) addr contOuts
      | txOutAddress x == addr = getScriptOutputs' xs addr (x:contOuts)
      | otherwise              = getScriptOutputs' xs addr contOuts

-- rewrite findOwnInput without higher order functions
{-# INLINABLE ownInput #-}
ownInput :: BankScriptContext -> BankTxOut
ownInput (BankScriptContext t_info (Spending o_ref)) = getScriptInput (txInfoInputs t_info) o_ref

-- get the validating script input
{-# INLINABLE getScriptInput #-}
getScriptInput :: [BankTxInInfo] -> V2.TxOutRef -> BankTxOut
getScriptInput [] _ = traceError "script input not found"
getScriptInput ((BankTxInInfo tref ot) : xs) o_ref
  | tref == o_ref = ot
  | otherwise = getScriptInput xs o_ref

{-# INLINABLE txInFromTxRef #-}
txInFromTxRef :: [V2.TxInInfo] -> V2.TxOutRef -> V2.TxInInfo
txInFromTxRef txIns outRef = txInFromTxRef' txIns
  where
    txInFromTxRef' :: [V2.TxInInfo] -> V2.TxInInfo
    txInFromTxRef' [] = traceError "Cant Find Tx In"
    txInFromTxRef' (x:xs)
      | V2.txInInfoOutRef x == outRef = x
      | otherwise                     = txInFromTxRef' xs

-- | Check if a transaction was signed by the given public key.
{-# INLINABLE signedBy #-}
signedBy :: [V2.PubKeyHash] -> V2.PubKeyHash -> Bool
signedBy list k = loop list
  where
    loop [] = False
    loop (x:xs)
      | x == k = True
      | otherwise = loop xs

{-# INLINABLE findPayout #-}
findPayout :: [BankTxOut] -> V2.Address -> V2.Value -> Bool
findPayout list addr val = helper list
  where
    helper :: [BankTxOut] -> Bool
    helper [] = False
    helper (x:xs)
      | checkAddr && checkVal = True
      | otherwise             = helper xs
      where
        checkAddr :: Bool
        checkAddr = txOutAddress x == addr

        checkVal :: Bool
        checkVal = Value.geq (txOutValue x) val

-- | Count the number of inputs that have inline datums.
{-# INLINABLE nInputs #-}
nInputs :: [BankTxInInfo] -> V2.Address -> Integer -> Bool
nInputs utxos addr number = loopInputs utxos 0 0
  where
    loopInputs :: [BankTxInInfo] -> Integer -> Integer -> Bool
    loopInputs []     !dC !sC = (dC == number) && (sC == number)
    loopInputs (x:xs) !dC !sC = 
      case txOutDatum txInOut  of
        NoOutputDatum       -> loopInputs xs dC sC
        (OutputDatum _)     -> 
          if txOutAddress txInOut == addr
            then loopInputs xs (dC + 1) (sC + 1) -- inline
            else loopInputs xs (dC + 1) sC
      where 
        txInOut :: BankTxOut
        txInOut = txInInfoResolved x

-- | Count the number of outputs that have inline datums.
{-# INLINABLE nOutputs #-}
nOutputs :: [BankTxOut] -> Integer -> Bool
nOutputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [BankTxOut] -> Integer  -> Bool
    loopInputs []     !counter = counter == number
    loopInputs (x:xs) !counter = 
      case txOutDatum x of
        NoOutputDatum       -> loopInputs xs   counter
        (OutputDatum _)     -> loopInputs xs ( counter + 1 ) -- inline
