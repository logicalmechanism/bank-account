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
module BankAccountDatum
  ( OwnerInfo (..)
  , AccountInfo (..)
  , validAccountInfo
  , checkMinBal
  , BankAccountDatum (..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V2.Ledger.Api   as V2
-------------------------------------------------------------------------------
-- | Create the Datum Data object.
-------------------------------------------------------------------------------
data OwnerInfo = OwnerInfo
  { oPkh :: V2.PubKeyHash
  -- ^ The public key hash of the owner.
  , oSc  :: V2.PubKeyHash
  -- ^ The stake credential hash of the owner.
  }
PlutusTx.unstableMakeIsData ''OwnerInfo

-- old == new
instance Eq OwnerInfo where
  {-# INLINABLE (==) #-}
  a == b = ( oPkh a == oPkh b ) &&
           ( oSc  a == oSc  b )

data AccountInfo = AccountInfo
  { aThreshold :: Integer
  -- ^ The threshold for withdraws.
  , aMinimum   :: Integer
  -- ^ The bank account minimum for withdraws.
  , aName      :: V2.BuiltinByteString
  -- ^ The bank account name
  }
PlutusTx.unstableMakeIsData ''AccountInfo

-- old == new
instance Eq AccountInfo where
  {-# INLINABLE (==) #-}
  a == b = ( aThreshold a == aThreshold b ) &&
           ( aMinimum   a == aMinimum   b ) &&
           ( aName      a == aName      b )

-- Minimum balance is 2 ADA and threshold must be there or more
validAccountInfo :: AccountInfo -> Bool
validAccountInfo (AccountInfo thres min _) = (thres > min) && (min >= 2000000)

checkMinBal :: V2.Value -> AccountInfo -> Integer -> Bool
checkMinBal val (AccountInfo thres min _) inc = (lovelaceAmt >= thres) && (lovelaceAmt - inc >= min)
  where
    lovelaceAmt :: Integer
    lovelaceAmt = Value.valueOf val Value.adaSymbol Value.adaToken

data BankAccountDatum = Bank OwnerInfo AccountInfo
PlutusTx.unstableMakeIsData ''BankAccountDatum