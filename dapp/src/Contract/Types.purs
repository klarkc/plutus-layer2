module Contract.Types
  ( ContractResult
  , TransactionId
  , Deposit
  , Withdraw
  , Address
  , Value
  )
  where

import Contract.Address as CA
import Contract.Transaction as CT
import Data.BigInt as DBI

data Value
type TransactionId = CT.TransactionHash
type Address = CA.Address
type Deposit = 
  { value :: Value
  }
type Withdraw =
  { donationTxId :: TransactionId
  }
type ContractResult =
  { txId :: TransactionId
  , txFinalFee :: Value
  }
