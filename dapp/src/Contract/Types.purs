module Contract.Types
  ( ContractResult
  , TransactionId
  , Deposit
  , Withdraw
  , Address
  , Value
  , Tx
  , TxData
  )
  where

import Contract.Address as CA
import Contract.Crypto (Hash)
import Contract.MerkleTree (Proof)
import Contract.Transaction as CT
import Data.BigInt as DBI

type TxData = String
type Tx = Hash
type Value = DBI.BigInt
type TransactionId = CT.TransactionHash
type Address = CA.Address
type Deposit = 
  { value :: Value
  , root :: Tx
  }
type Withdraw =
  { depositTxId :: TransactionId
  , element :: TxData
  , proof :: Proof
  }
type ContractResult =
  { txId :: TransactionId
  , txFinalFee :: Value
  }
