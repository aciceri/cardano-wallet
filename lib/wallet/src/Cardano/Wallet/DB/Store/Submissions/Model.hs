{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--Copyright: © 2022 IOHK
--License: Apache-2.0
--
--Data type 'TxLocalSubmissionHistory' for storing a set of submitted transactions.
--Transactions are encoded "as" expressed in DB tables.
module Cardano.Wallet.DB.Store.Submissions.Model
    ( TxLocalSubmissionHistory (..)
    , DeltaTxLocalSubmission (..)
    )
where

import Cardano.Wallet.DB.Sqlite.Schema
    ( LocalTxSubmission (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import Prelude

-- | All transactions that the wallet has submitted to a node,
-- indexed by transaction id.
--
-- Typically, transactions are submitted through the
-- LocalTxSubmission mini-protocol, hence the name of this type.
newtype TxLocalSubmissionHistory = TxLocalSubmissionHistory {relations :: Map TxId LocalTxSubmission}
    deriving (Eq, Show, Generic, Monoid, Semigroup)

data DeltaTxLocalSubmission
    = -- | Add or overwrite (by id) local-submission-transactions.
      Expand TxLocalSubmissionHistory
    | -- | Remove submissions by id.
      Prune [TxId]
    deriving (Eq, Show, Generic)

instance Buildable DeltaTxLocalSubmission where
    build = build . show

instance Delta DeltaTxLocalSubmission where
    type Base DeltaTxLocalSubmission = TxLocalSubmissionHistory
    apply (Expand addendum) x = addendum <> x
    apply (Prune tids) (TxLocalSubmissionHistory m) =
        TxLocalSubmissionHistory $
            Map.withoutKeys m (Set.fromList tids)
