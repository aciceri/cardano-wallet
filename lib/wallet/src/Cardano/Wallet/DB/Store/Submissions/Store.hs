{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--Copyright: 2022 IOHK
--License: Apache-2.0
--
--Implementation of a 'Store' for 'TxLocalSubmissionHistory'.
module Cardano.Wallet.DB.Store.Submissions.Store (mkStoreSubmissions) where

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..)
    , LocalTxSubmission (..)
    )
import Cardano.Wallet.DB.Store.Submissions.Model
    ( DeltaTxLocalSubmission (..)
    , TxLocalSubmissionHistory (..)
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Control.Arrow
    ( (&&&)
    )
import Control.Monad
    ( forM_
    )
import Data.DBVar
    ( Store (..)
    )
import Data.Foldable
    ( toList
    )
import Data.Map.Strict qualified as Map
import Data.Maybe
    ( fromJust
    )
import Database.Persist
    ( PersistEntity (keyFromRecordM)
    , PersistQueryWrite (deleteWhere)
    , PersistStoreWrite (repsertMany)
    , entityVal
    , selectList
    , (==.)
    )
import Database.Persist.Sql
    ( SqlPersistT
    )
import Prelude

repsertLocalSubmissions :: TxLocalSubmissionHistory -> SqlPersistT IO ()
repsertLocalSubmissions
    (TxLocalSubmissionHistory txs) =
        repsertMany [(fromJust keyFromRecordM x, x) | x <- toList txs]

mkStoreSubmissions
    :: WalletId
    -> Store (SqlPersistT IO) DeltaTxLocalSubmission
mkStoreSubmissions wid =
    Store
        { loadS =
            Right
                . TxLocalSubmissionHistory
                . Map.fromList
                . fmap ((localTxSubmissionTxId &&& id) . entityVal)
                <$> selectList [LocalTxSubmissionWalletId ==. wid] []
        , writeS = \txs -> do
            deleteWhere [LocalTxSubmissionWalletId ==. wid]
            repsertLocalSubmissions txs
        , updateS = \_ -> \case
            Expand addendum -> repsertLocalSubmissions addendum
            Prune tids -> forM_ tids $ \tid ->
                deleteWhere
                    [ LocalTxSubmissionWalletId ==. wid
                    , LocalTxSubmissionTxId ==. tid
                    ]
        }
