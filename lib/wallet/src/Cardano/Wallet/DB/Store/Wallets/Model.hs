{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 Copyright: © 2018-2022 IOHK
 License: Apache-2.0

Pure model for the transactions ('Tx') and metadata about them ('TxMeta')
in a collection of wallets.

-}
module Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..)
    , DeltaWalletsMetaWithSubmissions (..)
    , TxWalletsHistory
    , walletsLinkedTransactions
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..), TxMetaHistory (..), mkTxMetaHistory )
import Cardano.Wallet.DB.Store.Submissions.Model
    ( DeltaTxLocalSubmission (..), TxLocalSubmissionHistory (..) )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxSet (..), mkTxSet )
import Data.Delta
    ( Delta (..) )
import Data.DeltaMap
    ( DeltaMap (Adjust, Insert) )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL
    ( over, view, (^.) )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Fmt
    ( Buildable, build )

import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import qualified Cardano.Wallet.DB.Store.Transactions.Model as TxStore
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as WT
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data DeltaTxWalletsHistory
    = ExpandTxWalletsHistory W.WalletId [(WT.Tx, WT.TxMeta)]
    | ChangeTxMetaWalletsHistory W.WalletId DeltaWalletsMetaWithSubmissions
    | RollbackTo W.SlotNo
    | GarbageCollectTxWalletsHistory
    | RemoveWallet W.WalletId
    deriving ( Show, Eq )

instance Buildable DeltaTxWalletsHistory where
    build = build . show

data DeltaWalletsMetaWithSubmissions
    = ChangeMeta DeltaTxMetaHistory
    | ChangeSubmissions DeltaTxLocalSubmission
    deriving ( Show, Eq )

type MetasAndSubmissionsHistory = (TxMetaHistory, TxLocalSubmissionHistory)

constraintSubmissions
    :: MetasAndSubmissionsHistory -> MetasAndSubmissionsHistory
constraintSubmissions (metas,submissions) =
    ( metas
    , over #relations (\m -> Map.restrictKeys m
                       $ Map.keysSet (metas ^. #relations)) submissions)

instance Delta DeltaWalletsMetaWithSubmissions where
    type Base DeltaWalletsMetaWithSubmissions = MetasAndSubmissionsHistory
    apply (ChangeMeta cm) (metas,submissions) =
        constraintSubmissions (apply cm metas, submissions)
    apply (ChangeSubmissions cs) (metas,submissions) =
        constraintSubmissions (metas, apply cs submissions)

type TxWalletsHistory =
    (TxSet, Map W.WalletId MetasAndSubmissionsHistory)

instance Delta DeltaTxWalletsHistory where
    type Base DeltaTxWalletsHistory = TxWalletsHistory
    apply (ExpandTxWalletsHistory wid cs) (txh,mtxmh) =
        ( apply (TxStore.Append $ mkTxSet $ fst <$> cs) txh
        , flip apply mtxmh $ case Map.lookup wid mtxmh of
              Nothing -> Insert wid (mkTxMetaHistory wid cs, mempty)
              Just _ ->
                  Adjust wid
                  $ ChangeMeta
                  $ TxMetaStore.Expand
                  $ mkTxMetaHistory wid cs)
    apply (ChangeTxMetaWalletsHistory wid change) (txh, mtxmh) =
        (txh, garbageCollectEmptyWallets
            $ mtxmh & apply (Adjust wid change)
            )
    apply (RollbackTo slot) (x, mtxmh) =
        -- Roll back all wallets to a given slot (number)
        -- and garbage collect transactions that no longer
        -- have a 'TxMeta' associated with them.
        garbageCollectEmptyWallets
        $ garbageCollectTxWalletsHistory
            (x, Map.map (apply (Adjust wid change)) mtxmh)
      where
        change
            = ChangeMeta
            . TxMetaStore.Manipulate
            . TxMetaStore.RollBackTxMetaHistory slot
    apply (RemoveWallet wid) (x, mtxmh) = (x, Map.delete wid mtxmh)
    apply GarbageCollectTxWalletsHistory x = garbageCollectTxWalletsHistory x

-- | Garbage collect all transactions that are no longer referenced
-- by any 'TxMeta'.
garbageCollectTxWalletsHistory :: TxWalletsHistory -> TxWalletsHistory
garbageCollectTxWalletsHistory (TxSet txh, mtxmh) = (TxSet (gc txh), mtxmh)
  where
    gc :: Map TxId x -> Map TxId x
    gc x = Map.restrictKeys x $ walletsLinkedTransactions mtxmh

-- necessary because database will not distinguish between
-- a missing wallet in the map
-- and a wallet that has no meta-transactions
garbageCollectEmptyWallets :: Map k MetasAndSubmissionsHistory
    -> Map k MetasAndSubmissionsHistory
garbageCollectEmptyWallets = Map.filter (not . null . view #relations . fst)

linkedTransactions :: MetasAndSubmissionsHistory -> Set TxId
linkedTransactions (TxMetaHistory m,_) = Map.keysSet m

walletsLinkedTransactions
    :: Map W.WalletId MetasAndSubmissionsHistory -> Set TxId
walletsLinkedTransactions = Set.unions . toList . fmap linkedTransactions
