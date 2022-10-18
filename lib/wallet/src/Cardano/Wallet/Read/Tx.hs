{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--Copyright: © 2022 IOHK
--License: Apache-2.0
--
--The 'Tx' type represents transactions as they are read from the mainnet ledger.
--It is compatible with the era-specific index types from @cardano-ledger@.
module Cardano.Wallet.Read.Tx
    ( -- * Transactions
      Tx (..)
    , TxT
    )
where

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , MaryEra
    , ShelleyEra
    )
import Cardano.Api.Shelley qualified as Api
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Shelley.API qualified as Shelley
import Prelude

-- | Closed type family returning the ledger 'Tx' type for each known @era@.
type family TxT era where
    TxT ByronEra = Byron.ATxAux ()
    TxT ShelleyEra = Shelley.Tx (Api.ShelleyLedgerEra ShelleyEra)
    TxT AllegraEra = Shelley.Tx (Api.ShelleyLedgerEra AllegraEra)
    TxT MaryEra = Shelley.Tx (Api.ShelleyLedgerEra MaryEra)
    TxT AlonzoEra = Alonzo.ValidatedTx (Api.ShelleyLedgerEra AlonzoEra)
    TxT BabbageEra = Alonzo.ValidatedTx (Api.ShelleyLedgerEra BabbageEra)

-- | A tx in any era
newtype Tx era = Tx {unTx :: TxT era}

deriving instance Show (TxT era) => Show (Tx era)

deriving instance Eq (TxT era) => Eq (Tx era)
