{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module AltDex.WalletLog
  ( WalletLog(..)
  , LogKey
  , getWalletLog
  , append
  , remove
  , lookup
  ) where

import           Data.List           hiding (lookup)
import           Data.Text           (Text)
import           Playground.Contract (FromJSON, Generic, ToJSON)
import           Prelude             hiding (lookup)

type LogKey= Text
data WalletLog a = WalletLog[(LogKey, a)] [LogKey]
                 deriving ( Eq, Show , Generic , FromJSON , ToJSON )

getWalletLog:: WalletLog a -> [a]
getWalletLog(WalletLog as _) = map snd as

append :: LogKey -> a -> WalletLog a
append hid h = WalletLog[(hid, h)] []

remove :: LogKey -> WalletLog a
remove hid = WalletLog[] [hid]

lookup :: LogKey -> WalletLog a -> Maybe a
lookup hid (WalletLog hs _) = snd <$> find (\h' -> hid == fst h') (reverse hs)

instance Semigroup (WalletLog a) where
   WalletLog as1 bs1 <> WalletLog as2 bs2 = WalletLog as3 bs3
      where
        as = filter (\(hId,_) -> notElem hId $ map fst as2) as1 <> as2
        bs = nub (bs1 <> bs2)
        as3 = filter (flip notElem bs . fst) as
        bs3 = filter (flip notElem $ fst `map` as) bs

instance Monoid (WalletLog a) where
   mempty = WalletLog[] []

