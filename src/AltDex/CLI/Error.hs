{-# LANGUAGE LambdaCase #-}

module AltDex.CLI.Error
    ( whenNullThrow
    , whenSomethingThrow
    , whenNothingThrow
    , SomeError(..)) where


import           Prelude
import           Control.Monad.Except
import           Data.List.NonEmpty (nonEmpty, NonEmpty)
import           Data.String (IsString)
import           GHC.Exts (IsString(fromString))
import           GHC.Exception.Type (Exception)

whenNullThrow :: MonadError e m => e -> [a]  -> m (NonEmpty a)
whenNullThrow err =
    (\case
      Nothing -> throwError err
      Just xs -> return xs) . nonEmpty

whenSomethingThrow :: MonadError e m => (a -> e) -> Maybe a  -> m ()
whenSomethingThrow toErr = maybe (pure ()) (throwError . toErr)

whenNothingThrow :: MonadError e m => e -> Maybe a ->  m a
whenNothingThrow err = maybe (throwError err) pure

newtype SomeError =  SomeError String

instance Show SomeError where
  show   (SomeError m) = m

instance IsString SomeError where
  fromString v = SomeError v

instance Exception SomeError