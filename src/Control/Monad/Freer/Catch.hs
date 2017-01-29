{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       $HEADER$
-- Description:  Support for overloaded extensible exceptions as implemented
--               by exceptions package.
-- Copyright:    (c) 2017 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Support for overloaded extensible exceptions as implemented by
-- <https://hackage.haskell.org/package/exceptions exceptions> package.
module Control.Monad.Freer.Catch
    (
    -- * Throwing Exceptions
      throw

    -- * Catching Exceptions
    , catch
    , handle
    , try

    -- * Utilities
    , bracket
    , bracket_
    , finally
    , onException

    -- * Asynchronous Exception Control
    , mask
    , uninterruptibleMask
    )
  where

import Control.Monad ((>>), (>>=), return)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.), const, flip)

import Control.Exception (Exception, SomeException)
import Control.Monad.Freer (Eff, send)
import Control.Monad.Freer.Internal (Arr, interpose)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import qualified Control.Monad.Catch as Exceptions
    ( mask
    , throwM
    , try
    , uninterruptibleMask
    )

import Control.Monad.Freer.Base (BaseMember, sendBase)
import Control.Monad.Freer.Control (EffectControl, fromBase, toBase, toBaseWith)


throw :: (Exception e, MonadThrow m, BaseMember m effs) => e -> Eff effs a
throw = sendBase . Exceptions.throwM

-- |
--
-- Code is based on
-- <http://okmij.org/ftp/Haskell/extensible/EffDynCatch.hs EffDynCatch.hs>
catch
    :: forall m effs a e
    .  (Exception e, MonadCatch m, BaseMember m effs)
    => Eff effs a
    -> (e -> Eff effs a)
    -> Eff effs a
catch m h = interpose return f m
  where
    f :: m r -> Arr effs r a -> Eff effs a
    f action k = send (Exceptions.try action) >>= \case
        Right x -> k x
        Left  e -> h e

handle
    :: (Exception e, MonadCatch m, BaseMember m effs)
    => (e -> Eff effs a)
    -> Eff effs a
    -> Eff effs a
handle = flip catch

try :: forall e m effs a
    .  (Exception e, MonadCatch m, BaseMember m effs)
    => Eff effs a
    -> Eff effs (Either e a)
try = interpose (return . Right) f
  where
    f :: m r -> Arr effs r (Either e a) -> Eff effs (Either e a)
    f action k = send (Exceptions.try action) >>= \case
        Right x -> k x
        Left e  -> return (Left e)

onException
    :: (MonadCatch m, BaseMember m effs)
    => Eff effs a
    -> Eff effs b
    -> Eff effs a
onException m onErr = m `catch` \e -> onErr >> throw (e :: SomeException)

bracket
    :: (MonadMask m, EffectControl m effs)
    => Eff effs a
    -> (a -> Eff effs b)
    -> (a -> Eff effs c)
    -> Eff effs c
bracket before after thing = mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

-- | A variant of 'bracket' where the return value from the first computation
-- is not required.
bracket_
    :: (MonadMask m, EffectControl m effs)
    => Eff effs a
    -> Eff effs b
    -> Eff effs c
    -> Eff effs c
bracket_ before after thing = bracket before (const after) (const thing)

-- | A specialised variant of 'bracket' with just a computation to run
-- afterward.
finally
    :: (MonadMask m, EffectControl m effs)
    => Eff effs a
    -- ^ Computation to run first.
    -> Eff effs b
    -- ^ Computation to run afterward (even if an exception was raised).
    -> Eff effs a
    -- ^ Returns the value from the first computation.
thing `finally` finalThing = mask $ \restore -> do
    r <- restore thing `onException` finalThing
    _ <- finalThing
    return r

mask
    :: (MonadMask m, EffectControl m effs)
    => ((forall a. Eff effs a -> Eff effs a) -> Eff effs r) -> Eff effs r
mask f =
    fromBase $ \e ->
        Exceptions.mask $ \restore ->
            toBaseWith e $ f $ \x ->
                fromBase $ \e' ->
                    restore (toBase x e')

uninterruptibleMask
    :: (MonadMask m, EffectControl m effs)
    => ((forall a. Eff effs a -> Eff effs a) -> Eff effs r) -> Eff effs r
uninterruptibleMask f =
    fromBase $ \e ->
        Exceptions.uninterruptibleMask $ \restore ->
            toBaseWith e $ f $ \x ->
                fromBase $ \e' ->
                    restore (toBase x e')
