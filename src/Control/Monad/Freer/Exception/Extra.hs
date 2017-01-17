{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       $HEADER$
-- Description:  Extra goodies for Exc effect.
-- Copyright:    (c) 2017 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- <https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html Extended module>
-- built on top of "Control.Monad.Freer.Exception".
module Control.Monad.Freer.Exception.Extra
    (
    -- * Re-export Module That's Beeing Extended
    --
    -- | Everything is beeing re-exported from it.
      module Control.Monad.Freer.Exception

    -- * Effect Evaluation
    , runErrorM
    , runErrorAsBase
    , mapError

    -- * Effect Operations
    , handleError
    , throwNothing
    , throwLeft
    , thenThrow
    , otherwiseThrow

    -- ** Support For Lenses
    )
  where

import Control.Applicative (pure)
import Control.Exception (Exception)
import Control.Monad ((>>=))
import Data.Bool (Bool, not, otherwise)
import Data.Either (Either, either)
import Data.Maybe (Maybe, maybe)
import Data.Function (($), (.), flip)

import Control.Monad.Catch (MonadThrow, throwM)

import Control.Monad.Freer (Eff, Member, handleRelay, send)
import Control.Monad.Freer.Exception

import Control.Monad.Freer.Base (BaseMember)


-- {{{ Effect Evaluation ------------------------------------------------------

-- | Evaluate 'Exc' effect in terms of base effect using specified base effect
-- operation.
runErrorAsBase
    :: BaseMember m effs
    => (forall r. e -> m r)
    -- ^ Throw exception in context of base effect.
    -> Eff (Exc e ': effs) a
    -> Eff effs a
runErrorAsBase throw = handleRelay pure $ \(Exc e) k -> send (throw e) >>= k
    -- We aren't discarding the continuation, since we aren't actually handling
    -- the exception effect fully, but relaying it to other effect/monad, and
    -- we need it to be able to catch the exception and resume the
    -- continuation.
{-# INLINEABLE runErrorAsBase #-}

-- | Evaluate 'Exc' effect in terms of base effect, a monad that has
-- 'MonadThrow' capabilities.
--
-- This function is just a specialisation of 'runErrorAsBase':
--
-- @
-- 'runErrorM' = 'runErrorAsBase' 'throwM'
-- @
runErrorM
    :: (Exception e, MonadThrow m, BaseMember m effs)
    => Eff (Exc e ': effs) a
    -> Eff effs a
runErrorM = runErrorAsBase throwM
{-# INLINE runErrorM #-}

-- | Evaluate @('Exc' e)@ effect by mapping it in to another ('Exc' e') effect.
-- This is useful for example when embedding more specific exception in a more
-- generic one.
mapError
    :: (Member (Exc e') effs)
    => (e -> e')
    -> Eff (Exc e ': effs) a
    -> Eff effs a
mapError f = handleRelay pure $ \(Exc e) k -> throwError (f e) >>= k
    -- We aren't discarding the continuation, since we aren't actually handling
    -- the exception effect fully, but relaying it to other effect/monad, and
    -- we need it to be able to catch the exception and resume the
    -- continuation.
{-# INLINEABLE mapError #-}

-- }}} Effect Evaluation ------------------------------------------------------

-- {{{ Effect Operations ------------------------------------------------------

-- | A version of 'catchError' with the arguments flipped.
--
-- Usage example:
--
-- @
-- foo = 'handleError' errorHandler $ do
--     -- -->8--
--   where
--     errorHandler =
--         -- -->8--
-- @
handleError
    :: Member (Exc e) effs
    => (e -> Eff effs a)
    -> Eff effs a
    -> Eff effs a
handleError = flip catchError
{-# INLINE handleError #-}

-- | Throw exception when 'Nothing' is encountered.
throwNothing :: Member (Exc e) effs => e -> Maybe a -> Eff effs a
throwNothing e = maybe (throwError e) pure
{-# INLINE throwNothing #-}

-- | Throw exception when 'Left' is encountered.
throwLeft
    :: Member (Exc e) effs
    => (a -> e)
    -- ^ Convert 'Left' value in to an exception.
    -> Either a b
    -> Eff effs b
throwLeft f = either (throwError . f) pure
{-# INLINE throwLeft #-}

-- | Throw exception when condition is 'True'.
--
-- Relation between 'thenThrow' and 'otherwiseThrow' can be described as:
--
-- @
-- 'thenThrow' e = 'otherwiseThrow' e . 'not'
-- @
--
-- @
-- isReadOnly :: Handle -> 'Eff' effs 'Bool'
-- isReadOnly = -- -->8--
--
-- foo h = do
--     isReadOnly `thenThrow` ReadOnlyHandle h
--     -- -->8--
-- @
thenThrow :: Member (Exc e) effs => e -> Bool -> Eff effs ()
thenThrow e condition
  | condition = throwError e
  | otherwise = pure ()
{-# INLINE thenThrow #-}

-- | Throw exception when condition is 'False'.
--
-- Relation between 'thenThrow' and 'otherwiseThrow' can be described as:
--
-- @
-- 'otherwiseThrow' e = 'thenThrow' e . 'not'
-- @
--
-- Usage example:
--
-- @
-- checkIfResourceExists :: ResourceId -> 'Eff' effs 'Bool'
-- checkIfResourceExists = -- -->8--
--
-- foo rid = do
--     checkIfResourceExists rid `otherwiseThrow` ResourceDoesNotExist rid
--     -- -->8--
-- @
otherwiseThrow :: Member (Exc e) effs => e -> Bool -> Eff effs ()
otherwiseThrow e = thenThrow e . not
{-# INLINE otherwiseThrow #-}

-- }}} Effect Operations ------------------------------------------------------
