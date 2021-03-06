{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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

    -- * ExcMembers
    , ExcMembers
    , Excs

    -- * Effect Evaluation
    , evalError
    , runErrorM
    , runErrorAsBase
    , mapError

    -- ** Multiple Exception Effects
    , HandleErrors(..)
    , evalErrors

    -- * Effect Operations
    , handleError
    , tryError

    -- ** Specialised Variants of throwError
    , throwNothing
    , throwNothing_
    , throwLeft
    , throwLeft_
    , throwException

    -- ** Conditionally Throw an Exception
    , thenThrow
    , thenThrowM
    , otherwiseThrow
    , otherwiseThrowM
    )
  where

import Control.Applicative (pure)
import Control.Exception (Exception, SomeException, toException)
import Control.Monad ((>>=))
import Data.Bool (Bool, not, otherwise)
import Data.Either (Either(Left, Right), either)
import Data.Function (($), (.), flip, id)
import Data.Functor ((<$>), void)
import Data.Kind (Constraint)
import Data.Maybe (Maybe, maybe)

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Freer (Eff, Member, handleRelay, send)
import Control.Monad.Freer.Exception

import Control.Monad.Freer.Base (BaseMember)


-- | Simplified 'Control.Monad.Freer.Members' constraint for 'Exc' effects.
--
-- Following constraints are equivalent:
--
-- * @'Control.Monad.Freer.Members' '[Exc Err1, Exc Err2, ..., Exc ErrN] effs@
--
-- * @'ExcMembers' '[Err1, Err2, ..., ErrN] effs@
type family ExcMembers (es :: [*]) (effs :: [* -> *]) :: Constraint where
    ExcMembers '[]       effs = ()
    ExcMembers (e ': es) effs = (Member (Exc e) effs, ExcMembers es effs)

-- | Following two types are equivalent:
--
-- * @'Eff' ('Exc' Err1, 'Exc' Err2, ..., 'Exc' ErrN ': effs) r@
--
-- * @'Eff ('Excs' '[Err1, Err2, ... ErrN] effs) r@
type family Excs (es :: [*]) (effs :: [* -> *]) :: [* -> *] where
    Excs '[]       effs = effs
    Excs (e ': es) effs = Exc e ': Excs es effs

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

-- | Stack of exception handlers.
data HandleErrors (es :: [*]) (effs :: [* -> *]) a where
    -- | There is no error left to be handled.
    NoErrorHandler :: HandleErrors '[] effs a

    HandleError
        :: (e -> Eff (Excs es effs) a)
        -> HandleErrors es effs a
        -> HandleErrors (e ': es) effs a

-- | Similar to 'runError', but instead of returning the error in 'Either', it
-- handles it using provided function.
--
-- @
-- 'evalError' h === 'runError' >=> either h pure
-- @
evalError :: (e -> Eff effs a) -> Eff (Exc e ': effs) a -> Eff effs a
evalError h = handleRelay pure $ \(Exc e) _k -> h e
{-# INLINEABLE evalError #-}

-- | Evaluate 'Exc' effects for all @(es :: [*])@ exceptions by using provided
-- stack of error handlers.
--
-- Following equations hold:
--
-- * @'evalErrors' 'NoErrorHandler' === 'id'@
--
-- * @'evalErrors' ('HandleError' h 'NoErrorHandler') === 'evalError' h@
evalErrors
    :: HandleErrors es effs a
    -- ^ Stack of error handlers that will be used during evaluation.
    -> Eff (Excs es effs) a
    -> Eff effs a
evalErrors = \case
    NoErrorHandler   -> id
    HandleError h hs -> evalErrors hs . evalError h
{-# INLINEABLE evalErrors #-}

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

-- | Similar to 'catchError'\/'handleError', but returns an 'Either' result
-- which is @('Right' a)@ if no exception of type @e@ was raised, or
-- @('Left' ex)@ if an exception of type @e@ was raised and its value is @ex@.
tryError :: Member (Exc e) effs => Eff effs a -> Eff effs (Either e a)
tryError m = (Right <$> m) `catchError` (pure . Left)

-- | Throw exception when 'Nothing' is encountered.
throwNothing :: Member (Exc e) effs => e -> Maybe a -> Eff effs a
throwNothing e = maybe (throwError e) pure
{-# INLINE throwNothing #-}

-- | Same as 'throwNothing', but it discards result.
throwNothing_ :: Member (Exc e) effs => e -> Maybe a -> Eff effs ()
throwNothing_ e = void . throwNothing e
{-# INLINE throwNothing_ #-}

-- | Throw exception when 'Left' is encountered.
throwLeft
    :: Member (Exc e) effs
    => (a -> e)
    -- ^ Convert 'Left' value in to an exception.
    -> Either a b
    -> Eff effs b
throwLeft f = either (throwError . f) pure
{-# INLINE throwLeft #-}

-- | Same as 'throwLeft', but it discards result.
throwLeft_
    :: Member (Exc e) effs
    => (a -> e)
    -- ^ Convert 'Left' value in to an exception.
    -> Either a b
    -> Eff effs ()
throwLeft_ f = void . throwLeft f
{-# INLINE throwLeft_ #-}

throwException
    :: (Exception e, Member (Exc SomeException) effs)
    => e
    -> Eff effs a
throwException = throwError . toException
{-# INLINE throwException #-}

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
--     isReadOnly >>= thenThrow (ReadOnlyHandle h)
--     -- -->8--
-- @
thenThrow :: Member (Exc e) effs => e -> Bool -> Eff effs ()
thenThrow e condition
  | condition = throwError e
  | otherwise = pure ()
{-# INLINE thenThrow #-}

-- | Throw exception when computation returns 'True'.
--
-- @
-- isReadOnly :: Handle -> 'Eff' effs 'Bool'
-- isReadOnly = -- -->8--
--
-- foo h = do
--     isReadOnly `thenThrowM` ReadOnlyHandle h
--     -- -->8--
-- @
thenThrowM :: Member (Exc e) effs => Eff effs Bool -> e -> Eff effs ()
thenThrowM m e = m >>= thenThrow e
{-# INLINE thenThrowM #-}

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
--     checkIfResourceExists rid >>= otherwiseThrow (ResourceDoesNotExist rid)
--     -- -->8--
-- @
otherwiseThrow :: Member (Exc e) effs => e -> Bool -> Eff effs ()
otherwiseThrow e = thenThrow e . not
{-# INLINE otherwiseThrow #-}

-- | Throw exception when computation returns 'False'.
--
-- Usage example:
--
-- @
-- checkIfResourceExists :: ResourceId -> 'Eff' effs 'Bool'
-- checkIfResourceExists = -- -->8--
--
-- foo rid = do
--     checkIfResourceExists rid `otherwiseThrowM` ResourceDoesNotExist rid
--     -- -->8--
-- @
otherwiseThrowM :: Member (Exc e) effs => Eff effs Bool -> e -> Eff effs ()
otherwiseThrowM m e = m >>= otherwiseThrow e
{-# INLINE otherwiseThrowM #-}

-- }}} Effect Operations ------------------------------------------------------
