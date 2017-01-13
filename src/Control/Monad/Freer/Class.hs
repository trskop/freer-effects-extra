{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Type class MonadEff.
-- Copyright:    (c) 2016-2017 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Type class 'MonadEff' is inspired by its namesake from PureScript (see
-- <https://github.com/purescript/purescript-eff purescript-eff>).
module Control.Monad.Freer.Class
    (
    -- * MonadEff
      MonadEff(..)
    , liftEffP
    , weakenEff
    )
  where

import Control.Applicative (Alternative(..), Applicative(pure))
import Control.Monad (Monad, join)
import Data.Function ((.), id)
import Data.Functor ((<$>))
import Data.Monoid (Monoid)

import Control.Monad.Freer (Eff, NonDetEff, makeChoiceA, send)
import qualified Control.Monad.Freer.Internal as Internal
    ( Eff(E, Val)
    , run
    , runM
    , qComp
    )
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)

import Data.FTCQueue (tsingleton)
import Data.Open.Union (weaken)


-- | This class captures the notion that 'Eff' monad can be used as a base of a
-- monadic stack.
class Monad m => MonadEff effs m where
    -- | Lift 'Eff' monad in to a monad that natively supports specified
    -- effects (@effs@).
    liftEff :: Eff effs a -> m a

-- | Variant of 'liftEff' which allows effects to be specified explicitly using
-- a proxy type. This is useful in cases when type inference would fail without
-- explicitly knowing the exact type of @effs@.
liftEffP :: MonadEff effs m => proxy effs -> Eff effs a -> m a
liftEffP _proxy = liftEff

-- | @'Eff' '[]@ is isomorphic to 'Identity', therefore it can be lifted in to
-- any monad.
--
-- @
-- runIdentity . 'liftEff' === 'Internal.run'
-- @
instance Monad m => MonadEff '[] m where
    liftEff = pure . Internal.run

-- | @'Eff' '[m]@, where @m@ is a 'Monad', is isomorphic to just @m@.
--
-- @
-- 'liftEff' = 'Internal.runM'
-- @
instance Monad m => MonadEff '[m] m where
    liftEff = Internal.runM

-- | 'Eff' monad can be embedded in to itself.
--
-- @
-- 'liftEff' = 'id'
-- @
instance MonadEff effs (Eff effs) where
    liftEff = id

-- | 'Eff' monad with less effects can be injected in to an 'Eff' with strictly
-- more effects.
--
-- @
-- 'liftEff' = 'weakenEff'
-- @
instance (effs ~ (e ': es)) => MonadEff effs (Eff (eff ': effs)) where
    liftEff = weakenEff

-- | 'Eff' monad with less effects can be injected in to an 'Eff' with strictly
-- more effects.
weakenEff :: (effs ~ (e ': es)) => Eff effs a -> Eff (eff ': effs) a
weakenEff = \case
    Internal.Val x -> Internal.Val x
    Internal.E u q -> Internal.E (weaken u) (tsingleton k)
      where
        k = q `Internal.qComp` weakenEff
{-# INLINEABLE weakenEff #-}

-- {{{ Monad Transformers -----------------------------------------------------

instance MonadEff eff m => MonadEff eff (ContT e m) where
    liftEff = lift . liftEff

instance MonadEff eff m => MonadEff eff (ExceptT e m) where
    liftEff = lift . liftEff

instance MonadEff eff m => MonadEff eff (IdentityT m) where
    liftEff = lift . liftEff

instance MonadEff eff m => MonadEff eff (ListT m) where
    liftEff = lift . liftEff

instance MonadEff eff m => MonadEff eff (MaybeT m) where
    liftEff = lift . liftEff

instance (MonadEff eff m, Monoid w) => MonadEff eff (Lazy.RWST r w s m) where
    liftEff = lift . liftEff

instance (MonadEff eff m, Monoid w) => MonadEff eff (Strict.RWST r w s m) where
    liftEff = lift . liftEff

instance MonadEff eff m => MonadEff eff (ReaderT r m) where
    liftEff = lift . liftEff

instance MonadEff eff m => MonadEff eff (Lazy.StateT s m) where
    liftEff = lift . liftEff

instance MonadEff eff m => MonadEff eff (Strict.StateT s m) where
    liftEff = lift . liftEff

instance (MonadEff eff m, Monoid w) => MonadEff eff (Lazy.WriterT w m) where
    liftEff = lift . liftEff

instance (MonadEff eff m, Monoid w) => MonadEff eff (Strict.WriterT w m) where
    liftEff = lift . liftEff

-- }}} Monad Transformers -----------------------------------------------------
