{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Control.Monad (Monad)
import Data.Function ((.), id)
import Data.Monoid (Monoid)

import Control.Monad.Freer (Eff)
import qualified Control.Monad.Freer.Internal as Internal (Eff(E, Val), qComp)
import Control.Monad.Trans.Class (lift)
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
import Data.OpenUnion (weaken)


-- | This class captures the notion that 'Eff' monad can be used as a base of a
-- monadic stack.
--
-- Example of a monadic stack where 'Eff' is used as a base monad:
--
-- @
-- type Aff effs a = 'ExceptT' err ('ContT' () ('Eff' effs)) a
-- @
--
-- In the above example 'liftEff' can be used to lift 'Eff' computation in to
-- 'Aff' computation.
--
-- Dual situation, to having 'Eff' on the bottom of monadic stack, is when
-- 'Eff' is used on top of a monadic stack. For details on how to handle such
-- situations see module "Control.Monad.Freer.Base".
class Monad m => MonadEff effs m | m -> effs where
    -- | Lift 'Eff' monad in to a monad that natively supports specified
    -- effects @effs :: [* -> *]@.
    liftEff :: Eff effs a -> m a

-- | Variant of 'liftEff' which allows effects to be specified explicitly using
-- a proxy type. This is useful in cases when type inference would fail without
-- explicitly knowing the exact type of @effs :: [* -> *]@.
--
-- >>> :set -XDataKinds -XTypeApplications
-- >>> import Data.Proxy
-- >>> :t liftEffP (Proxy @'[Reader Int, IO])
-- liftEffP (Proxy @'[Reader Int, IO])
--   :: MonadEff '[Reader Int, IO] m => Eff '[Reader Int, IO] a -> m a
liftEffP :: MonadEff effs m => proxy effs -> Eff effs a -> m a
liftEffP _proxy = liftEff

-- | 'Eff' monad can be embedded in to itself.
--
-- @
-- 'liftEff' = 'id'
-- @
instance MonadEff effs (Eff effs) where
    liftEff = id

-- | 'Eff' monad with less effects can be injected in to an 'Eff' with strictly
-- more effects.
weakenEff :: Eff effs a -> Eff (eff ': effs) a
weakenEff = \case
    Internal.Val x -> Internal.Val x
    Internal.E u q -> Internal.E (weaken u) (tsingleton k)
      where
        k = q `Internal.qComp` weakenEff
{-# INLINEABLE weakenEff #-}

-- {{{ Monad Transformers -----------------------------------------------------

-- | @'liftEff' = 'lift' . 'liftEff'@
instance MonadEff eff m => MonadEff eff (ContT e m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance MonadEff eff m => MonadEff eff (ExceptT e m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance MonadEff eff m => MonadEff eff (IdentityT m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance MonadEff eff m => MonadEff eff (ListT m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance MonadEff eff m => MonadEff eff (MaybeT m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance (MonadEff eff m, Monoid w) => MonadEff eff (Lazy.RWST r w s m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance (MonadEff eff m, Monoid w) => MonadEff eff (Strict.RWST r w s m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance MonadEff eff m => MonadEff eff (ReaderT r m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance MonadEff eff m => MonadEff eff (Lazy.StateT s m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance MonadEff eff m => MonadEff eff (Strict.StateT s m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance (MonadEff eff m, Monoid w) => MonadEff eff (Lazy.WriterT w m) where
    liftEff = lift . liftEff

-- | @'liftEff' = 'lift' . 'liftEff'@
instance (MonadEff eff m, Monoid w) => MonadEff eff (Strict.WriterT w m) where
    liftEff = lift . liftEff

-- }}} Monad Transformers -----------------------------------------------------
