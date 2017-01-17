{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       $HEADER$
-- Description:  Extra goodies for Reader effect.
-- Copyright:    (c) 2017 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- <https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html Extended module>
-- built on top of "Control.Monad.Freer.Reader".
module Control.Monad.Freer.Reader.Extra
    (
    -- * Re-export Module That's Beeing Extended
    --
    -- | Everything is beeing re-exported from it except 'asks', which is
    -- reimplemented here with more general type signature.
      module Control.Monad.Freer.Reader

    -- * Effect Evaluation
    , runReaderM
    , runReaderAsBase

    -- * Effect Operations
    , asks

    -- ** Support For Lenses
    , viewEff
    , viewsEff
    , iviewEff
    , iviewsEff
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Functor.Const (Const(Const, getConst))  -- base >=4.9

import Control.Lens
    ( Getting
    , Indexed(Indexed)
    , IndexedGetting
    , LensLike'
    )
import Control.Monad.Freer (Eff, Member, handleRelay, send)
import Control.Monad.Freer.Reader hiding (asks)
import Control.Monad.Reader.Class (MonadReader)
import qualified Control.Monad.Reader.Class as MonadReader (ask)
import Data.Profunctor.Unsafe as Unsafe ((#.))

import Control.Monad.Freer.Base (BaseMember)


-- {{{ Effect Evaluation ------------------------------------------------------

-- | Evaluate 'Reader' effect in terms of base effect using specified base
-- effect operations.
runReaderAsBase
    :: BaseMember m effs
    => m e
    -> Eff (Reader e ': effs) a
    -> Eff effs a
runReaderAsBase baseAsk = handleRelay pure $ \Reader k -> send baseAsk >>= k
{-# INLINEABLE runReaderAsBase #-}

-- | Evaluate 'Reader' effect in terms of base effect, a monad that has
-- 'MonadReader' capabilities.
--
-- This function is just a specialisation of 'runReaderAsBase':
--
-- @
-- 'runReaderM' = 'runReaderAsBase' 'MonadReader.ask'
-- @
runReaderM
    :: (MonadReader e m, BaseMember m effs)
    => Eff (Reader e ': effs) a
    -> Eff effs a
runReaderM = runReaderAsBase MonadReader.ask
{-# INLINE runReaderM #-}

-- }}} Effect Evaluation ------------------------------------------------------

-- {{{ Effect Operations ------------------------------------------------------

-- | Get a specific component of the environment.
asks :: Member (Reader e) effs => (e -> a) -> Eff effs a
asks f = f <$> ask
{-# INLINE asks #-}

-- {{{ Effect Operations -- Lens ----------------------------------------------

-- | Variant of 'Control.Lens.view' that works for 'Reader' effect. See /lens/
-- documentation for more details, and examples.
viewEff :: Member (Reader s) effs => Getting a s a -> Eff effs a
viewEff l = asks (getConst Unsafe.#. l Const)
{-# INLINE viewEff #-}

-- | Variant of 'Control.Lens.views' that works for 'Reader' effect. See /lens/
-- documentation for more details, and examples.
viewsEff
    :: Member (Reader s) effs
    => LensLike' (Const r) s a
    -> (a -> r)
    -> Eff effs r
viewsEff l f = asks (getConst Unsafe.#. l (Const Unsafe.#. f))
{-# INLINE viewsEff #-}

-- | Variant of 'Control.Lens.iview' that works for 'Reader' effect. See /lens/
-- documentation for more details, and examples.
iviewEff
    :: Member (Reader s) effs
    => IndexedGetting i (i, a) s a
    -> Eff effs (i, a)
iviewEff l =
    asks (getConst Unsafe.#. l (Indexed $ \i -> Const Unsafe.#. (,) i))
{-# INLINE iviewEff #-}

-- | Variant of 'Control.Lens.iviews' that works for 'Reader' effect. See
-- /lens/ documentation for more details, and examples.
iviewsEff
    :: Member (Reader s) effs
    => IndexedGetting i r s a
    -> (i -> a -> r)
    -> Eff effs r
iviewsEff l f = asks (getConst Unsafe.#. l (Const Unsafe.#. Indexed f))
{-# INLINE iviewsEff #-}

-- }}} Effect Operations -- Lens ----------------------------------------------

-- {{{ Effect Operations ------------------------------------------------------
