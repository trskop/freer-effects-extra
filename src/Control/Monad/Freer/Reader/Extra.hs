{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type class MonadEff.
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

    -- * Effect Operations
    , asks

    -- ** Support For Lenses
    , viewEff
    , viewsEff
    , iviewEff
    , iviewsEff
    )
  where

import Data.Function (($))
import Data.Functor ((<$>))
import Data.Functor.Const (Const(Const, getConst))  -- base >=4.9

import Control.Lens
    ( Getting
    , Indexed(Indexed)
    , IndexedGetting
    , LensLike'
    )
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Reader hiding (asks)
import Data.Profunctor.Unsafe as Unsafe ((#.))


-- | Get a specific component of the environment.
asks :: Member (Reader e) effs => (e -> a) -> Eff effs a
asks f = f <$> ask
{-# INLINE asks #-}

-- {{{ Lens -------------------------------------------------------------------

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

-- }}} Lens -------------------------------------------------------------------
