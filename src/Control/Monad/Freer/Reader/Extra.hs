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
    ( module Control.Monad.Freer.Reader
    , asks
    )
  where

import Data.Functor ((<$>))

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Reader hiding (asks)


-- | Get a specific component of the environment.
asks :: Member (Reader e) effs => (e -> a) -> Eff effs a
asks f = f <$> ask
