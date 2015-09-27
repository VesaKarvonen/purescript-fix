module Data.Fix
  ( FIX()
  , FixEff()
  , Proxy()
  , Fix, proxy, fix
  ) where

--

import Control.Monad.Eff
import Data.Lazy
import Data.Tuple
import Prelude

--

foreign import data FIX :: !
type FixEff a = forall e. Eff (fix :: FIX | e) a
type Proxy a = {value :: a, tie :: a -> FixEff Unit}

class Fix a where
  proxy :: FixEff (Proxy a)

fix :: forall a. (Fix a) => (a -> a) -> FixEff a
fix a2a = fix' proxy a2a

--

instance fixTuple :: (Fix a, Fix b) => Fix (Tuple a b) where
  proxy = proxyTuple proxy proxy

fix' :: forall a. FixEff (Proxy a) -> (a -> a) -> FixEff a
fix' aPE a2a = do
  aP <- aPE
  let a = a2a aP.value
  aP.tie a
  pure a

valueTuple :: forall a b. Proxy a -> Proxy b -> Tuple a b
valueTuple aP bP =
  Tuple aP.value bP.value

tieTuple :: forall a b. Proxy a -> Proxy b -> Tuple a b -> FixEff Unit
tieTuple aP bP (Tuple a b) = do
  aP.tie a
  bP.tie b

proxyTuple :: forall a b. FixEff (Proxy a) -> FixEff (Proxy b) -> FixEff (Proxy (Tuple a b))
proxyTuple aPE bPE = do
  aP <- aPE
  bP <- bPE
  pure {value: valueTuple aP bP, tie: tieTuple aP bP}

--

foreign import lazyProxy :: forall a. FixEff (Proxy (Lazy a))

instance lazyFix :: Fix (Lazy a) where
  proxy = lazyProxy
