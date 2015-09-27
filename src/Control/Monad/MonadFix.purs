module Control.Monad.MonadFix where

--

import Control.Monad.Eff
import Data.Fix
import Data.Lazy
import Prelude

--

class (Monad m) <= MonadFix m where
  liftFix :: forall a. FixEff a -> m a

mfix :: forall a m. (Fix a, MonadFix m) => (a -> m a) -> m a
mfix a2aM = do
  aP <- liftFix proxy
  a <- a2aM aP.value
  liftFix (aP.tie a)
  pure a

--

foreign import lazyLiftFix :: forall a. FixEff a -> Lazy a

instance lazyMonadFix :: MonadFix Lazy where
  liftFix = lazyLiftFix
