module Test.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.MonadFix
import Data.Fix
import Data.Lazy
import Data.Maybe
import Data.Tuple
import Prelude

--

newtype Maybel a = Maybel (Lazy (Maybe a))

--

instance maybelApplicative :: Applicative Maybel where
  pure a = Maybel (defer \_ -> Just a)

instance maybelBind :: Bind Maybel where
  bind (Maybel aM) a2bM =
    Maybel (defer \_ ->
            force aM >>= \a -> case a2bM a of Maybel bM -> force bM)

instance maybelFunctor :: Functor Maybel where
  map = liftM1

instance maybelApply :: Apply Maybel where
  apply = ap

instance maybelMonad :: Monad Maybel

instance maybelLiftFix :: MonadFix Maybel where
  liftFix xF =
    Maybel (do x <- liftFix xF
               pure (pure x))

--

data List a = Nil | Cons a (List a)

instance listShow :: (Show a) => Show (List a) where
  show Nil = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

data StreamCons a = SNil
                  | SCons a (Stream a)
type Stream a = Lazy (StreamCons a)

sempty :: forall a. Stream a
sempty = defer \_ -> SNil

scons :: forall a. a -> Stream a -> Stream a
scons x xs = defer \_ -> SCons x xs

stake :: forall a. Int -> Stream a -> List a
stake n xs =
  if n <= 0
    then Nil
    else case force xs of
           SNil -> Nil
           SCons x xs -> Cons x (stake (n-1) xs)

--

ones :: Maybel (Stream Int)
ones = do
  Tuple xs ys <-
    mfix \(Tuple xs ys) -> do
            xs <- pure (scons 1 ys)
            ys <- pure (scons 2 xs)
            pure (Tuple xs ys)
  pure xs

main = do
  case force (case ones of Maybel x -> x) of
    Just xs -> log (show (stake 10 xs))
    Nothing -> log "No show"
  log "You should add some tests."
