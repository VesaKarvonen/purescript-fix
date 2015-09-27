module Test.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION(), throwException, error)
import Control.Monad.MonadFix
import Data.Fix
import Data.Lazy
import Data.List.Lazy
import Data.Maybe
import Data.Tuple
import Prelude

--

newtype Maybel a = Maybel (Lazy (Maybe a))

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

oneTwoThrees :: Maybel (List Int)
oneTwoThrees = do
  Tuple xs _ <-
    mfix \(Tuple xs (Tuple ys zs)) -> do
      xs <- pure (cons 1 ys)
      ys <- pure (cons 2 zs)
      zs <- pure (cons 3 xs)
      pure (Tuple xs (Tuple ys zs))
  pure xs

testEq :: forall a e. (Eq a) => a -> a -> Eff (err :: EXCEPTION | e) Unit
testEq actual expected =
  if actual == expected
    then pure unit
    else throwException $ error $ "Invalid result"

takeAsArray :: forall a. Int -> List a -> Array a
takeAsArray n xs = fromList (take n xs)

main = do
  xs <- fix \xs -> cons 1 xs
  testEq (takeAsArray 5 xs) [1, 1, 1, 1, 1]

  case force (case oneTwoThrees of Maybel x -> x) of
    Just xs -> testEq (takeAsArray 5 xs) [1, 2, 3, 1, 2]
    Nothing -> throwException $ error $ "Got nothing"
