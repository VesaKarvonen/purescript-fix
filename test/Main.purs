module Test.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION(), throwException, error)
import Control.Monad.MonadFix
import Data.Fix
import Data.Lazy
import qualified Data.List as S
import qualified Data.List.Lazy as L
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

oneTwoThrees :: Maybel (L.List Int)
oneTwoThrees = do
  Tuple xs _ <-
    mfix \(Tuple xs (Tuple ys zs)) -> do
      xs <- pure (L.cons 1 ys)
      ys <- pure (L.cons 2 zs)
      zs <- pure (L.cons 3 xs)
      pure (Tuple xs (Tuple ys zs))
  pure xs

testEq :: forall a e. (Eq a) => a -> a -> Eff (err :: EXCEPTION | e) Unit
testEq actual expected =
  if actual == expected
    then pure unit
    else throwException $ error $ "Invalid result"

takeAsStrict :: forall a. Int -> L.List a -> S.List a
takeAsStrict n xs = L.fromList (L.take n xs)

main = do
  xs <- fix \xs -> L.cons 1 xs
  testEq (takeAsStrict 5 xs) (S.Cons 1 (S.Cons 1 (S.Cons 1 (S.Cons 1 (S.Cons 1 S.Nil)))))

  case force (case oneTwoThrees of Maybel x -> x) of
    Just xs -> testEq (takeAsStrict 5 xs) (S.Cons 1 (S.Cons 2 (S.Cons 3 (S.Cons 1 (S.Cons 2 S.Nil)))))
    Nothing -> throwException $ error $ "Got nothing"
