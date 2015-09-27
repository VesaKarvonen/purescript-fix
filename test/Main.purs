module Test.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION(), throwException, error)
import Control.Monad.MonadFix
import Data.Fix
import Data.Lazy
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

data List a = Nil | Cons a (List a)

instance listEq :: (Eq a) => Eq (List a) where
  eq (Cons x xs) (Cons y ys) = eq x y && eq xs ys
  eq Nil         Nil         = true
  eq Nil         (Cons _ _)  = false
  eq (Cons _ _)  Nil         = false

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

oneTwoThrees :: Maybel (Stream Int)
oneTwoThrees = do
  Tuple xs _ <-
    mfix \(Tuple xs (Tuple ys zs)) -> do
      xs <- pure (scons 1 ys)
      ys <- pure (scons 2 zs)
      zs <- pure (scons 3 xs)
      pure (Tuple xs (Tuple ys zs))
  pure xs

testEq :: forall a e. (Eq a) => a -> a -> Eff (err :: EXCEPTION | e) Unit
testEq actual expected =
  if actual == expected
    then pure unit
    else throwException $ error $ "Invalid result"

main = do
  xs <- fix \xs -> defer \_ -> SCons 1 xs
  testEq (stake 5 xs) (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 Nil)))))

  case force (case oneTwoThrees of Maybel x -> x) of
    Just xs -> testEq (stake 5 xs) (Cons 1 (Cons 2 (Cons 3 (Cons 1 (Cons 2 Nil)))))
    Nothing -> throwException $ error $ "Got nothing"
