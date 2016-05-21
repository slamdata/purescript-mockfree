module Test.Mock.Mockfree
  ( Assertion()
  , Op(..)
  , OpPrism()
  , MockOp(..)
  , MockSpec()
  , assertEquals
  , expect
  , expectRead
  , expectWrite
  , op
  , readOp
  , runMock
  , writeOp
  ) where

  import Prelude(class Show, class Eq, Unit(), ($), (<<<), (>>=), (<$>), (<>), (==), bind, const, id, pure, show, unit)

  import Data.Either(Either(..))
  import Data.List(List(..), length, reverse)
  import Data.Lens.Types(PrismP())
  import Data.Lens.Prism(review)
  import Data.Lens.Fold(preview)
  import Data.Maybe(maybe)
  import Data.Tuple(Tuple(..))

  import Control.Monad.State.Trans(StateT(), runStateT)
  import Control.Monad.Free(Free(), foldFree, liftF)
  import Control.Monad.Trans(lift)
  import Control.Monad.State(State(), execState, get, modify, put)

  -- ***************************************************************************
  data Op a b c = Op a (b -> c)

  type OpPrism f a b = forall c. PrismP (f c) (Op a b c)

  -- | A helper function to create a read-only operation.
  readOp :: forall f b. OpPrism f Unit b -> Free f b
  readOp p = op p unit

  -- | A helper function to create a write-and-read operation.
  op :: forall f a b. OpPrism f a b -> a -> Free f b
  op p a = liftF $ review p (Op a id)

  -- | A helper function to create a write-only operation.
  writeOp :: forall f a. OpPrism f a Unit -> a -> Free f Unit
  writeOp p a = op p a

  -- ***************************************************************************
  type Assertion a = a -> Either String Unit

  -- | Creates an assertion that asserts values are equal to the specified
  -- | reference value.
  assertEquals :: forall a. (Show a, Eq a) => a -> Assertion a
  assertEquals e a = if e == a then Right unit else Left $ "Expected " <> show e <> " but found " <> show a

  -- | Creates an expectation for an arbitrary `f` operation.
  expect :: forall f a b. OpPrism f a b -> Assertion a -> (a -> b) -> MockSpec f
  expect p a f = modify (Cons (MockOp (\f' -> f' p a f)))

  -- | Creates an expectation for a read-only `f` operation.
  expectRead :: forall f b. OpPrism f Unit b -> b -> MockSpec f
  expectRead p b = expect p (const $ pure unit) (const b)

  -- | Creates an expectation for a write-only `f` operation.
  expectWrite :: forall f a. OpPrism f a Unit -> Assertion a -> MockSpec f
  expectWrite p a = expect p a (const unit)

  -- ***************************************************************************
  data MockOp f = MockOp (forall z. (forall a b. OpPrism f a b -> Assertion a -> (a -> b) -> z) -> z)

  type MockSpec f = State (List (MockOp f)) Unit

  -- | Attempts to execute a single operation using a mock op, returning an
  -- | error message if the expectation fails.
  runOp :: forall f c. MockOp f -> f c -> Either String c
  runOp (MockOp fold) fc =
    fold (\prism assert aToB -> maybe (Left "Unexpected operation") (\(Op a bToC) -> const (bToC (aToB a)) <$> assert a) $ preview prism fc)

  -- | Attempts to execute a Free program against a mock spec, returning an
  -- | error message if the expectations are not met.
  runMock :: forall f a0. MockSpec f -> Free f a0 -> Either String a0
  runMock spec program = runStateT (foldFree transform program) (reverse $ execState spec Nil) >>= finalize
    where
      finalize :: forall a. Tuple a (List (MockOp f)) -> Either String a
      finalize (Tuple a Nil) = Right a
      finalize (Tuple a   v) = Left $ "Unexpected early termination (" <> show (length v) <> " operation(s) remaining)"

      transform :: forall a. f a -> StateT (List (MockOp f)) (Either String) a
      transform fa = do
        ops <- get
        case ops of
          Nil -> lift (Left "Unexpected operation after end of mock spec" :: Either String a)
          Cons op' ops' -> do
            put ops'
            lift $ runOp op' fa
