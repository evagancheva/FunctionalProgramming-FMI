module Sol9 where
import Prelude hiding (Applicative (..), Either (..), Functor (..), Monad (..), (<$>))

--task 0
class MyFunctor f where
    myFmap :: (a->b) -> f a -> f b
    (<$>) :: (MyFunctor f) => (a -> b) -> f a -> f b
    (<$>) = myFmap

class MyFunctor f => MyApplicative f where
    pure :: a -> f a
    (<*>) :: f(a->b) -> f a -> f b
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 op lhs rhs = op <$> lhs <*> rhs

class (MyApplicative m ) => MyMonad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b


--task 1
instance MyFunctor Maybe where
    myFmap _ Nothing = Nothing
    myFmap f (Just x) = Just (f x)

instance MyApplicative Maybe where
    pure :: a -> Maybe a
    pure = Just

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Just f <*> Just x = Just $ f x

    liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    liftA2 op (Just x) (Just y) = Just $ x `op` y
    liftA2 _ _ _ = Nothing

instance MyMonad Maybe where
    return = Just
    --return = pure

    Nothing >>= _ = Nothing
    Just x >>= f = f x

    Nothing >> _ = Nothing
    _ >> rhs = rhs


--task 2
data Either a b = Left a | Right b
    deriving (Show)

instance MyFunctor (Either c) where
    myFmap _ (Left x) = Left x
    myFmap f (Right x) = Right (f x)

instance MyApplicative (Either c) where
    pure = Right

    Right f <*> Right x = Right $ f x
    Left f <*> _ = Left f
    Right _ <*> Left x = Left x

instance MyMonad (Either c) where
    return = pure
    Left x >>= f = Left x
    Right y >>= f = f y

    Left x >> _ = Left x
    _ >> rhs = rhs

--task 3
newtype Sum a = Sum a deriving Show
newtype Product a = Product a deriving Show

instance Num a => Semigroup (Sum a) where
    (Sum x) <> (Sum y) = Sum (x + y)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

instance MyFunctor Sum where
    myFmap f (Sum x) = Sum (f x)

instance MyApplicative Sum where
    pure = Sum
    (<*>) (Sum f) (Sum x) = Sum (f x)

instance MyMonad Sum where
    return = Sum
    (>>=) (Sum x) f = f x

    ma >> mb = ma >>= const mb

--Product

--task 4
data List a = Nil | Cons a (List a)

instance MyFunctor List where
    myFmap _ Nil = Nil
    myFmap f (Cons x xs) = Cons (f x) (myFmap f xs)

instance MyApplicative List where
    pure x = Cons x Nil
    <*> (Cons f fs) xs = 








