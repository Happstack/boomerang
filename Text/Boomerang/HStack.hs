-- | a simple heteregenous stack library
{-# LANGUAGE TypeOperators #-}
module Text.Boomerang.HStack
    ( (:-)(..)
    , arg, hdTraverse, hdMap, hhead, htail, pop 
    ) where

infixr 8 :-
-- | A stack datatype. Just a better looking tuple.
data a :- b = a :- b deriving (Eq, Show)

-- | Stack destructor.
pop :: (a -> b -> r) -> (a :- b) -> r
pop f (a :- b) = f a b

-- | Get the top of the stack.
hhead :: (a :- b) -> a
hhead (a :- _) = a

-- | Get the stack with the top popped.
htail :: (a :- b) -> b
htail (_ :- b) = b

-- | Applicative traversal over the top of the stack.
hdTraverse :: Functor f => (a -> f b) -> a :- t -> f (b :- t)
hdTraverse f (a :- t) = fmap (:- t) (f a)

arg :: (ty -> r -> s) -> (a -> ty) -> (a :- r) -> s
arg c f = pop (c . f)

-- | Map over the top of the stack.
hdMap :: (a1 -> a2) -> (a1 :- b) -> (a2 :- b)
hdMap = arg (:-)

