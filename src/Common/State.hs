module State ( State (..) ) where

newtype State s a =
  State { runState :: s -> (s, a) }

instance Functor (State s) where

  fmap f (State run) = State $ \s ->
    let (s', a) = run s in
    (s', f a)

instance Applicative (State s) where

  pure a = State $ \s -> (s, a)

  State sf <*> State sx = State $ \s ->
    let (s',  f) = sf s
        (s'', x) = sx s'
    in (s'', f x)

instance Monad (State s) where

  return = pure

  State sx >>= sf = State $ \s ->
    let (s', x) = sx s
        State f = sf x in
    f s'
