import Control.Monad ( ap, (>=>) )
import Data.Functor ( (<&>) )

data StreamTrans i o a
 = Return a
 | ReadS (Maybe i -> StreamTrans i o a)
 | WriteS o (StreamTrans i o a)

instance Functor (StreamTrans i o) where
  fmap f m = m <&> f       -- m >>= (return . f)

instance Applicative (StreamTrans i o) where
  pure = return
  (<*>) = ap

instance Monad (StreamTrans i o) where
  return = Return
  Return x       >>= f = f x
  ReadS cont     >>= f = ReadS    $ cont  >=> f -- (\res -> cont res >>= f) 
  WriteS x trans >>= f = WriteS x $ trans >>= f
