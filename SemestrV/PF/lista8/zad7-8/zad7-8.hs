{-# LANGUAGE LambdaCase #-}

import Control.Monad ( ap, (>=>) , foldM )
import Data.Functor ( (<&>) )
import System.IO (isEOF)

data StreamTrans i o a
 = Return a
 | ReadS (Maybe i -> StreamTrans i o a)
 | WriteS o (StreamTrans i o a)

instance Functor (StreamTrans i o) where
  fmap f m = m <&> f

instance Applicative (StreamTrans i o) where
  pure = return
  (<*>) = ap

instance Monad (StreamTrans i o) where
  return = Return
  Return x       >>= f = f x
  ReadS cont     >>= f = ReadS    $ cont  >=> f -- (\res -> cont res >>= f) 
  WriteS x trans >>= f = WriteS x $ trans >>= f

data BF
  = MoveR      -- >
  | MoveL      -- <
  | Inc        -- +
  | Dec        -- -
  | Output     -- .
  | Input      -- ,
  | While [BF] -- [ ]
  deriving Show

runIOStreamsTrans :: StreamTrans Char Char a -> IO a
runIOStreamsTrans (Return retval) = return retval
runIOStreamsTrans (WriteS char trans) = do
  putChar char
  runIOStreamsTrans trans
runIOStreamsTrans (ReadS cont) = do
  eof <- isEOF
  if eof 
    then runIOStreamsTrans $ cont Nothing 
    else do
      char <- getChar 
      runIOStreamsTrans $ cont $ Just char

catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput =
  catchOutputAcc [] where
    catchOutputAcc acc (Return x)       = Return (x, reverse acc)
    catchOutputAcc acc (ReadS cont)     = ReadS $ catchOutputAcc acc . cont
    catchOutputAcc acc (WriteS x trans) = catchOutputAcc (x : acc) trans

brainfuckParse :: StreamTrans Char BF ()
brainfuckParse = 
  ReadS $ \case
    Nothing  -> return ()
    Just '>' -> WriteS MoveR brainfuckParse
    Just '<' -> WriteS MoveL brainfuckParse
    Just '+' -> WriteS Inc brainfuckParse
    Just '-' -> WriteS Dec brainfuckParse
    Just '.' -> WriteS Output brainfuckParse
    Just ',' -> WriteS Input brainfuckParse
    Just '[' -> do
      (_, bfList) <- catchOutput brainfuckParse
      WriteS (While bfList) brainfuckParse
    Just ']' -> return ()
    Just _   -> error "Syntax error"

runCatchOutput :: StreamTrans i b (a, [o]) -> [i] -> (a, [o])
runCatchOutput (Return (retval, reslist)) _ = (retval, reslist)
runCatchOutput (ReadS cont) []     = runCatchOutput (cont Nothing) []
runCatchOutput (ReadS cont) (x:xs) = runCatchOutput (cont $ Just x) xs
runCatchOutput (WriteS _ _) _      = error ""

parseString :: [Char] -> [BF]
parseString str = snd $ runCatchOutput (catchOutput brainfuckParse) str

coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

type Tape = ([Integer], [Integer])

evalBF :: Tape -> BF -> StreamTrans Char Char Tape
evalBF (l, x:r) MoveR = return (x:l, r)
evalBF (x:l, r) MoveL = return (l, x:r)
evalBF (l, x:r) Inc   = return (l, (x + 1):r)
evalBF (l, x:r) Dec   = return (l, (x - 1):r)
evalBF tape@(l, x:r) Output = WriteS (coerceEnum x) (return tape)
evalBF tape@(l, x:r) Input  = 
  ReadS $ \case
  Nothing -> return tape 
  Just y -> return (l, toInteger (fromEnum y):r)
evalBF tape@(l,r) (While bfList) = do
  case r of
    []    -> error "Odkryliśmy koniec nieskończoności"
    0 : _ -> return tape
    _ : _ -> do
      nextTape <- evalBFBlock tape bfList
      evalBF nextTape (While bfList)
evalBF _ _ = error "Odkryliśmy koniec nieskończoności"

evalBFBlock :: Tape -> [BF] -> StreamTrans Char Char Tape
evalBFBlock = foldM evalBF

runBF :: [BF] -> StreamTrans Char Char ()
runBF tokenList = do
  evalBFBlock (repeat 0, repeat 0) tokenList
  return ()

runIOBF :: [BF] -> IO ()
runIOBF tokenList = runIOStreamsTrans $ runBF tokenList