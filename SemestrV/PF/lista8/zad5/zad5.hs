{-# LANGUAGE LambdaCase #-}
import qualified Data.Char as Char
import System.IO (isEOF)

data StreamTrans i o a
 = Return a
 | ReadS (Maybe i -> StreamTrans i o a)
 | WriteS o (StreamTrans i o a)

toLower :: StreamTrans Char Char ()
toLower = 
  ReadS $ \case
    Nothing -> Return ()
    Just x  -> WriteS (Char.toLower x) toLower

(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
_ |>| (Return y) = Return y
trans1@(Return x) |>| (ReadS d)    = trans1 |>| d Nothing
(WriteS x c)      |>| (ReadS d)    = c |>| d (Just x)
trans1            |>| (WriteS y d) = WriteS y $ trans1 |>| d
(ReadS c)         |>| trans2       = ReadS (\x -> c x |>| trans2)

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

main :: IO ()
main = 
  runIOStreamsTrans (toLower |>| toLower)