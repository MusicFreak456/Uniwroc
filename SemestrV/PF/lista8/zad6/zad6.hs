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

catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput =
  catchOutputAcc [] where
    catchOutputAcc acc (Return x)       = Return (x, reverse acc)
    catchOutputAcc acc (ReadS cont)     = ReadS $ catchOutputAcc acc . cont
    catchOutputAcc acc (WriteS x trans) = catchOutputAcc (x : acc) trans

runCatchOutput :: StreamTrans i b (a, [o]) -> [i] -> (a, [o])
runCatchOutput (Return (retval, reslist)) _ = (retval, reslist)
runCatchOutput (ReadS cont) []     = runCatchOutput (cont Nothing) []
runCatchOutput (ReadS cont) (x:xs) = runCatchOutput (cont $ Just x) xs
runCatchOutput (WriteS _ _) _      = error ""