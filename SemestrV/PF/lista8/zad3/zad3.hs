{-# LANGUAGE LambdaCase #-}
import qualified Data.Char as Char

data StreamTrans i o a
 = Return a
 | ReadS (Maybe i -> StreamTrans i o a)
 | WriteS o (StreamTrans i o a)

toLower :: StreamTrans Char Char ()
toLower = 
  ReadS $ \case
    Nothing -> Return ()
    Just x  -> WriteS (Char.toLower x) toLower

listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans (Return x) _ = ([], x)
listTrans (WriteS elem trans) xs = 
  (elem : resList, retVal) where
    (resList, retVal) = listTrans trans xs
listTrans (ReadS cont) []     = listTrans (cont Nothing) []
listTrans (ReadS cont) (i:is) = listTrans (cont $ Just i) is