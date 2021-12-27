import Data.Char (toLower)
import System.IO (isEOF)
import Control.Monad (unless)

echoLower :: IO ()
echoLower = do
  eof <- isEOF
  unless eof $ do
    char <- getChar
    putChar $ toLower char
    echoLower

main :: IO ()
main = do
  echoLower