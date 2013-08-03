import Control.Monad.Error

data MyError = ThisError
             | ThatError
             | OtherError String
             deriving (Show)

instance Error MyError where
    strMsg str = OtherError str

f :: Integer -> Either MyError Integer
f 0 = throwError $ OtherError "Foo"
f n = return n

g :: Integer -> Either MyError Integer
g 1 = throwError $ OtherError "Bar"
g n = return n
