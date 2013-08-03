import System.Random
import System.Environment
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as L

makePad :: Int -> [Word8]
makePad = randoms . mkStdGen

applyPad :: Int -> L.ByteString -> L.ByteString
applyPad seed = L.pack . zipWith xor (makePad seed) . L.unpack

main :: IO ()
main = getArgs >>= return . head >>= L.interact . applyPad . read
