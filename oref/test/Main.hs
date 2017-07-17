module Main where

import Data.ORef


-- | Non-conflicting write.
good :: Own (Int,Int)
good = do
    refX <- newORef 3
    refY <- newORef 5
    readORef refX (writeORef refY)
    readORef refX (\x -> readORef refY (\y -> return (x,y)))

-- | Conflicting write.
bad :: Own (Int,Int)
bad = do
    refX <- newORef 3
    refY <- newORef 5
    readORef refX (writeORef refX)
    readORef refX (\x -> readORef refY (\y -> return (x,y)))

magic :: Int -> Maybe Int
magic x = evalOwn $ do
  ref <- newORef x
  readORef ref (\a -> return (a + 1))

darkMagic :: Int -> Maybe Int
darkMagic x = evalOwn $ do
  ref <- newORef x
  readORef ref (\b -> writeORef ref b)
  readORef ref (\a -> return (a + 1))

main :: IO ()
main = do
  putStrLn "Hello"
