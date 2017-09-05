module Main where

import Data.ORef


-- | Non-conflicting write.
good :: Own (Int,Int)
good = do
    refX <- newORef 3
    refY <- newORef 5
    readORef refX (writeORef refY)
    readORef refX (\x -> readORef refY (\y -> return (x,y)))

evalGood :: IO (Either String (Int, Int))
evalGood = evalOwn good

-- | Conflicting write.
bad :: Own (Int,Int)
bad = do
    refX <- newORef 3
    refY <- newORef 5
    readORef refX (writeORef refX)
    readORef refX (\x -> readORef refY (\y -> return (x,y)))

evalBad :: IO (Either String (Int, Int))
evalBad = evalOwn bad

-- | Create an ORef and try to mutate it by adding 1 to it
magic :: Int -> IO (Either String Int)
magic x = evalOwn $ do
  ref <- newORef x
  readORef ref (\a -> return (a + 1))

darkMagic :: Int -> IO (Either String Int)
darkMagic x = evalOwn $ do
  ref <- newORef x
  readORef ref (\b -> writeORef ref b)
  readORef ref (\a -> return (a + 1))

main :: IO ()
main = do
  g <- evalGood
  putStrLn (show g)

  b <- evalBad
  putStrLn (show b)

  m <- magic 1
  putStrLn (show m)
