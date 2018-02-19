module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent

import Data.Typeable (Typeable)

import Data.ORef


-- | newORef test
-- Trivial test for newORef
newORefExample :: Own (ORef String)
newORefExample = newORef "hello"

evalNewORefExample :: IO (Either String (ORef String))
evalNewORefExample = startOwn newORefExample

-- | dropORef test
-- Test if a ORef that is dropped is really dropped
dropORefExample1 :: Own ()
dropORefExample1 = do
  ref <- newORef "hello"
  dropORef ref

-- This should suceed and evaluate to Right ()
evalDropORefExample1 :: IO (Either String ())
evalDropORefExample1 = startOwn dropORefExample1

-- | dropORef test
-- Test if a ORef that is dropped is really dropped by using it after dropping it.
dropORefExample2 :: Own ()
dropORefExample2 = do
  ref <- newORef "hello"
  dropORef ref
  _copy <- copyORef ref -- this will fail
  return ()

-- This should fail and evaluate to Left String
evalDropORefExample2 :: IO (Either String ())
evalDropORefExample2 = startOwn dropORefExample2

-- | copyORef test
-- A simple test for copyORef
copyORefExample :: Own ()
copyORefExample = do
  ref <- newORef "hello"
  _copy <- copyORef ref
  return ()

-- This should succeed and evaluate to Right ()
evalCopyORefExample :: IO (Either String ())
evalCopyORefExample = startOwn copyORefExample

-- Example from paper of references diverging after a copy operation
divergingCopy = startOwn $ do
  x <- newORef (1 :: Int)
  y <- copyORef x
  let f :: Int -> Own Int
      f i = return (i+1)
  borrowORef' x f
  xContents <- readORef x
  yContents <- readORef y
  return (xContents, yContents)

letTest :: Own ()
letTest = do
  ref <- newORef "hello"
  let test = ref
  _testCopy <- copyORef test
  _copy <- copyORef ref
  return ()

evalLetTest :: IO (Either String ())
evalLetTest = startOwn letTest

-- | moveORef success test
-- A simple test for the move operation that should pass.
moveORefExample1 :: Own ()
moveORefExample1 = do
  x <- newORef "hello"
  -- move resource from x to y
  _y <- moveORef x
  return ()

-- This should succeed and evaluate to Right ()
evalMoveORefExample1 :: IO (Either String ())
evalMoveORefExample1 = startOwn moveORefExample1

-- | moveORef failure test
-- A simple test for the move operation that should fail.
moveORefExample2 :: Own ()
moveORefExample2 = do
  x <- newORef "hello"
  -- move resource from x to y
  _y <- moveORef x
  -- move resource from x to z
  _z <- moveORef x -- this operation will fail
  return ()

-- This should fail and evaluate to Left String
evalMoveORefExample2 :: IO (Either String ())
evalMoveORefExample2 = startOwn moveORefExample2

-- | moveORef' success test
-- Test for the second kind of move operation that should pass
moveExample1 :: Own ()
moveExample1 = do
  x <- newORef "hello"
  y <- newORef "world"
  -- move resource from x to the existing ORef y
  moveORef' x y
  return ()

-- This should pass and evaluate to Right ()
evalMoveExample1 :: IO (Either String ())
evalMoveExample1 = startOwn moveExample1

-- | moveORef' failure test
-- Test for the second kind of move operation that should fail
moveExample2 :: Own ()
moveExample2 = do
  x <- newORef "hello"
  y <- newORef "world"
  z <- newORef "?"
  -- move resource from x to the existing ORef y
  moveORef' x y
  -- move resource from x to the existing ORef z
  moveORef' x z -- this step will fail
  return ()

-- This should fail and evaluate to Left String
evalMoveExample2 :: IO (Either String ())
evalMoveExample2 = startOwn moveExample2


-- Example from paper of a move operation that will pass.
moveOp1 = startOwn $ do
  x <- newORef "greeting"
  y <- moveORef x
  yContents <- readORef y
  return yContents

-- Example from paper of a move operation that will fail.
moveOp2 = startOwn $ do
  x <- newORef "greeting"
  y <- moveORef x
  xContents <- readORef x
  yContents <- readORef y
  return (xContents, yContents)

-- | borrowORef simple success test
-- Trivial test for the borrow operation that should pass.
borrowORefExample1 :: Own ()
borrowORefExample1 = do
  x <- newORef "Hello from ORef x in borrowExample1!"
  -- let f = (\x -> (liftIO $ putStrLn $ "The contents of the ORef is " ++ x))
  let f :: String -> IO ()
      f a = putStrLn $ "The contents of the ORef is " ++ a
      g :: String -> Own ()
      g b = liftIO $ f b
  _ <- borrowORef x g
  return ()

-- This should pass and evaluate to Right ()
-- and should print the contents of the ORef
evalBorrowORefExample1 :: IO (Either String ())
evalBorrowORefExample1 = startOwn borrowORefExample1

-- | borrowORef simple failure test
-- Trivial test for the borrow operation that should fail
borrowORefExample2 :: Own ()
borrowORefExample2 = do
  x <- newORef (1 :: Int)
  _y <- moveORef x
  let f :: Int -> Own Int
      f i = return (i+1)
  _result <- borrowORef x f -- this will fail because x has moved
  return ()

-- This should pass and evaluate to Left String
evalBorrowORefExample2 :: IO (Either String ())
evalBorrowORefExample2 = startOwn borrowORefExample2

-- | borrowORef test
-- A test to show that a partially applied writeORef passed as the function
-- to a readORef will not be able to mutate the original ORef.
borrowORefExample3 :: Own ()
borrowORefExample3 = do
  ref <- newORef "Original value of the ref"
  let f :: String -> Own ()
      f str = writeORef ref str -- a partially applied write on the existing ref
  borrowORef ref f -- this should fail
  return ()

-- This should evaluate to Left String
evalBorrowORefExample3 :: IO (Either String ())
evalBorrowORefExample3 = startOwn borrowORefExample3

-- | writeORef test
-- A trivial test to show a write operation that will succeed.
writeORefExample1 :: Own ()
writeORefExample1 = do
  x <- newORef "Original contents"
  writeORef x "Some new contents"
  return ()

-- This should pass and evaluate to Right ()
evalWriteORefExample1 :: IO (Either String ())
evalWriteORefExample1 = startOwn writeORefExample1

-- | writeORef test
-- A trivial test to show a write operation that will fail.
writeORefExample2 :: Own ()
writeORefExample2 = do
  x <- newORef "Original contents"
  dropORef x
  writeORef x "Some new contents" -- This operation will fail
  return ()

-- This should fail and evaluate to Left String
evalWriteORefExample2 :: IO (Either String ())
evalWriteORefExample2 = startOwn writeORefExample2


-- TODO
-- | borrowORef test of multiple immutable borrows
-- This is an example of multiple functions performing immutable operations on
-- a single ORef

-- TODO
-- | borrowORef test for flag status.
-- Test for borrowORef to show that during the read operation is the ORef is
-- marked as having borrowers (both flags are False)
-- and that after the borrow operation is complete the ORef is marked as not
-- having any borrowers (both flags are True.)

-- TODO
-- | mutableBorrowORef test



-- | Non-conflicting write.
good :: Own (Int,Int)
good = do
  refX <- newORef 3
  refY <- newORef 5
  borrowORef refX (writeORef refY)
  borrowORef refX (\x -> borrowORef refY (\y -> return (x,y)))

evalGood :: IO (Either String (Int, Int))
evalGood = startOwn good

-- | Conflicting write.
bad :: Own (Int,Int)
bad = do
  refX <- newORef 3
  refY <- newORef 5
  borrowORef refX (writeORef refX)
  borrowORef refX (\x -> borrowORef refY (\y -> return (x,y)))

evalBad :: IO (Either String (Int, Int))
evalBad = startOwn bad

-- | Create an ORef and try to mutate it by adding 1 to it
magic :: Int -> IO (Either String Int)
magic x = startOwn $ do
  ref <- newORef x
  borrowORef ref (\a -> return (a + 1))

darkMagic :: Int -> IO (Either String Int)
darkMagic x = startOwn $ do
  ref <- newORef x
  borrowORef ref (\b -> writeORef ref b)
  borrowORef ref (\a -> return (a + 1))


-- | deadlock with normal MVars
--
-- If you run this in ghci, it will usually—but not always—print nothing.
--
-- Adapted from Real World Haskell's example on deadlock with MVar's
-- TODO Cite Chapter 24 of Real World Haskell
nestedModification outer inner = do
  modifyMVar_ outer $ \x -> do
    yield  -- force this thread to temporarily yield the CPU
    modifyMVar_ inner $ \y -> return (y + 1)
    return (x + 1)
  putStrLn "done"

deadlockMVar = do
  a <- newMVar 1
  b <- newMVar 2
  forkIO $ nestedModification a b
  forkIO $ nestedModification b a

-- |  with normal mvars
-- Adapted for ORef from Real World Haskell's example on deadlock with MVar's
-- TODO Cite Chapter 24 of Real World Haskell
nestedModificationORef :: (Typeable a, Num a) => ORef a -> ORef a -> Own ()
nestedModificationORef outer inner = do
  borrowORef' outer $ \x -> do
    liftIO $ yield  -- force this thread to temporarily yield the CPU
    borrowORef' inner $ \y -> return (y + 1)
    return (x + 1)
  liftIO $ putStrLn "done - ORef deadlock example"

deadlockORef :: Own ()
deadlockORef = do
  a <- newORef (1 :: Int)
  b <- newORef 2
  forkOwn $ nestedModificationORef a b
  liftIO $ threadDelay 1000 -- Wait a second just so that the output is printed nicely
  forkOwn $ nestedModificationORef b a
  return ()


main :: IO ()
main = do
  putStrLn "\nStarting ORef Library Tests."

  test1 <- evalDropORefExample1
  case test1 of
    Right () -> putStrLn "Test 1 passed."
    Left _ ->  putStrLn " -- Test 1 failed. -- "

  test2 <- evalDropORefExample2
  case test2 of
    Right _ -> putStrLn " -- Test 2 failed. -- "
    Left _ -> putStrLn "Test 2 passed."

  test3 <- evalCopyORefExample
  case test3 of
    Right () -> putStrLn "Test 3 passed"
    _ -> putStrLn " -- Test 3 failed -- "

  let_test <- evalLetTest
  case let_test of
    Right _ -> putStrLn " -- Let Test failed -- "
    _ -> putStrLn "Let Test passed"

  test4 <- evalMoveORefExample1
  case test4 of
    Right () -> putStrLn "Test 4 passed."
    _ -> putStrLn " -- Test 4 failed. -- "

  test5 <- evalMoveORefExample2
  case test5 of
    Right _ -> putStrLn " -- Test 5 failed. -- "
    Left _ -> putStrLn "Test 5 passed."

  test6 <- evalMoveExample1
  case test6 of
    Right () -> putStrLn "Test 6 passed."
    _ -> putStrLn " -- Test 6 failed. -- "

  test7 <- evalMoveExample2
  case test7 of
    Right _ -> putStrLn " -- Test 7 failed. -- "
    Left _ -> putStrLn "Test 7 passed."

  test8 <- evalBorrowORefExample1
  case test8 of
    Right () -> putStrLn "Test 8 passed."
    _ -> putStrLn " -- Test 8 failed. -- "

  test9 <- evalBorrowORefExample2
  case test9 of
    Right _ -> putStrLn " -- Test 9 failed. -- "
    Left _ -> putStrLn "Test 9 passed."

  test10 <- evalBorrowORefExample3
  case test10 of
    Left _ -> putStrLn "Test 10 passed."
    _ -> putStrLn " -- Test 10 failed. -- "

  test11 <- evalWriteORefExample1
  case test11 of
    Right () -> putStrLn "Test 11 passed"
    _ -> putStrLn " -- Test 11 failed. -- "

  test12 <- evalWriteORefExample2
  case test12 of
    Right _ -> putStrLn " -- Test 12 failed. -- "
    Left _ -> putStrLn "Test 12 passed."

  putStrLn "\nSome more complex examples: "

  g <- evalGood
  putStrLn (show g)

  b <- evalBad
  putStrLn (show b)

  m <- magic 1
  putStrLn (show m)

  -- Run these two in GHCi

  putStrLn "\nDeadlock MVar"
  dlmv <- deadlockMVar
  putStrLn $ show dlmv

  threadDelay 1000

  putStrLn "\nORef example with no deadlock"
  -- this should fail - not with an exception but with a Left String
  dlor <- startOwn deadlockORef :: IO (Either String ())
  threadDelay 1000
  putStrLn $ show dlor
