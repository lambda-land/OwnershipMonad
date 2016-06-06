module Main where

import LibDataMap

-- TODO Map.findMax doesn't work on an empty list

-- Create a program made up of set commands
prog :: Maybe State
prog = runPrg [ SetFreeVar "x" 1
              , SetFreeVar "y" 2
              , SetFreeVar "z" 3
              , SetVarVarMove "a" "z" -- move z to a
              , SetVarVarCopy "b" "a" -- copy a to b
              , SetVarToOp "c" (Add "a" "b") -- Set c to a+b
              , SetBorrowedFrom "d" "c" -- set d as a borrow of c
               ] initialResource

-- Create a program that will fail and simply be Nothing
prog' :: Maybe State
prog' = runPrg [ SetFreeVar "x" 1
               , SetBorrowedFrom "a" "x" -- set "a" as a borrow of "x"
               , SetVarVarMove "b" "x" -- try to move "x" to "b"
               ] initialResource

main :: IO ()
main = putStrLn (show prog)
