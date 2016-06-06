module Lib where

import Prelude hiding (lookup)
import Control.Monad.ST
import Data.HashTable.ST.Basic
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- A variable name is a string
type Var = String

-- The status of a resource
data Status = Free
            | Abandoned
            | Owned Var          -- Owned
            | Borrowed Var [Var] -- Lent out from the Owner Var to another Var (that would lend it out to the next Var in the list)
            deriving (Eq, Show)


type Id = Int

type Resource = (Int, Status)

-- | The state type represents the resources available
--
--  -------------------------------------
--  | res0   |  Int   |  Var  |  Status |
--  -------------------------------------
--  | res1   |  Int   |  Var  |  Status |
--  -------------------------------------
--  | res2   |  Int   |  Var  |  Status |
--  -------------------------------------
--  | res3   |  Int   |  Var  |  Status |
--  -------------------------------------
--  | res4   |  Int   |  Var  |  Status |
--  -------------------------------------
--
--  The Id is used here to link a variable name string with a resource
type State s = (HashTable s Var Id, HashTable s Id Resource)

-- | SetCmd represents the ways that variable may be bound to resources
data SetCmd = SetFreeVar    Var Int
            | SetVarVarMove Var Var -- Move the resource from one variable to another variable
            | SetVarVarCopy Var Var -- Copy the resource from owned by one variable to another variable
            | SetVarToOp    Var Op  -- Bind the resource to the result of performing an operation
            | SetBorrowedFrom Var Var -- Set a variable as a borrow of another variable

data Op = Add Var Var

-- | Function to check if a resource is being borrowed
isResBorrowed :: Id -> State s -> ST s (Maybe Bool)
isResBorrowed varId (_, resMap) = do
  res <- lookup resMap varId
  case res of
    Nothing -> return Nothing -- That resource doen't exist
    Just (_, Borrowed _ _) -> return (Just True)
    Just (_, _) -> return (Just False)

-- | check whether the Variable's associated resource is being borrowed
isVarBorrowed :: Var -> State s -> ST s (Maybe Bool)
isVarBorrowed var (varMap, resMap) = do
  varId <- lookup varMap var
  case varId of
    Nothing -> return Nothing -- False because the variable does't exists
    Just varId -> isResBorrowed varId (varMap, resMap)

-- Get the list of variables that are borrowing the resource
getBorrowers :: Id -> State s -> ST s (Maybe [Var])
getBorrowers varId (_, resMap) = do
  res <- lookup resMap varId
  case res of
    Just (_, Borrowed _ borrowersList) -> return (Just borrowersList)
    _ -> return Nothing -- That resource doen't exist or isn't being borrowed

-- | move
--
-- A variable is taking ownership of the resource that another var owns
-- (if it is not being borrowed)
-- and abandoning it's own resource which has it's value set to 0
-- We maintain the abandoned resource's Id however because new Id's
-- are created by taking the max id in the map and adding one to it.
-- Removing Id's could mess this up.
--
move :: Var -> Id -> Var -> State s -> MaybeT (ST s) (Maybe (State s))
move taker abandonedId owner (varMap, resMap) = do
 ownerId <- lift (lookup varMap owner)
 case ownerId of
   Nothing -> return Nothing
   Just oId -> do
     _ <- lift (lookup resMap oId) -- make sure the ownerId has an associated resource
     isBorrowed <- lift (isResBorrowed oId (varMap, resMap))
     case isBorrowed of
       Nothing -> return Nothing
       Just True -> return Nothing
       Just False -> do
         vm0 <- lift (insert varMap taker oId)
         rm <- lift (insert resMap abandonedId (0, Abandoned))
--         (vm1, rm) <- (lift (delete vm0 owner), rm)
         return (Just (vm0, rm))
--         return (Just (varMap, resMap))


{-
-- | Run an operation and return the result
runOp :: Op -> State -> Maybe Int
runOp (Add v1 v2) (varMap, resMap) = do
  v1Id <- Map.lookup v1 varMap
  v2Id <- Map.lookup v2 varMap
  (v1Val, v1Status) <- Map.lookup v1Id resMap
  (v2Val, v2Status) <- Map.lookup v2Id resMap
  case (v1Status, v2Status) of
    (Owned _, Owned _) -> return (v1Val + v2Val) -- the value returned by the operation
    _ -> Nothing -- The values are being borrowed TODO if we aren't modifying them why does this matter, this probably shouldn't return Nothing for this case

-- TODO create recursive operation



-- | Run the Set Cmd on the given state
--
-- For the case
-- `runCmd (SetVarVarMove newVar oldVar) (varMap, resMap) = ... `
--
-- In the case of
-- `Just newVarId -> Nothing`
--
-- When the variable that the resource is being moved to already exists the following occurs:
--       A resource `y` that already exists and owns a resource may take ownership of another variable `x`s resource
--       That resource `x` must be removed from the variable tracking mapping of (Map Var Id)
--       and the other variable `y` will abandon it's old resource ID and take on the resource id that the other resource `x` had.
--       The resource that is no longer being used will continue to exists since new resources are created by looking at the maximun id that exists and then adding one.
--       Its status is set to `Abandoned` instead of `Owned` and if there were a garbage collector it could handle free up the memory that this resource had. 
--
-- A similar example in Rust can be found here:
-- https://play.rust-lang.org/?gist=81c7011458c850d5b8da056260ec491c&version=stable&backtrace=0
-- and for just the previous in Gist form
-- https://gist.github.com/anonymous/81c7011458c850d5b8da056260ec491c
-- 
-- TODO each of the cases that runCmd matches against could really become a function unto themselves
--
runCmd :: SetCmd -> State -> Maybe State
runCmd (SetFreeVar var int) (varMap, resMap) =
    case Map.lookup var varMap of
      Nothing -> let (maxK, _) = Map.findMax resMap -- if the variable is completely new then find the max key, add one, that's the new var's id
                  in Just (Map.insert var (maxK + 1) varMap, Map.insert (maxK + 1) (int, Owned var) resMap)
      Just varId -> do
                      isBorrowed <- isVarBorrowed var (varMap, resMap)
                      if isBorrowed
                        then Nothing  -- Can't change it while it's borrowed
                        else return (varMap, Map.insert varId (int, Owned var) resMap)  -- set the existing var to the new value (by updating the value in the resMap)

runCmd (SetVarVarMove newVar oldVar) (varMap, resMap) =
    case Map.lookup newVar varMap of -- Lookup the new variable to make sure it doesn't already exist
       Nothing -> do
         oldVarId <- Map.lookup oldVar varMap   -- The new variable doesn't exists so we can go ahead and lookup the old variable's id
         isBorrowed <- isResBorrowed oldVarId (varMap, resMap) -- The old variable's resource exists so check if it's borrowed
         if isBorrowed
           then Nothing
           else return (Map.insert newVar oldVarId (Map.delete oldVar varMap), resMap) -- delete the old variable from the varMap and insert the new variable
       Just newVarId -> do    -- The new variable already exists. What will happen in this case is in the runCmd's documentation
         _ <- Map.lookup oldVar varMap -- see if the oldVarId exists
         move newVar newVarId oldVar (varMap, resMap) -- The oldVar does exists so run the move function

runCmd (SetVarVarCopy newVar oldVar) (varMap, resMap) =
    case Map.lookup newVar varMap of -- see if the new variable already exists or not
      Nothing -> do
        oldVarId <- Map.lookup oldVar varMap -- the new variable does not exist so lookup the old variable's id
        (oldVarValue, _) <- Map.lookup oldVarId resMap -- the old var exists so lookup it's resource
        let (maxK, _) = Map.findMax resMap -- the new id is the max id plus one TODO check this let in 
        return (Map.insert newVar (maxK + 1) varMap, Map.insert (maxK + 1) (oldVarValue, Owned newVar) resMap)
      Just newVarId -> do
        isNewVarBorrowed <- isResBorrowed newVarId (varMap, resMap) -- check if the variables resource exists and if being borrowed
        if isNewVarBorrowed
          then Nothing
          else do
                 _ <- Map.lookup newVarId resMap -- see if the new variable has a resource
                 oldVarId <- Map.lookup oldVar varMap -- get the old variable's id
                 (oldVarValue, _) <- Map.lookup oldVarId resMap -- get the old variable's resource
                 isOldVarBorrowed <- isResBorrowed oldVarId (varMap, resMap)
                 if isOldVarBorrowed
                   then Nothing
                   else return (varMap, Map.insert newVarId (oldVarValue, Owned newVar) resMap)

runCmd (SetVarToOp var op) (varMap, resMap) =
  case Map.lookup var varMap of    -- first check if the var we are going to store the results in exists
    Nothing -> do
      opVal <- runOp op (varMap, resMap)
      let (maxK, _) = Map.findMax resMap   -- the new id is the max id plus one
      return (Map.insert var (maxK + 1) varMap, Map.insert (maxK + 1) (opVal, Owned var) resMap) -- insert the result of running the operation TODO should we exclude var from the state we pass in here 
    Just varId -> do
      isBorrowed <- isResBorrowed varId (varMap, resMap)  -- Check for borrowers of the variable since it already exists
      if isBorrowed
        then Nothing -- Can't change the var while there are borrowers
        else do
              opVal <- runOp op (varMap, resMap)
              return (Map.insert var varId varMap, Map.insert varId (opVal, Owned var) resMap) -- insert the result of running the operation

runCmd (SetBorrowedFrom borrowerV ownerV) (varMap, resMap) =
  case Map.lookup borrowerV varMap of -- Does the borrower exists or not
    Nothing -> do  -- the borrower variable does not exist
      ownerVId <- Map.lookup ownerV varMap
      (val, status) <- Map.lookup ownerVId resMap
      case status of
        Owned v -> return (Map.insert borrowerV ownerVId varMap, Map.insert ownerVId (val, Borrowed v [borrowerV]) resMap)
        Borrowed v borrowerList -> return (Map.insert borrowerV ownerVId varMap, Map.insert ownerVId (val, Borrowed v (borrowerList ++ [borrowerV])) resMap)
        _ -> Nothing
    Just borrowerVId -> do -- The borrower variable does exist
      ownerVId <- Map.lookup ownerV varMap
      (val, status) <- Map.lookup ownerVId resMap
      let abandonedRes = (0, Abandoned)
      let resMapAfterAbandonment = (Map.insert borrowerVId abandonedRes resMap) -- abandon the resource previously associated with the borrower
      case status of
        Owned v -> return (Map.insert borrowerV ownerVId varMap, Map.insert ownerVId (val, Borrowed v [borrowerV]) resMapAfterAbandonment)
        Borrowed v borrowerList -> return (Map.insert borrowerV ownerVId varMap, Map.insert ownerVId (val, Borrowed v (borrowerList ++ [borrowerV])) resMapAfterAbandonment)
        _ -> Nothing

-}
