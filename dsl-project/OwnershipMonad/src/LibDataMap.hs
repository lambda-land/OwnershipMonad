module LibDataMap where

import qualified Data.Map as Map hiding (filter)


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
--  ---------------   ----------------------
--  | Var  |  Id  |   |  Id  |   Resource  |
--  ---------------   ----------------------
--
--  The Id is used here to link a variable name string with a resource
--
type State = (Map.Map Var Id, Map.Map Id Resource)

-- | SetCmd represents the ways that variable may be bound to resources
data SetCmd = SetFreeVar    Var Int
            | SetVarVarMove Var Var     -- Move the resource from one variable to another variable
            | SetVarVarCopy Var Var     -- Copy the resource from owned by one variable to another variable
            | SetVarToOp    Var Op      -- Bind the resource to the result of performing an operation
            | SetBorrowedFrom Var Var   -- Set a variable as a borrow of another variable
            | Drop Var                  -- Drop a variable and either abandon its resource or remove its borrow

data Op = Add Var Var
-- TODO add recurisive Ops eventually


-- | Function to check if a resource is being borrowed
isResBorrowed :: Id -> State -> Maybe Bool
isResBorrowed varId (_, resMap) = case Map.lookup varId resMap of
                                   Nothing -> Nothing -- That resource doen't exist
                                   Just (_, Borrowed _ _) -> Just True
                                   Just (_, _) -> Just False

-- | check whether the Variable's associated resource is being borrowed
isVarBorrowed :: Var -> State -> Maybe Bool
isVarBorrowed var (varMap, resMap) = case Map.lookup var varMap of
                                    Nothing -> Nothing -- False because the variable does't exists
                                    Just varId -> isResBorrowed varId (varMap, resMap)

-- Get the list of variables that are borrowing the resource
getBorrowers :: Id -> State -> Maybe [Var]
getBorrowers varId (_, resMap) = case Map.lookup varId resMap of
                                    Just (_, Borrowed _ borrowersList) -> Just borrowersList
                                    _ -> Nothing -- That resource doen't exist or isn't being borrowed

-- Get the list of variables that are borrowing the resource
getOwner :: Id -> State -> Maybe Var
getOwner varId (_, resMap) = case Map.lookup varId resMap of
                                    Just (_, Owned v) -> Just v
                                    Just (_, Borrowed owner _) -> Just owner
                                    _ -> Nothing -- That resource doen't exist or isn't being borrowed

-- | Check if a variable is a borrower
isABorrower :: Var -> State -> Maybe Bool
isABorrower var (varMap, resMap)= do
  varId <- Map.lookup var varMap -- if the variable doesn't exist return Nothing
  res <- Map.lookup varId resMap -- make sure the resource exists
  isBorrowed <- isResBorrowed varId (varMap, resMap)
  if isBorrowed
    then do
           borrowersList <- getBorrowers varId (varMap, resMap)
           return (elem var borrowersList)
    else return False -- The resource isn't borrowed so the variable can't be a borrower borrowering it


-- | Check if a variable has borrowers
hasBorrowers :: Var -> State -> Maybe Bool
hasBorrowers var (varMap, resMap)= do
  varId <- Map.lookup var varMap -- if the variable doesn't exist return Nothing
  res <- Map.lookup varId resMap -- make sure the resource exists
  isBorrowed <- isResBorrowed varId (varMap, resMap)
  owner <- getOwner varId (varMap, resMap)
  return (isBorrowed && owner == var) -- if the variable is the owner of a borrowered res it has borrowers


-- | move
--
-- A variable is taking ownership of the resource that another var owns
-- (if it is not being borrowed)
-- and abandoning it's own resource which has it's value set to 0
-- We maintain the abandoned resource's Id however because new Id's
-- are created by taking the max id in the map and adding one to it.
-- Removing Id's could mess this up.
--
--
move :: Var -> Id -> Var -> State -> Maybe State
move taker abandonedId owner (varMap, resMap) = do
 ownerId <- Map.lookup owner varMap
 _ <- Map.lookup ownerId resMap -- make sure the ownerId has an associated resource
 isBorrowed <- isResBorrowed ownerId (varMap, resMap)
 isTakerBorrowed <- isResBorrowed abandonedId (varMap, resMap)
 if isBorrowed || isTakerBorrowed
   then Nothing
   else do
          (val, Owned _) <- Map.lookup ownerId resMap -- we need to get the value
          let resMapAfterAbandonment = Map.insert abandonedId (0, Abandoned) resMap
          let finalResMap = Map.insert ownerId (val, Owned taker) resMapAfterAbandonment
          let finalVarMap = Map.delete owner (Map.insert taker ownerId varMap)
          return (finalVarMap, finalResMap)


-- | Run an operation and return the result
runOp :: Op -> State -> Maybe Int
runOp (Add v1 v2) (varMap, resMap) = do
  v1Id <- Map.lookup v1 varMap
  v2Id <- Map.lookup v2 varMap
  (v1Val, v1Status) <- Map.lookup v1Id resMap
  (v2Val, v2Status) <- Map.lookup v2Id resMap
  case (v1Status, v2Status) of
    (Owned _, Owned _) -> return (v1Val + v2Val) -- the value returned by the operation
    _ -> Nothing -- The values are being borrowed -- TODO if we aren't modifying them why does this matter? this probably shouldn't return Nothing for this case


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
                      owner <- getOwner varId (varMap, resMap)
                      if isBorrowed
                        then if (var == owner)
                                then Nothing   -- Owner can't change resource while it is borrowed
                                else do
                                        borrowerList <- getBorrowers varId (varMap, resMap)
                                        if length borrowerList == 1 --then the borrower can update
                                          then return (varMap, Map.insert varId (int, Borrowed owner borrowerList) resMap)
                                          else Nothing -- can't update the resource if there are more than one borrowers
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
         isNewVarBorrowed <- isResBorrowed newVarId (varMap, resMap) -- check if the new variables resource is being borrowed
         if isNewVarBorrowed
           then Nothing
           else do
                 move newVar newVarId oldVar (varMap, resMap) -- The oldVar does exists and the new variable is not being borrowed so run the move function

runCmd (SetVarVarCopy newVar oldVar) (varMap, resMap) =
    case Map.lookup newVar varMap of -- see if the new variable already exists or not
      Nothing -> do
        oldVarId <- Map.lookup oldVar varMap -- the new variable does not exist so lookup the old variable's id
        (oldVarValue, _) <- Map.lookup oldVarId resMap -- the old var exists so lookup it's resource
        let (maxK, _) = Map.findMax resMap -- the new id is the max id plus one   TODO check this let in
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
      return (Map.insert var (maxK + 1) varMap, Map.insert (maxK + 1) (opVal, Owned var) resMap) -- insert the result of running the operation TODO should we exclude var from the state we pass in here?
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
    Just borrowerVId -> do      -- The borrower variable exist
      isWannabeBorrowerBeingBorrowed <- isResBorrowed borrowerVId (varMap, resMap) -- check if the new variable's (the one that wants to become a borrower) resource is being borrowed
      if isWannabeBorrowerBeingBorrowed
        then Nothing  -- if the variable already existed and it was being borrowed then it can't change
        else do
              ownerVId <- Map.lookup ownerV varMap
              (val, status) <- Map.lookup ownerVId resMap
              let abandonedRes = (0, Abandoned)
              let resMapAfterAbandonment = (Map.insert borrowerVId abandonedRes resMap) -- abandon the resource previously associated with the borrower
              case status of
                Owned v -> return (Map.insert borrowerV ownerVId varMap, Map.insert ownerVId (val, Borrowed v [borrowerV]) resMapAfterAbandonment)
                Borrowed v borrowerList -> return (Map.insert borrowerV ownerVId varMap, Map.insert ownerVId (val, Borrowed v (borrowerList ++ [borrowerV])) resMapAfterAbandonment)
                _ -> Nothing

runCmd (Drop var) (varMap, resMap) = do
  isBorrowing <- isABorrower var (varMap, resMap) -- check if the varible to be dropped is borrowing a resource
  varId <- Map.lookup var varMap -- get the variable to be dropped's id
  if isBorrowing
    then do   -- The variable is a borrower
          borrowerList <- getBorrowers varId (varMap, resMap) -- get the borrower list for this borrowed resource
          let newBorrowers = (filter (/= var) borrowerList) -- borrower list with the dropped variable removed
          (i, Borrowed o _) <- Map.lookup varId resMap -- get the value stored in the resource and the owner
          case newBorrowers of
            [] -> return (Map.delete var varMap, Map.insert varId (i, Owned o) resMap) -- insert the new resource (sans the borrower we removed) and since there was only one borrower, and it was dropped, the resource becomes owned
            _  -> return (Map.delete var varMap, Map.insert varId (i, Borrowed o newBorrowers) resMap) -- insert the new resource (sans the borrower we removed)
    else do
         hasBorrowers <- isVarBorrowed var (varMap, resMap) -- See if the variable's resource has any borrowers
         if hasBorrowers
           then Nothing -- Can't drop a variable that is being borrowed
           else return (Map.delete var varMap, Map.insert varId (0, Abandoned) resMap) -- The variable is not a borrower so just remove it and abandon its resource


-- | Run a program
runPrg :: [SetCmd] -> State -> Maybe State
runPrg [] st = Just st
runPrg (c:cs) st = do
  st' <- runCmd c st
  runPrg cs st'


-- | Just for creating some initial resource with one thing in it
initialResource :: State
initialResource = (Map.fromList [("foo", 1)], Map.fromList [(1, (42, Owned "foo"))])
