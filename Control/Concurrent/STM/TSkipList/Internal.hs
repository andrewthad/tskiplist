-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TBox.TSkipList.Internal
-- Copyright   :  Peter Robinson 2010-2013
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <thaldyron@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-----------------------------------------------------------------------------
module Control.Concurrent.STM.TSkipList.Internal(-- * Data types and Construction
                                         TSkipList(..),newIO,newIO',new,new',
                                         Traversal(..),ForwardPtrs,Node(..),
                                         -- * Operations
                                         insert,lookup,update,delete,
                                         filter,filterGT,filterLT,filterElems,
                                         minimum, maximum, filterRange,
                                         -- * Low-level Operations
                                         traverse, lookupNode, insertNode,
                                         -- * Utilities 
                                         chooseLevel,
                                         toString,
                                       ) 
where
import Data.Maybe
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Control.Exception
import System.IO.Unsafe

import System.Random
import Data.Array.MArray
import Data.Map(Map)
import qualified Data.Map as M
import Prelude hiding(filter,lookup,minimum,maximum)

type ForwardPtrs k a = TArray Int (Node k a)

-- | A skip list data type.
data TSkipList k a = TSkipList 
  { maxLevel    :: Int     -- ^ The maximal height of the skip list. 
  , probability :: Float   -- ^ Probability parameter.
  , curLevel    :: TVar Int  -- ^ The current max level.
  , listHead    :: ForwardPtrs k a -- ^ Pointer to the first element in the highest level.
  }

-- | An entry of the skip list.
data Node k a
  = Nil 
  | Node { key          :: k 
         , contentTVar  :: TVar a 
         , forwardPtrs  :: ForwardPtrs k a
         }

{-
newNode :: k -> TVar a -> Int -> STM (Node k a)
newNode k t maxLvl = Node k t `liftM` (newForwardPtrs maxLvl)
-}

-- | True iff the node is nonempty
isNil :: Node k a -> Bool
isNil Nil = True
isNil _   = False

-- | Creates a skiplist.
newIO' :: Float  -- ^ Probability for choosing a new level
       -> Int    -- ^ Maximum number of levels. 
       -> IO (TSkipList k a)
newIO' p maxLvl = atomically $ new' p maxLvl

-- | Creates a skiplist. Default values for storing up to 2^16 elements. 
newIO :: IO (TSkipList k a)
newIO = newIO' 0.5 16



-- | Creates a skiplist.
new' :: Float  -- ^ Probability for choosing a new level
     -> Int    -- ^ Maximum number of levels
     -> STM (TSkipList k a)
new' p maxLvl = 
  TSkipList maxLvl p `liftM` newTVar 1 
                        `ap` newForwardPtrs maxLvl

-- | Creates a skiplist. Default values for storing up to 2^16 elements. 
new :: STM (TSkipList k a)
new = new' 0.5 16


newForwardPtrs :: Int -> STM (ForwardPtrs k a)
newForwardPtrs maxLvl = newArray (1,maxLvl) Nil
  -- newListArray (1,maxLvl) $ replicate maxLvl Nil


-- | Returns a randomly chosen level. Is used for inserting new elements.
-- For performance reasons, this function uses 'unsafePerformIO' to access the
-- random number generator. (It would be possible to store the random number
-- generator in a 'TVar' and thus be able to access it safely from within the
-- STM monad. This, however, might cause high contention among threads.)
chooseLevel :: TSkipList k a -> Int
chooseLevel tskip = 
  min (maxLevel tskip) $  1 + truncate (log x / log (probability tskip))
  where x = fst $ randomR (0.0, 1.0) (unsafePerformIO newStdGen)


{-
chooseLevel :: TSkipList k a -> STM Int
chooseLevel tskip = do
  stdG <- unsafeIOToSTM newStdGen
  let rs :: StdGen -> [Float]
      rs g = x : rs g' where (x,g') = randomR (0,1) g
  let samples =  take (maxLevel tskip - 1) (rs stdG) 
  return $ 1 + length (takeWhile (probability tskip <) samples) 
-}

{-
chooseLevel tskip = do
  stdG <- unsafeIOToSTM newStdGen
  let rs :: StdGen -> [(Float,StdGen)]
      rs g = (x,g') : rs g' where (x,g') = randomR (0,1) g
  let (samples,newStdGs) = unzip $ take (maxLevel tskip) (rs stdG) 
  return $ 1 + length (takeWhile ((<) (probability tskip)) $ take (maxLevel tskip - 1) samples)
-}


-- | Returns all elements less than the key. Takes /O(m)/ where /m/ is the
-- number of elements that have a smaller key.
filterLT :: (Ord k) => k -> TSkipList k a -> STM (Map k a)
filterLT k tskip = 
  leqAcc (listHead tskip) 1 M.empty
  where
  leqAcc fwdPtrs lvl curAcc = do
    let moveDown acc _ level  = 
          leqAcc fwdPtrs (level-1) acc 
    let moveRight acc succNode level = 
          addElem acc succNode >>=
            leqAcc (forwardPtrs succNode) level 
    let onFound acc _ _ = 
          return acc
    traverse k fwdPtrs lvl Traversal{ onLT  = moveDown curAcc 
                                    , onGT  = moveRight curAcc
                                    , onEQ  = onFound curAcc
                                    , onNil = moveDown curAcc
                                    , onSuccSuccNil = Nothing
                                    }
                           curAcc

  addElem acc succNode = do 
    a <- readTVar (contentTVar succNode)
    return $ M.insert (key succNode) a acc 


-- | Returns all elements greater than the key.
-- TODO: currently in O(n), should be made more efficient (like 'leq')
filterGT :: (Ord k) => k -> TSkipList k a -> STM (Map k a)
filterGT k = filter ((<) k)



-- | Finds all elements within a specific key range (k1,k2). /O(log n + k2 - k1)/.
filterRange :: (Ord k) => (k,k) -> TSkipList k a -> STM [(k,a)]
filterRange (k1,k2) tskip = do
  mnode <- lookupNode k1 tskip 
  case mnode of
    Nothing   -> return []
    Just node -> do 
      a <- readTVar (contentTVar node) 
      leqAcc (forwardPtrs node) 1 [(k1,a)]
  where
  leqAcc fwdPtrs lvl curAcc = do
    let onFound acc succNode level = 
          addElem acc succNode >>=
            leqAcc (forwardPtrs succNode) level 
    let travStrategy = Traversal { onLT = \_ _ -> return $ reverse curAcc
                                 , onGT = onFound curAcc
                                 , onEQ = onFound curAcc
                                 , onNil = \_ _ -> return $ reverse curAcc
                                 , onSuccSuccNil = Nothing
                                 }
    traverse k2 fwdPtrs lvl travStrategy curAcc

  addElem acc succNode = do 
    a <- readTVar (contentTVar succNode)
    return ((key succNode,a):acc )

      
      
-- | Returns the minimum entry. /O(1)/.
minimum :: (Ord k) => TSkipList k a -> STM (k,a)
minimum tskip = do
  succNode <- readArray (listHead tskip) 1
  case succNode of
    Nil -> throw $ AssertionFailed "TSkipList.min: skip list is empty!"
    Node k tvar _ -> do 
      a <- readTVar tvar 
      return (k,a)



-- | Returns the maximum entry. /O(log n)/.
maximum :: (Ord k) => TSkipList k a -> STM (k,a)
maximum tskip = do
  curLvl <- readTVar (curLevel tskip)
  curNode <- readArray (listHead tskip) curLvl
  if isNil curNode then
    throw $ AssertionFailed "TSkipList.maximum: empty skip list!"
  else do
    a <- readTVar $ contentTVar curNode
    maximumTraverse (key curNode,a) (listHead tskip) curLvl
    where
    maximumTraverse b@(fstKey,_) fwdPtrs lvl = do
      let moveRight succNode level = maximumTraverse b (forwardPtrs succNode) level
      let endOfLevel succNode level 
              | level > 1 = maximumTraverse b fwdPtrs (lvl-1)
              | otherwise = do 
                  a <- readTVar $ contentTVar succNode
                  return (key succNode,a)
      let travStrategy = Traversal { onLT = moveRight
                                   , onGT = moveRight 
                                   , onEQ = moveRight
                                   , onNil = throw $ AssertionFailed "TSkipList.maximum: empty skip list!"
                                   , onSuccSuccNil = Just endOfLevel 
                                   }
      traverse fstKey fwdPtrs lvl travStrategy b



lookupNode :: (Ord k) => k -> TSkipList k a -> STM (Maybe (Node k a))
lookupNode k tskip = 
  lookupAcc (listHead tskip) =<< readTVar (curLevel tskip)
  where
  lookupAcc fwdPtrs lvl = do
    let moveDown _ level  = lookupAcc fwdPtrs (level-1)
    let moveRight succNode = lookupAcc (forwardPtrs succNode) 
    let onFound succNode _   = return (Just succNode)
    let travStrategy = Traversal{ onLT = moveDown
                                , onGT = moveRight
                                , onEQ = onFound
                                , onNil = moveDown
                                , onSuccSuccNil = Nothing
                                }
    traverse k fwdPtrs lvl travStrategy Nothing

-- | Searches for a given entry. /O(log n)/.
lookup :: (Ord k) => k -> TSkipList k a -> STM (Maybe a)
lookup k tskip = 
    maybe (return Nothing)
          (\n -> Just <$> readTVar (contentTVar n)) =<< lookupNode k tskip 


-- | Updates an element. Throws 'AssertionFailed' if the element is not in the
-- list. /O(log n)/.
update :: (Ord k) => k -> a -> TSkipList k a -> STM ()
update k a tskip = 
  maybe (throw $ AssertionFailed "TSkipList.update: element not found!") 
        (flip writeTVar a . contentTVar) =<< lookupNode k tskip 


-- | Deletes an element. Does nothing if the element is not found. /O(log n)/.
delete :: (Ord k) => k -> TSkipList k a -> STM ()
delete k tskip = 
  deleteAcc (listHead tskip) =<<  readTVar (curLevel tskip)
  where
  deleteAcc fwdPtrs lvl = do
    let moveDown _ level       = deleteAcc fwdPtrs (level-1)
    let moveRight succNode     = deleteAcc (forwardPtrs succNode) 
    let onFound succNode level = do
          succsuccNode <- readArray (forwardPtrs succNode) level 
          writeArray fwdPtrs level succsuccNode
          moveDown succNode level
    let travStrategy = Traversal{ onLT = moveDown
                                , onGT = moveRight
                                , onEQ = onFound
                                , onNil = moveDown
                                , onSuccSuccNil = Just moveRight
                                }
    traverse k fwdPtrs lvl travStrategy ()


-- | Inserts/updates the value for a specific key.  /O(log n)/.
insert :: (Ord k) => k -> a -> TSkipList k a ->  STM ()
insert k a tskip = do
  mNode <- lookupNode k tskip 
  case mNode of
    Just node -> writeTVar (contentTVar node) a
    Nothing   -> do
      tvar    <- newTVar a 
      newPtrs <- newForwardPtrs (maxLevel tskip)
      let node = Node k tvar newPtrs
      insertNode k node tskip


insertNode :: (Ord k) => k -> Node k a -> TSkipList k a ->  STM ()
insertNode k node tskip = do
  let newLevel =  chooseLevel tskip
  -- Adapt current maximum level if necesary:
  curLvl   <- readTVar (curLevel tskip)
  when (curLvl < newLevel) $ 
    writeTVar (curLevel tskip) newLevel
  insertAcc (listHead tskip) newLevel
  where
  insertAcc fwdPtrs lvl = do
    let moveDown succNode level = do 
          writeArray (forwardPtrs node) level succNode
          writeArray fwdPtrs level node
          insertAcc fwdPtrs (level-1)
    let moveRight succNode = 
          insertAcc (forwardPtrs succNode) 
    let onFound _ _ = throw $ AssertionFailed "TSkipList.insertNode: internal error"{- do
          writeArray fwdPtrs level node
          insertAcc fwdPtrs (level-1)
     -}
    let travStrategy = Traversal{ onLT = moveDown
                                , onGT = moveRight
                                , onEQ = onFound
                                , onNil = moveDown
                                , onSuccSuccNil = Nothing
                                }
    traverse k fwdPtrs lvl travStrategy ()


-- | Traversal strategy. 
data Traversal k a b = Traversal 
  { onLT  :: Node k a -> Int -> STM b
  , onGT  :: Node k a -> Int -> STM b
  , onEQ  :: Node k a -> Int -> STM b
  , onNil :: Node k a -> Int -> STM b
  , onSuccSuccNil :: Maybe (Node k a -> Int -> STM b)
  }

traverse :: (Ord k) 
         => k                -- ^ search key
         -> ForwardPtrs k a  -- ^ forward pointers to the next node
         -> Int              -- ^ current level
         -> Traversal k a b  -- ^ traversal strategy
         -> b                -- ^ default value
         -> STM b
traverse k fwdPtrs level traversal def
  | level < 1 = return def
  | otherwise = do
    succNode <- readArray fwdPtrs level 
    if isNil succNode then
      onNil traversal succNode level
    else do
      succsuccNode <- readArray (forwardPtrs succNode) level
      if isNil succsuccNode && isJust (onSuccSuccNil traversal) then
          (fromJust $ onSuccSuccNil traversal) succNode level
        else case k `compare` key succNode of
             GT -> onGT traversal succNode level
             LT -> onLT traversal succNode level
             EQ -> onEQ traversal succNode level

-- | Returns all elements that satisfy the predicate on keys. /O(n)/.
filter :: (Ord k) 
      => (k -> Bool) -> TSkipList k a -> STM (Map k a)
filter pr tskip = 
  filterAcc (listHead tskip) 1 M.empty
  where
  filterAcc fwdPtrs level acc = do
    succNode <- readArray fwdPtrs level 
    if isNil succNode 
      then return acc
      else do
        newAcc <- addElem acc succNode
        filterAcc (forwardPtrs succNode) level newAcc

  addElem acc succNode = do
    a <- readTVar (contentTVar succNode) 
    return $ if pr (key succNode)
              then M.insert (key succNode) a acc
              else acc 



-- | Returns all elements that satisfy the predicate on keys and values. /O(n)/.
filterElems :: (Ord k) 
      => (k -> a -> Bool) -> TSkipList k a -> STM (Map k a)
filterElems pr tskip = 
  filterAcc (listHead tskip) 1 M.empty
  where
  filterAcc fwdPtrs level acc = do
    succNode <- readArray fwdPtrs level 
    if isNil succNode 
      then return acc
      else do
        newAcc <- addElem acc succNode
        filterAcc (forwardPtrs succNode) level newAcc

  addElem acc succNode = do
    a <- readTVar (contentTVar succNode) 
    return $ if pr (key succNode) a 
              then M.insert (key succNode) a acc
              else acc 


-- | Returns the skip list as a string.
toString :: (Show k,Ord k) => TSkipList k a -> STM String
toString tskip = do
  maxKey <- fst <$> maximum tskip
  curLvl   <- readTVar (curLevel tskip)
  ls <- forM (reverse [1..curLvl]) $ printAcc maxKey (listHead tskip) []
  return $ unlines ls
  where
  printAcc maxKey fwdPtrs acc curLvl = do
    let moveDown succNode level = 
          if (isNil succNode) 
            then return acc
            else printAcc maxKey (forwardPtrs succNode) acc level
    let moveRight succNode level = do
          let n = (' ':show (key succNode))
          printAcc maxKey (forwardPtrs succNode) (acc++n) level
    let onFound succNode level = do
          let n = (' ':show (key succNode))
          printAcc maxKey (forwardPtrs succNode) (acc++n) level
    let travStrategy = Traversal{ onLT = moveDown
                                , onGT = moveRight
                                , onEQ = onFound
                                , onNil = moveDown
                                , onSuccSuccNil = Nothing
                                }
    traverse maxKey fwdPtrs curLvl travStrategy ""

