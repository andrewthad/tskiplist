-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TBox.TSkipList
-- Copyright   :  Peter Robinson 2010-2013
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <thaldyron@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- This module provides an implementation of a skip list in the 'STM' monad.
-- The elements of the skip list are stored in a 'TVar'.
--
-- A skip list is a probabilistic data structure with dictionary operations
-- (similar to 'Data.Map').
-- In contrast to a balanced tree, a skip list does not need any 
-- (expensive) rebalancing operation, which makes it particularly suitable 
-- for concurrent programming. 
-- 
-- See: /William Pugh. Skip Lists: A Probabilistic Alternative to Balanced Trees./
--
-- This module should be imported qualified.
--
-- /Example (GHCi):/ 
--
-- > t <- newIO :: IO (TSkipList Int String) 
-- > atomically $ sequence_ [ insert i (show i) t | i <- [1..10] ]
-- >
-- > putStrLn =<< atomically (toString t)
-- > 9
-- > 9
-- > 3 7 9
-- > 1 3 7 9
-- > 1 2 3 4 5 6 7 8 9 10
-- >
-- > atomically $ delete  7 t
-- > putStrLn =<< atomically (toString t)
-- > 9
-- > 9
-- > 3 9
-- > 1 3 9
-- > 1 2 3 4 5 6 8 9 10
-- > 
-- > atomically $ sequence [ lookup i t | i <- [5..10] ]
-- > [Just "5",Just "6",Nothing,Just "8",Just "9",Just "10"]
-- >
-- > atomically $ update 8 "X" t
-- > atomically $ filterRange (5,10) t 
-- > ["5","6","X","9","10"]
-- >
-- > atomically $ maximum t
-- > (10,"10")
-----------------------------------------------------------------------------
module Control.Concurrent.STM.TSkipList(-- * Data Type and Construction
                                         TSkipList,newIO,newIO',new,new',
                                         -- * Operations
                                         insert,lookup,update,delete,
                                         filter,filterGT,filterLT,filterElems,filterRange,
                                         minimum, maximum,
                                         -- * Utilities 
                                         chooseLevel,
                                         toString,
                                       ) 
where
import Control.Concurrent.STM.TSkipList.Internal
import Prelude hiding(filter,lookup,minimum,maximum)


