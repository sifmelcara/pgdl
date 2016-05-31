{-# LANGUAGE OverloadedStrings #-}

module Types
where

import Text.HTML.DirectoryListing.Type
import Data.Text (Text)
import Data.Maybe
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V
import Data.Vector ((!), Vector)

data DNode = Directory Entry (IO [DNode]) | File Entry Text Bool
--                                                          downloaded?

data DList = DList [(Vector Int, Vector DNode)] (Maybe Int)
--                                                ^ selected location of Vector Int

-- | filter the elements in a DList, this action directly modify
-- the state of the top element of directory stack. The purpose of this function is
-- to provide the ability to dynamically filter list
filterDList :: DList -> (DNode -> Bool) -> DList
filterDList (DList ((vIdx, vDn):xs) sel) f = DList ((vIdx', vDn):xs) sel'
    where
    vIdx' = V.filter (\i -> f $ vDn ! i) vIdx
    sel' = case sel of
        Nothing -> Nothing
        Just i -> V.elemIndex (vIdx ! i) vIdx'
        -- ^ keep the focus on the old selected element if it haven't been filtered out

-- | pop the directory stack (used when leaving a directory or exiting a filtered list)
popDList :: DList -> DList 
popDList dl@(DList [_] sel) = dl
popDList (DList (x:xs) sel) = DList xs sel

-- | used when we move to the search (filter) state
dupDList :: DList -> DList
dupDList (DList (x:xs) sel) = DList (x:x:xs) sel

-- | used when we enter a new directory
pushDList :: DList -> [DNode] -> DList
pushDList (DList xs sel) x = DList ((V.enumFromN 0 (length x), V.fromList x):xs) sel

-- | return a brick list which is ready to be rendered
extractDList :: DList -> L.List DNode
extractDList (DList ((vIdx, vDn):xs) _) = L.list "mainList" (V.backpermute vDn vIdx) 3

-- | extract the currently selected DNode
extractSelectedDNode :: DList -> Maybe DNode
extractSelectedDNode (DList ((vIdx, vDn):xs) sel) = (\i -> vDn ! (vIdx ! i)) <$> sel

