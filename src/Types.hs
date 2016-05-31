{-# LANGUAGE OverloadedStrings #-}

module Types
where

import Control.Applicative ((<$>))
import Text.HTML.DirectoryListing.Type
import Data.Text (Text)
import Data.Maybe
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V
import Data.Vector ((!), Vector)

data DNode = Directory Entry (IO [DNode]) | File Entry Text Bool
--                                                          downloaded?

-- | Note: The list in a DList should never be empty
data DList = DList (Vector DNode) [Vector Int] (Maybe Int)
--                 ^ DNode pool                  ^ selected location of Vector Int

newDList :: [DNode] -> DList 
newDList = pushDList (DList V.empty [] Nothing)

-- | filter the elements in a DList, this action directly modify
-- the state of the top element of directory stack. The purpose of this function is
-- to provide the ability to dynamically filter list
filterDList :: DList -> (DNode -> Bool) -> DList
filterDList (DList vDn (vIdx:xs) sel) f = DList vDn (vIdx':xs) sel'
    where
    vIdx' = V.filter (\i -> f $ vDn ! i) vIdx
    sel' = case sel of
        Nothing -> Nothing
        Just i -> V.elemIndex (vIdx ! i) vIdx'
        -- ^ keep the focus on the old selected element if it haven't been filtered out

-- | pop the directory stack (used when leaving a directory or exiting a filtered list)
popDList :: DList -> DList 
popDList dl@(DList _ [_] sel) = dl
popDList (DList vDn (x:xs) sel) = DList vDn xs sel

-- | used when we move to the search (filter) state
dupDList :: DList -> DList
dupDList (DList vDn (x:xs) sel) = DList vDn (x:x:xs) sel

-- | used when we enter a new directory
pushDList :: DList -> [DNode] -> DList
pushDList (DList vDn xs sel) dns = DList vDn' (V.enumFromN (V.length vDn) (length dns):xs) sel
    where
    vDn' = vDn V.++ V.fromList dns

-- | return a brick list which is ready to be rendered
extractDList :: DList -> L.List DNode
extractDList (DList vDn (vIdx:xs) _) = L.list "mainList" (V.backpermute vDn vIdx) 3

-- | extract the currently selected DNode
extractSelectedDNode :: DList -> Maybe DNode
extractSelectedDNode (DList vDn (vIdx:xs) sel) = (vDn!) . (vIdx!) <$> sel

