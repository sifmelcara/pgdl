{-# LANGUAGE OverloadedStrings #-}

module Types
where

import Control.Applicative ((<$>))
import Text.HTML.DirectoryListing.Type
import Data.Text (Text)
import Data.Maybe
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import Brick.Widgets.List (listSelectedL, listElementsL, listSelectedElement)
import qualified Data.Vector as V
import Data.Vector ((!), Vector)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import Lens.Micro
import Control.Monad

data DNode = Directory Entry (IO [DNode]) | File Entry Text Bool
--                                                          downloaded?

-- | Note: The list in a DList should never be empty
data DList = DList (Seq DNode) [L.List Int]
--                 ^ DNode pool             ^ filter function apply to List


newDList :: [DNode] -> DList 
newDList = pushDList (DList S.empty []) 

-- | filter the elements in a DList, this action directly modify
-- the state of the top element of directory stack. The purpose of this function is
-- to provide the ability to dynamically filter list
-- Note: This causes the error:
--    https://github.com/coreyoconnor/vty/blob/08391a64c40240b3c5ce863a9ab3fa2e6f5bead9/src/Graphics/Vty/PictureToSpans.hs#L312
filterDList :: DList -> (DNode -> Bool) -> DList
filterDList (DList _ [_])  _ = error "try to filter a list without a reference (forgot to dupDList?)"
filterDList (DList sDn (x:ref:xs)) f = DList sDn (x':ref:xs)
    where
    newElements = (V.filter (f . S.index sDn) $ ref ^. listElementsL)
    x' = (listSelectedL .~ newSelLoc) $ x & listElementsL .~ newElements 
    oldSelectionVal = snd <$> listSelectedElement x
    -- determine new selected location carefully, brick/vector will often crash
    -- if the new location is out of bound.
    newSelLoc = do
        oldSelVal <- oldSelectionVal
        case V.elemIndex oldSelVal newElements of
            Nothing -> case V.length newElements of
                0 -> Nothing
                _ -> return 0
            Just i -> return i

-- | pop the directory stack (used when leaving a directory or exiting a filtered list)
popDList :: DList -> DList 
popDList dl@(DList _ [_]) = dl
popDList (DList sDn (x:xs)) = DList sDn xs

-- | used when we move to the search (filter) state
dupDList :: DList -> DList
dupDList (DList sDn (x:xs)) = DList sDn (x:x:xs)

-- | used when we enter a new directory
pushDList :: DList -> [DNode] -> DList
pushDList (DList sDn xs) dns = DList sDn' (x:xs)
    where
    sDn' = sDn >< S.fromList dns
    x = L.list "mainList" (V.enumFromN (S.length sDn) $ length dns) 3

-- | extract the currently selected DNode
extractSelectedDNode :: DList -> Maybe DNode
extractSelectedDNode (DList sDn (x:_)) = do
    (_, i) <- listSelectedElement x
    return $ S.index sDn i

-- | perform a monadic action on the selected element
mapSelectedDNodeM :: Monad m => DList -> (DNode -> m DNode) -> Maybe (m DList)
mapSelectedDNodeM (DList sDn (x:xs)) f = do
    (_, i) <- listSelectedElement x
    return $ do
        let e = S.index sDn i
        e' <- f e
        return $ DList (S.update i e' sDn) (x:xs)

replaceSelectedDNode :: DList -> DNode -> Maybe DList
replaceSelectedDNode dlst d = join $ mapSelectedDNodeM dlst (const $ Just d) 

adjustCurrentBrickList :: Monad m => DList -> (L.List Int -> m (L.List Int)) -> m DList
adjustCurrentBrickList (DList sDn (x:xs)) f = do
    x' <- f x
    return $ DList sDn (x':xs)

renderDList :: DList -> (Bool -> DNode -> T.Widget) -> T.Widget
renderDList (DList sDn (x:_)) render = L.renderList actualList render
    where
    actualList = x & listElementsL %~ V.map (S.index sDn)
    
