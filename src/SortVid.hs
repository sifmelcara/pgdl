
module SortVid where

import Video

import qualified Data.Text as T
import Data.List
import Data.Function

sortVid :: [Video] -> [Video]
sortVid = sortBy (cmpDate `on` vidDate)

cmpDate :: T.Text -> T.Text -> Ordering
cmpDate t1 t2 = 
