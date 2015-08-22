module Misc where
import List exposing (..)

allNothing : List (Maybe a) -> Bool
allNothing l =
    let
        flist = List.filter (\m -> case m of 
                                       Nothing -> False 
                                       _ -> True) l
    in
        (List.length flist) == 0
