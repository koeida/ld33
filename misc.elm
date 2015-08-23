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

mapWhen : (a -> Bool) -> (a -> a) -> List a -> List a
mapWhen c f l =
    case l of
        [] -> []
        x::xs -> 
            let 
                res = if c x then f x else x
            in
               res :: mapWhen c f xs

