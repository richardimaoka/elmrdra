module Data.ArrayExtend exposing (insert, remove, sort)

import Array exposing (Array)


insert : Int -> a -> Array a -> Array a
insert index element array =
    if index < 0 || Array.length array <= index then
        array

    else
        let
            upper =
                Array.slice 0 index array

            lower =
                Array.slice index (Array.length array) array
        in
        Array.append (Array.push element upper) lower


remove : Int -> Array a -> Array a
remove index array =
    if index < 0 || Array.length array <= index then
        array

    else
        let
            upper =
                Array.slice 0 index array

            lower =
                Array.slice (index + 1) (Array.length array) array
        in
        Array.append upper lower


sort : Int -> Int -> Array a -> Array a
sort fromIndex toIndex array =
    case Array.get fromIndex array of
        -- if fromIndex is out of range, return array without modification
        Nothing ->
            array

        Just element ->
            -- remove, then insert
            insert toIndex element (remove fromIndex array)
