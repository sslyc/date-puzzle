module Defaults

open Pieces
open PuzzleMap

///生成默认积木
let genDefaultPieces() =  
    //根据实际情况，生成积木
    genPieces [
        [ (0, 0); (0, 1); (1, 1); (2, 1); (2, 0) ]
        [ (0, 0); (0, 1); (1, 1) ]
        [ (0, 0); (0, 1); (1, 0); (2, 0) ]
        [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 1) ]
        [ (0, 0); (0, 1); (1, 0); (1, 1) ]
        [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 1) ]
        [ (0, 0); (0, 1); (0, 2); (0, 3); ]
        [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 0) ]
        [ (0, 0); (0, 1); (0, 2); (1, 0); (2, 0) ]
        [ (0, 0); (1, 0); (1, 1); (2, 1) ]
        [ (0, 0); (1, 0); (2, 0); (1, 1) ]
    ]

///生成默认地图
let genDefaultMap() =
    let invalids = seq { for i in 3 .. 7 -> (i, 6) }
    let searchSpace = 
        [
            { Name = "month"; StartWith = 1; Positions = [| for i in 0..11 -> 4 + i % 4, i / 4 |] }
            { Name = "day"; StartWith = 1; Positions = [| for i in 0..3 do i, 2 done; for i in 0..26 do i % 8, 3 + i / 8 |] }
            { Name = "weekday"; StartWith = 1; Positions = [| for i in 0..2 do 1 + i, 0 done; for i in 0..3 do i, 1 |] }
        ]
    genMap 8 7 invalids searchSpace