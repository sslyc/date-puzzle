module Solution

open Pieces
open PuzzleMap

///解决问题，找出单个情况的第一个答案
let solveOne map (sel: seq<int>) (pieces: Piece[]) print =

    let printSuccess () =
        let combined = 
            if Seq.length sel = 0 then "this puzzle"
            else sel |> Seq.map (fun x -> (string x)) |> Seq.reduce (fun pre x -> pre + "," + x)
        printfn ""
        printfn "= One of solutions for (%s) is =" combined
        printMap map

    //初始化地图，填充空白位置
    do sitMapBlanks map sel

    //搜索时记录积木是否已经使用的map，为了效率，元素作为变量参与算法
    let pieceUsed = Array.create pieces.Length false

    //尝试填充shape，若失败撤销填充，若成功标记used为true
    let tryShape i shape x y =
        //排除出界情况
        if x - shape.Offset < 0 then false
        elif x - shape.Offset + shape.X > map.X then false
        elif y + shape.Y > map.Y then false
        else
            let rec tryShapeRec i (shape: Shape) xLoop yLoop =
                match xLoop, yLoop with
                | _, -1 -> true
                | -1, _ -> tryShapeRec i shape shape.X (yLoop - 1)
                | _ -> 
                    //x为当前测试map位置，它减去offset是当前图形起始位置， loopx为循环遍历当前图形小坐标。 y同理
                    let xIndex, yIndex = x - shape.Offset + xLoop, y + yLoop
                    if shape.Map[xLoop, yLoop] <> 1 then tryShapeRec i shape (xLoop - 1) yLoop
                    elif map.Map[xIndex, yIndex] <> -1 then false
                    else
                        map.Map[xIndex, yIndex] <- i
                        let rsl = tryShapeRec i shape (xLoop - 1) yLoop
                        if not rsl then map.Map[xIndex, yIndex] <- -1
                        rsl

            let rsl = tryShapeRec i shape shape.X shape.Y
            if rsl then pieceUsed[i] <- true
            rsl
            
    //回滚填充的shape
    let eraseShape i (shape: Shape) x y =
        for yLoop in 0 .. shape.Y do
            for xLoop in 0 .. shape.X do
                if shape.Map[xLoop, yLoop] = 1 then
                    map.Map[x - shape.Offset + xLoop, y + yLoop] <- -1
        pieceUsed[i] <- false

    //主过程，逐级递归
    let rec solveLoop x y = 
        match (x, y) with
        | x, y when x = map.X && y = map.Y -> 
            if print then printSuccess()
            true
        | _ -> 
            if map.Map[x, y] <> -1 then //已经填充
                solveLoop ((x + 1) % (map.X + 1)) (y + ((x + 1) / (map.X + 1)))
            else  //尝试填充
                seq { 0 .. pieces.Length - 1 }
                |> Seq.exists(fun i ->
                    if not pieceUsed[i] then
                        pieces[i].Shapes
                        |> List.exists (fun shape ->
                            if tryShape i shape x y then
                                //printMap map //测试，打印中间过程
                                let nextRsl = solveLoop ((x + 1) % (map.X + 1)) (y + ((x + 1) / (map.X + 1)))
                                if nextRsl then true
                                else eraseShape i shape x y; false
                            else false)
                    else false)

    //调用
    let rsl = solveLoop 0 0
    if not rsl && print then printfn "None solution for this selection."
    rsl
