//积木变形形状
type Shape = 
    { X: int //最大x坐标，而非length
      Y: int //最大y坐标，而非length
      Offset: int
      Map: int[,] }

//积木单元
type Piece = 
    { Shapes: list<Shape> }


//根据提供的占位坐标点序列，生成所有积木单元，并计算全部不重复的形状
let genPieces (piecesDesc: seq<seq<int * int>>) =

    //生成一个积木单元
    let genPiece (arr: seq<int * int>) =

        //根据基础形状，生成所有不重复形状
        let genAvailabeShapes shape =

            //变形，f为目的地元素函数
            let transShape shape f =
                let cur = Array2D.create (shape.Y + 1) (shape.X + 1) 0 //(x,y)格式，先列后行

                for y in 0 .. shape.X do  //行列互换
                    for x in 0 .. shape.Y do
                        cur[x, y] <- f shape x y //注意，行列互换了

                //取第一行的偏移
                let offset =
                    seq { 0 .. shape.Y }  //注意，列长变成了原来的行长度
                    |> Seq.pick (fun x ->
                        if cur[x, 0] = 1 then 
                            Some x
                        else
                            None)

                { Map = cur
                  X = shape.Y
                  Y = shape.X
                  Offset = offset }
    
            //旋转变形（顺时针，因此行列互换）
            let rotateShape shape =
                transShape shape (fun shape x y -> shape.Map[y, shape.Y - x])  //新左上点=原左下点，所以是顺时针
    
            //翻转变形（左上、右下对角线，因此仍然是行列互换）
            let turnOverShape shape =
                transShape shape (fun shape x y -> shape.Map[y, x])
    
            //重复旋转获取四种情况（含未旋转）
            let getRotateShapes4 shape = 
                let rec rotateLoop shape n = //4、3、2、1，尾递归
                    match n with
                    | 1 -> [ shape ]
                    | _ -> shape :: rotateLoop (rotateShape shape) (n - 1)
                rotateLoop shape 4

            //旋转、翻转获取变形列表
            let rotatedShapes = getRotateShapes4 shape  //旋转的4个形状

            let turnedOverShape = turnOverShape shape  //翻转形状
            let turnedOverRotatedShapes = getRotateShapes4 turnedOverShape //基于翻转旋转的4个形状
            
            let allShapes = List.append rotatedShapes turnedOverRotatedShapes  //拼接全部8个形状

            ////筛选有效形状（不重复）
            //let searchAvailableShapes (allshapes: List<Shape>) =
            //判断形状相同，用于剔除旋转后重复的形状
            let shapeEqual shape1 shape2 =
                if shape1.X <> shape2.X then false
                elif shape1.Y <> shape2.Y then false
                else
                    seq { 0 .. shape1.Y }
                    |> Seq.exists (fun y ->
                        seq { 0 .. shape1.X }
                        |> Seq.exists (fun x ->
                            shape1.Map[x, y] <> shape2.Map[x, y]))
                    |> not

            //去重作为结果，顺序会反过来，无所谓
            allShapes
            |> Seq.fold (fun next shape ->
                let hasEqual =
                    next
                    |> Seq.exists(fun av -> shapeEqual av shape)
                if hasEqual then next else shape :: next) []
            //|> List.rev  //如果不高兴，可以再正过来

        //初始化
        let maxX = Seq.map (fun (x, _) -> x) arr |> Seq.max
        let maxY = Seq.map (fun (_, y) -> y) arr |> Seq.max
        let cur = Array2D.create (maxX + 1) (maxY + 1) 0 //(x,y)格式，先列后行

        //根据参数填充格子
        Seq.iter (fun (x, y) -> cur[x, y] <- 1) arr  

        //取第一行的偏移
        let offset =
            seq { 0 .. maxX }
            |> Seq.pick (fun x ->
                if cur[x, 0] = 1 then Some x
                else None)
                
        //基础形状
        let shape = 
            { X = maxX
              Y = maxY
              Offset = offset  //用于判断
              Map = cur }

        //计算本积木的所有可用变形并返回为一个Piece
        { Shapes = genAvailabeShapes shape }

    //根据参数生成所有积木，块数越多的越早测试，有效剪枝
    Seq.map (fun pieceDesc -> genPiece pieceDesc) piecesDesc
    //先按照形状的多少预排（感觉用处不大）
    |> Seq.sortByDescending (fun piece -> piece.Shapes.Length)
    //再按照块的大小排序（后一次排序为主排序，因为sort是稳定排序，所以前一次排序会作为副排序）
    |> Seq.sortByDescending (fun piece -> 
        seq { 
            for y in 0 .. piece.Shapes[0].Y do
                for x in 0 .. piece.Shapes[0].X -> piece.Shapes[0].Map[x, y] 
        }
        |> Seq.sum)
    |> Array.ofSeq  //采用数组，使得索引器效率变为O(1)

//根据实际情况，生成积木
//原始解
let pieces = genPieces [
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

////根据实际情况，生成积木
////测试通过
//let pieces = genPieces [
//    [ (0, 0); (0, 1); (1, 1); (2, 1); (2, 0) ]
//    [ (0, 0); (0, 1); ]
//    [ (1, 0); (0, 1); (1, 1); (2, 1); (1, 2) ]
//    [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 1) ]
//    [ (0, 0); (0, 1); (1, 0); (1, 1) ]
//    [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 1) ]
//    [ (0, 0); (0, 1); (0, 2); (0, 3); ]
//    [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 0) ]
//    [ (0, 0); (0, 1); (0, 2); (1, 0); (2, 0) ]
//    [ (0, 0); (1, 0); (1, 1); (2, 1) ]
//    [ (0, 0); (1, 0); (2, 0); (1, 1) ]
//]

////根据实际情况，生成积木
////测试通过
//let pieces = genPieces [
//    [ (1, 0); (0, 1); (1, 1); (2, 1); (0, 2); ]
//    [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 1) ]
//    [ (0, 0); (0, 1); (1, 0); (1, 1); (2, 1) ]
//    [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 1) ]
//    [ (0, 0); (0, 1); (0, 2); (0, 3); ]
//    [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 0) ]
//    [ (0, 0); (0, 1); (0, 2); (1, 0); (2, 0) ]
//    [ (0, 0); (1, 0); (1, 1); (2, 1) ]
//    [ (0, 0); (1, 0); (2, 0); (1, 1) ]
//    [ (0, 0); (0, 1); (1, 1); (2, 1); (2, 0) ]
//    [ (0, 0); ]


////根据实际情况，生成积木
////测试通过
//let pieces = genPieces [
//    [ (1, 0); (0, 1); (1, 1); (2, 1); (0, 2); ]
//    [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 1) ]
//    [ (0, 0); (0, 1); (1, 0); (1, 1); (2, 1) ]
//    [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 1) ]
//    [ (0, 0); (0, 1); (0, 2); (1, 0); ]
//    [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 0) ]
//    [ (0, 0); (0, 1); (0, 2); (1, 0); (2, 0) ]
//    [ (0, 0); (1, 0); (1, 1); (2, 1) ]
//    [ (0, 0); (1, 0); (2, 0);  ]
//    [ (0, 0); (0, 1); (1, 1); (2, 1); (2, 0); (2, 2) ]
//    [ (0, 0); ]
//]

//测试函数，打印生成的积木形状
let printPieces () =
    for piece in pieces do
        printfn ""
        for shape in piece.Shapes do
            for y in 0 .. shape.Y do
                for x in 0 .. shape.X do
                    if shape.Map[x, y] = 1 then printf "* "
                    else printf "  "
                printfn ""
            printfn ""
        printfn "==============="
    printfn ""
printPieces ()

let solve month day weekday print =
    //计算位置坐标
    let blankPos (month, day, weekday) =
    
        //计算星期坐标
        let weekdayPos weekdayZB =
            if weekdayZB < 3 then 1 + weekdayZB, 0
            else weekdayZB - 3, 1
            
        //计算月份坐标
        let monthPos monthZB =
            4 + monthZB % 4, monthZB / 4
    
        //计算日期坐标
        let dayPos dayZB =
            match dayZB with
            | 0 | 1 | 2 | 3 -> dayZB, 2
            | _ -> (dayZB - 4) % 8, 3 + (dayZB - 4) / 8
    
        monthPos (month - 1), dayPos (day - 1), weekdayPos (weekday - 1)

    let monthPos, dayPos, weekdayPos = blankPos (month, day, weekday)

    //搜索时用的map
    let map = Array2D.create 8 7 -1 //(x,y)格式，先列后行
    //赋初值
    seq { 3 .. 7 } |> Seq.iter (fun i -> map[i, 6] <- -100)  //-100为地图黑域
    map[fst monthPos, snd monthPos] <- 100  //100为空出位置
    map[fst dayPos, snd dayPos] <- 100
    map[fst weekdayPos, snd weekdayPos] <- 100
    //搜索时记录积木是否已经使用的map，为了效率，元素作为变量参与算法
    let pieceUsed = Array.create pieces.Length false

    //打印解
    let printMap () =
        let showChar d =
            match d with
            | -100 | 100 -> ' '
            | -1 -> '_'  //打印中间过程时用于测试
            | d -> "*#+&@$%08oD"[d]  //积木打印字符集
        printfn ""
        for y in 0..6 do
            for x in 0..7 do
                printf "%c " (showChar map.[x, y])
            printfn ""
        printfn ""

    //尝试填充shape，若失败撤销填充，若成功标记used为true
    let tryShape i shape x y =
        //排除出界情况
        if x - shape.Offset < 0 then false
        elif x - shape.Offset + shape.X > 7 then false
        elif y + shape.Y > 6 then false
        else
            let rec tryShapeRec i shape xLoop yLoop =
                match xLoop, yLoop with
                | _, -1 -> true
                | -1, _ -> tryShapeRec i shape shape.X (yLoop - 1)
                | _ -> 
                    //x为当前测试map位置，它减去offset是当前图形起始位置， loopx为循环遍历当前图形小坐标。 y同理
                    let xIndex, yIndex = x - shape.Offset + xLoop, y + yLoop
                    if shape.Map[xLoop, yLoop] <> 1 then tryShapeRec i shape (xLoop - 1) yLoop
                    elif map[xIndex, yIndex] <> -1 then false
                    else
                        map[xIndex, yIndex] <- i
                        let rsl = tryShapeRec i shape (xLoop - 1) yLoop
                        if not rsl then map[xIndex, yIndex] <- -1
                        rsl

            let rsl = tryShapeRec i shape shape.X shape.Y
            if rsl then pieceUsed[i] <- true
            rsl
            
    //回滚填充的shape
    let eraseShape i shape x y =
        for yLoop in 0 .. shape.Y do
            for xLoop in 0 .. shape.X do
                if shape.Map[xLoop, yLoop] = 1 then
                    map[x - shape.Offset + xLoop, y + yLoop] <- -1
        pieceUsed[i] <- false

    //主过程，逐级递归
    let rec solveLoop x y = 
        match (x, y) with
        | 3, 6 -> 
            if print then printMap (); 
            true
        | _ -> 
            if map[x, y] <> -1 then //已经填充
                solveLoop ((x + 1) % 8) (y + ((x + 1) / 8))
            else  //尝试填充
                seq { 0..10 }
                |> Seq.exists(fun i ->
                    if not pieceUsed[i] then
                        pieces[i].Shapes
                        |> List.exists (fun shape ->
                            if tryShape i shape x y then
                                //printMap () //测试，打印中间过程
                                let nextRsl = solveLoop ((x + 1) % 8) (y + ((x + 1) / 8))
                                if nextRsl then true
                                else eraseShape i shape x y; false
                            else false)
                    else false)

    //printMap () //测试用，打印初始map

    //调用
    solveLoop 0 0

open System
open System.Linq

printfn "模式："
printfn "1) 根据输入求解"
printfn "2) 验证所有组合有解"
printf "请选择："

let choose = Console.ReadLine().Trim()

printfn ""

match choose with
| "1" -> 
    while true do

        printf "输入 月份、日期、星期："
        try

            let month, day, weekday = 
                Console.ReadLine().Trim().Split([| ' '; '\t' |]).Where(fun x -> not (String.IsNullOrEmpty(x)))
                |> (fun x ->
                    if x.Count() = 3 then
                        int (Seq.item 0 x), int (Seq.item 1 x), int (Seq.item 2 x)
                    else
                        failwith "")
            if 
                month < 1 ||
                month > 12 ||
                day < 1 ||
                day > 31 ||
                weekday < 1 ||
                weekday > 7 then failwith ""

            printfn ""
            printfn "=== %02d.%02d-%d ===" month day weekday

            //解决问题
            let solved = solve month day weekday true

            if not solved then printfn "此题无解"

            printfn "==============="
            printfn ""
        with ex -> 
            printfn ""; 
            printfn "输入有误，请重新输入！"; 
            printfn ""
| "2" ->
    
    let allAv = 
        seq { 1..12 }
        |> Seq.tryPick (fun month ->
            seq { 1..31 }
            |> Seq.tryPick (fun day ->
                seq { 1..7 }
                |> Seq.tryPick(fun weekday ->
                    if solve month day weekday false then None
                    else Some (month, day, weekday))))

    match allAv with
    | Some (month, day, weekday) -> printfn "%02d.%02d-%d 无解！" month day weekday
    | _ -> printfn "恭喜！测试通过，全部有解！"

| _ -> printfn "输入有误，退出程序！"