module Pieces

///积木变形形状
type Shape =
    { X: int //最大x坐标，而非length
      Y: int //最大y坐标，而非length
      Offset: int
      Map: int[,] }

///积木单元
type Piece =
    { Shapes: list<Shape> }

///根据提供的占位坐标点序列，生成所有积木单元，并计算全部不重复的形状
let genPieces (piecesDesc: seq<#seq<int * int>>) =

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

        //初始化，并确保积木左和上顶格摆放
        let maxX = Seq.map (fun (x, _) -> x) arr |> Seq.max
        let maxY = Seq.map (fun (_, y) -> y) arr |> Seq.max
        let minX = Seq.fold (fun pre (x, _) -> if x > pre then pre else x) (System.Int32.MaxValue) arr
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

///打印生成的积木形状，可用于测试
let printPieces pieces =
    printfn "= Pieces Info ="
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
    printfn "= You've got %d pieces =" (Seq.length pieces)
    printfn ""
