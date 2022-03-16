module PuzzleMap

///搜索区域
type SearchArea = 
    { Positions: (int * int)[] 
      StartWith: int
      Name: string }

///谜题地图
type PuzzleMap =
    { Map: int[,]
      //最大x坐标，而非length
      X: int 
      //最大y坐标，而非length
      Y: int
      SearchSpaces: SearchArea[] }

///根据描述生成地图，其中invalidBlocks为固有无效区域，searchArea为待选空白区域，每项为一个独立选择项
let genMap xSize ySize (invalidBlocks: seq<int * int>) (searchArea: seq<SearchArea>) =
    //创建map
    let map = Array2D.create xSize ySize -1 //(x,y)格式，先列后行

    //填充无效位
    invalidBlocks
    |> Seq.iter (fun (x, y) ->
        map[x, y] <- -100)

    { Map = map; X = xSize - 1; Y = ySize - 1; SearchSpaces =  searchArea |> Array.ofSeq }

///清空地图
let clearMap map =
    for i in 0..map.X do
        for j in 0..map.Y do
            if map.Map[i, j] <> -100 then map.Map[i, j] <- -1  //非无效位全部清空

///初始化地图，填充空白位置，用于开始一次搜索
let sitMapBlanks map (sel: seq<int>) =
    do 
        clearMap map
        //填入空出位置
        Seq.iter2 (fun area i -> 
            let index = i - area.StartWith
            map.Map[fst area.Positions[index], snd area.Positions[index]] <- 100
        ) (map.SearchSpaces) sel

///打印地图
let printMap map =
    let showChar d =
        match d with
        | -100 | 100 -> ' '
        | -1 -> '_'  //用于打印中间过程
        | d when d < 14 -> "*#+&@$%8otx4v"[d]  //默认积木打印字符集
        | d -> 'A' + char (d - 14) //大写字母A开始
    printfn ""
    for y in 0 .. map.Y do
        for x in 0 .. map.X do
            printf "%c " (showChar map.Map[x, y])
        printfn ""
    printfn ""

///打印地图描述
let printMapDescription map =
    let showChar d =
        match d with
        | -100 | 100 -> ' '
        | -1 -> '_'  
        | d when d < 14 -> "*#+&@$%8otx4vd"[d]  
        | d -> 'A' + char (d - 14) 
    let infoMap = Array2D.copy map.Map
    for i in 0 .. map.SearchSpaces.Length - 1 do
        for x, y in map.SearchSpaces[i].Positions do
            infoMap[x, y] <- i
    printfn "= Map Info (each kind of char means a search area) ="
    printfn ""    
    for y in 0 .. map.Y do
        for x in 0 .. map.X do
            printf "%c " (showChar infoMap[x, y])
        printfn ""
    printfn ""