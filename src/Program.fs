
open System
open System.Linq
open Pieces
open PuzzleMap
open Defaults
open Console
open Solution
open Args
open FileProcess

[<Literal>]
let exitCode = 0

[<EntryPoint>]
let main args =
    try
        //解析参数
        let argsConfig = 
            [
                { Name = "map" 
                  Description = "Specify a map-pieces file, instead of default map&pieces"
                  LongAlias = []
                  ShortAlias = ['m']
                  Type = ArgumentType.String
                  ByPosition = false }
                { Name = "verify"
                  Description = "Configure if the program should find solutions of all combos or only one solution by user console input."
                  LongAlias = []
                  ShortAlias = ['v']
                  Type = ArgumentType.Boolean
                  ByPosition = false }
                { Name = "print-info"
                  Description = "Print the map and pieces configuration."
                  LongAlias = []
                  ShortAlias = ['p']
                  Type = ArgumentType.Boolean
                  ByPosition = false }
            ]
        let argsValue = Args(argsConfig).Process(args)
        //根据参数初始化配置
        let map, pieces = 
            match argsValue["map"] with 
            | Some x -> 
                let (String fileName) = x
                use fs = new System.IO.StreamReader(fileName)
                let str = fs.ReadToEnd()
                genMapAndPiecesWithDesc str
            | None -> genDefaultMap(), genDefaultPieces()

        let verify = 
            match argsValue["verify"] with
            | (Some (Boolean x)) -> x
            | _ -> false
        let shouldPrintInfo = 
            match argsValue["print-info"] with
            | (Some (Boolean x)) -> x
            | _ -> false
        if shouldPrintInfo then
            printPieces pieces
            printMapDescription map
        //解题
        if not verify then //如果解决单个问题
            if map.SearchSpaces.Length = 0 then //只有一种情况
                printfn ""
                printfn "Proceeding..."
                printfn ""
                solveOne map [] pieces true |> ignore
            else
                printSelInfo map.SearchSpaces
                while true do
                    printSel map.SearchSpaces
                    try
                        let sel = readSel map.SearchSpaces
                        printfn ""
                        printfn "Proceeding..."
                        printfn ""
                        //解决问题
                        solveOne map sel pieces true |> ignore
                    with ex -> 
                        printfn ""; 
                        printfn "%s" ex.Message; 
                        printfn ""
        else  //如果验证所有组合
            printfn ""
            printfn "Proceeding..."
            printfn ""

            //递归深入层级
            let rec allAvRec sel index = 
                if index > map.SearchSpaces.Length - 1 then 
                    if solveOne map sel pieces false then None
                    else Some (sel)
                else
                    let toSearch = map.SearchSpaces[index]
                    seq { toSearch.StartWith .. toSearch.StartWith + toSearch.Positions.Length - 1 }
                    |> Seq.tryPick (fun x ->
                        allAvRec (sel @ [x]) (index + 1))
                
            let allAv = allAvRec [] 0
            //判断是否成功，None为成功
            match allAv with
            | Some sel -> 
                let combined = 
                    if Seq.length sel = 0 then "this puzzle"
                    else sel |> Seq.map (fun x -> (string x)) |> Seq.reduce (fun pre x -> pre + "," + x)
            
                printfn ""
                printfn "None solution for (%s) " combined
                printfn "= Combines verifying failed ="
            | _ -> printfn "= Congratulations! Every combo found the solution ="
    with ex -> printfn "%s" ex.Message
    exitCode