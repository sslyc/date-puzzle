module Console

open System
open PuzzleMap


///打印选择项提示
let printSelInfo (arr: seq<SearchArea>) =
    printfn "【Search Areas Selection Info】"
    arr
    |> Seq.iter (fun x ->
        printf "%s" x.Name
        printf ": "
        printfn "%d-%d" x.StartWith (x.StartWith + x.Positions.Length - 1))
    printfn ""
    printfn "When input a selection, separated each argument with space(s)."
    printfn ""

let printSel (arr: seq<SearchArea>) =
    printf "Please input "
    let str = 
        arr
        |> Seq.map (fun i -> i.Name)
        |> Seq.reduce (fun x y -> x + ", " + y)
    printfn "%s:" str
    printf "> " 

///读取选择项
let readSel (searchAreas: SearchArea[]) =
    Console.ReadLine().Trim().Split([| ' '; '\t' |])
    |> Array.filter (fun x -> not (String.IsNullOrEmpty(x)))
    |> (fun x ->
        if x.Length <> searchAreas.Length then
            failwithf "Invalid input: Wrong numbers of arguments. Provided %d while Requires %d." x.Length searchAreas.Length
        else
            [| 
                for i in 0 .. x.Length - 1 do
                    let cur = int (Array.item i x)
                    if cur < searchAreas[i].StartWith || 
                        cur > searchAreas[i].StartWith + searchAreas[i].Positions.Length - 1 then
                            failwithf "Invalid input: Argument '%s' requires in range %d-%d, while provided %d." 
                                searchAreas[i].Name
                                searchAreas[i].StartWith
                                (searchAreas[i].StartWith + searchAreas[i].Positions.Length - 1)
                                cur
                    else
                        cur
            |])
    