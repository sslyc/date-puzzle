module FileProcess

open PuzzleMap
open Pieces

//////////map format/////////
//xSize,ySize
//map
//invalid
//x1,y1-x2,y2
//x3,y3
//x4,y4
//end invalid
//month 1
//x1,y1
//x2,y2-x3,y3
//end month
//day 1
//x1,y1
//x2,y2-x3,y3
//end day
//end map
//pieces
//piece
//x1,y1
//x2,y2-x3,y3
//end piece
//end pieces

//NOTICE:
//After invalid section, next few sections before map ending are custom search areas. 
//You can name them as will. The name will show on the Selection Info when in single mode.
//Within one area, the coordinates describe how to search the area. The coordinates will search one by one, to make a blank on the map.
//Several areas are searching in the same time. That means, 'month' and 'day' will take two blanks , one is from month area, another is from day area, to complete the solution.
//Section name is not case-sensitive, and every custom area name will be transform into lower form.

//IMPORTANT: 
//When using x1,y1-x2,y2 format, 
//it means a rect that starts from left-top by x1,x1, 
//ends to right-bottom by x2,y2. 
//!!!DO NOT!!! make x1 > x2 or y1 > y2, that will make the rect empty.

//通过描述文本
let genMapAndPiecesWithDesc (desc: string) =

    //开始折腾
    let rec genMapAndPiecesWithDescRec (arr: string[]) index search searchValue sectionStartWith invalidDesc searchAreaDesc piecesDesc =
        match index with
        | _ when index > arr.Length - 1 ->
            invalidDesc, searchAreaDesc, piecesDesc
        | _ ->
            let line = arr[index].Trim()
            let outter = ["map"; "pieces"] 
            match line.ToLower() with
            | close when close.StartsWith("end") -> 
                if List.length search = 0 then failwithf "Invalid map file: line %d, unexpected 'end' section." index
                let closeArr = close.Split('\t', ' ') |> Array.filter (fun i -> i <> "")
                if closeArr.Length <> 2 || closeArr[1] <> List.head search then failwithf "Invalid map file: line %d, expect 'end %s', but found %s." index (List.head search) line
                match List.head search with
                | "pieces" | "map" ->
                    genMapAndPiecesWithDescRec arr (index + 1) (List.tail search) searchValue 0 invalidDesc searchAreaDesc piecesDesc
                | "piece" ->
                    genMapAndPiecesWithDescRec arr (index + 1) (List.tail search) [] 0 invalidDesc searchAreaDesc (piecesDesc @ [searchValue])
                | "invalid" ->
                    genMapAndPiecesWithDescRec arr (index + 1) (List.tail search) [] 0 (invalidDesc @ searchValue) searchAreaDesc piecesDesc
                | custom ->
                    let area = { Name = custom; StartWith = sectionStartWith; Positions = searchValue |> Array.ofList }
                    genMapAndPiecesWithDescRec arr (index + 1) (List.tail search) [] 0 invalidDesc (searchAreaDesc @ [area]) piecesDesc
            | coor when coor.Contains(",") ->
                if List.length search = 0 then failwithf "Invalid map file: line %d, unexpected 'end' section." index
                if outter |> List.exists (fun i -> i = List.head search) then
                    failwithf "Invalid map file: line %d, expect '%s' in '%s' section." index line (List.head search)
                let coorGp = coor.Split('-') |> Array.map (fun i -> i.Trim())
                match coorGp.Length with
                | 1 ->
                    let singleCoor = coorGp[0].Split(',') |> Array.map (fun i -> i.Trim())
                    if singleCoor.Length <> 2 then failwithf "Invalid map file: line %d, invalid coordinate(s)" index
                    let isIntX, x = System.Int32.TryParse(singleCoor[0])
                    if not isIntX then failwithf "Invalid map file: line %d, invalid coordinate(s)" index
                    let isIntY, y = System.Int32.TryParse(singleCoor[1])
                    if not isIntY then failwithf "Invalid map file: line %d, invalid coordinate(s)" index
                    genMapAndPiecesWithDescRec arr (index + 1) search (searchValue @ [(x, y)]) sectionStartWith invalidDesc searchAreaDesc piecesDesc
                | 2 ->
                    let oneCoor = coorGp[0].Split(',') |> Array.map (fun i -> i.Trim())
                    if oneCoor.Length <> 2 then failwithf "Invalid map file: line %d, invalid coordinate(s)" index
                    let isIntX, xLeft = System.Int32.TryParse(oneCoor[0])
                    if not isIntX then failwithf "Invalid map file: line %d, invalid coordinate(s)" index
                    let isIntY, yTop = System.Int32.TryParse(oneCoor[1])
                    if not isIntY then failwithf "Invalid map file: line %d, invalid coordinate(s)" index
                    let twoCoor = coorGp[1].Split(',') |> Array.map (fun i -> i.Trim())
                    if twoCoor.Length <> 2 then failwithf "Invalid map file: line %d, invalid coordinate(s)" index
                    let isIntX, xRight = System.Int32.TryParse(twoCoor[0])
                    if not isIntX then failwithf "Invalid map file: line %d, invalid coordinate(s)" index
                    let isIntY, yBottom = System.Int32.TryParse(twoCoor[1])
                    if not isIntY then failwithf "Invalid map file: line %d, invalid coordinate(s)" index
                    let list = 
                        [
                            for y in yTop..yBottom do
                                for x in xLeft..xRight do
                                    x, y
                        ]
                    genMapAndPiecesWithDescRec arr (index + 1) search (searchValue @ list) sectionStartWith invalidDesc searchAreaDesc piecesDesc
                | _ -> failwithf "Invalid map file: line %d, too many '-'" index
            | section ->
                if outter |> List.exists (fun i -> i = section) then
                    if search.Length <> 0 then failwithf "Invalid map file: line %d, '%s' can only be placed in top level" index section
                    genMapAndPiecesWithDescRec arr (index + 1) (section :: search) searchValue 0 invalidDesc searchAreaDesc piecesDesc
                else
                    if search.Length = 0 then failwithf "Invalid map file: line %d, '%s' can not be placed in top level" index line
                    if not (outter |> List.exists (fun i -> i = List.head search)) then
                        failwithf "Invalid map file: line %d, '%s' section can not be placed in '%s' section" index line (List.head search)
                    if List.head search = "pieces" && section <> "piece" then
                        failwithf "Invalid map file: line %d, expect 'piece', but found '%s'" index line
                    if List.head search = "map" && section <> "invalid" then
                        let secArr = section.Split(' ', '\t') |> Array.filter (fun i -> i <> "")
                        if secArr.Length <> 2 then failwithf "Invalid map file: line %d, custom area need start number specified" index
                        let sectionName, (success, startWith) = secArr[0], System.Int32.TryParse secArr[1]
                        if not success then failwithf "Invalid map file: line %d, custom area start number must be integer" index
                        genMapAndPiecesWithDescRec arr (index + 1) (sectionName :: search) [] startWith invalidDesc searchAreaDesc piecesDesc
                    else
                        genMapAndPiecesWithDescRec arr (index + 1) (section :: search) [] 0 invalidDesc searchAreaDesc piecesDesc  //没有可能到这里了 searchValue不为空
    //组织为数组
    let descArr = desc.Replace("\r", "").Split('\n') |> Array.filter (fun i -> i <> "")   
    if descArr.Length < 1 then failwithf "Invalid map file: empty file"
    let mapSizeArr = descArr[0].Split(',') |> Array.map (fun i -> i.Trim()) |> Array.filter (fun i -> i <> "")
    if mapSizeArr.Length <> 2 then failwithf "Invalid map file: line %d invalid map size" 0 
    let isIntXSize, xSize = System.Int32.TryParse(mapSizeArr[0])
    if not isIntXSize then failwithf "Invalid map file: line %d, invalid map size" 0 
    let isIntYSize, ySize = System.Int32.TryParse(mapSizeArr[1])
    if not isIntYSize then failwithf "Invalid map file: line %d, invalid map size" 0
    let invalidDesc, areasDesc, piecesDesc = 
        genMapAndPiecesWithDescRec descArr 1 [] [] 0 [] [] []
    genMap xSize ySize invalidDesc areasDesc, genPieces piecesDesc
    