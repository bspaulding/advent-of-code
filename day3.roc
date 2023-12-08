app "day3"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stderr, pf.Stdout, pf.File, pf.Path, pf.Task.{ Task, await }]
    provides [main] to pf

fileName = "day3.test.txt"

parseNextChar = \{currentWord, items}, c, x, y -> 
    when c is
        "." ->
            if currentWord != [] then
                {currentWord: [], items: Dict.insert items (Point (x - (List.len currentWord)) y) (PartNumber (Str.joinWith currentWord ""))}
            else
                {currentWord: [], items}
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> 
            {currentWord: List.append currentWord c, items}
        _ -> if currentWord != [] then
            {currentWord: [], items: Dict.insert items (Point x y) (Symbol c)
                                    |> Dict.insert (Point (x - (List.len currentWord)) y) (PartNumber (Str.joinWith currentWord ""))}
            else 
            {currentWord: [], items: Dict.insert items (Point x y) (Symbol c)}


parseLine = \{ items }, line, y ->
    line
    |> Str.graphemes
    |> List.walkWithIndex { currentWord: [], items } (\s, c, x -> parseNextChar s c x y)

adjacentAnySymbol = \items, (Point x y), pn -> 
    pnLen = List.len (Str.graphemes pn)
    startX = if x == 0 then 0 else x - 1
    startY = if y == 0 then 0 else y - 1
    pointsToCheck = List.range { start: At startX, end: At (x + 1 + pnLen)}
        |> List.joinMap (\x1 -> List.range { start: At startY, end: At (y + 1) }
                                |> List.map (\y1 -> Point x1 y1))
    # dbg PointsToCheck (Point x y) (PartNumber pn) pointsToCheck
    isSymbol = \p -> 
        when Dict.get items p is
            Ok (Symbol _) -> Bool.true
            _ -> Bool.false
    List.any pointsToCheck isSymbol

task =
    contents <- File.readUtf8 (Path.fromStr fileName) |> await
    lines =
        Str.split contents "\n"
        |> List.dropIf Str.isEmpty


    items =
        List.walkWithIndex lines { currentWord: [], items: Dict.empty {} } parseLine
        |> .items

    dbg 
        Items items

    filterPartNumbers = \pns, point, item ->
        when item is
            PartNumber pn if adjacentAnySymbol items point pn ->
                List.append pns (PartNumber pn)
            _ -> pns
    partNumbers =
        Dict.walk items [] filterPartNumbers
    # dbg
    #     partNumbers
    parsedNumbers = List.map partNumbers (\(PartNumber pn) -> Result.withDefault (Str.toU32 pn) 0)
    sum = List.walk parsedNumbers 0 Num.add
    dbg Sum sum

    Stdout.line (redraw lines items)

strLen = \str -> str |> Str.graphemes |> List.len

redraw = \input, items -> 
    maxY = List.len input
    maxX = input |> List.first |> Result.withDefault "" |> Str.graphemes |> List.len
    dbg MapSize maxX maxY
    fullItems = Dict.walk items items (\state, (Point x y), v ->
        when v is 
            PartNumber pn -> List.range { start: At x, end: At (strLen pn - 1)}
                |> List.walk items (\state, p, v -> )
    )
    drawLine = \y ->
        List.range { start: At 0, end: At maxX }
        |> List.map (\x -> 
                value = when Dict.get fullItems (Point x y) is
                    Ok (Symbol s) -> s
                    Ok (PartNumber pn) -> pn
                    Err KeyNotFound -> "."
                dbg Point x y Value value
                value
            )
        |> Str.joinWith ""
    lines = List.range { start: At 0, end: At maxY }
        |> List.map drawLine
        |> Str.joinWith "\n"
    
    lines

main =
    result <- Task.attempt task
    when result is
        Ok {} -> Task.ok {}
        Err _err -> Stderr.line "Error!"
