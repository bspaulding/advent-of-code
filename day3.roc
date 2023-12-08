app "day3"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stderr, pf.Stdout, pf.File, pf.Path, pf.Task.{ Task, await }]
    provides [main] to pf

fileName = "day3.input.txt"

insertCurrentWord = \{currentWord, items}, x, y ->
    { currentWord: []
    , items: Dict.insert items (Point (x - (List.len currentWord)) y) (PartNumber (Str.joinWith currentWord ""))}
parseNextChar = \{currentWord, items}, c, x, y, width -> 
    when c is
        "." ->
            if currentWord != [] then
                insertCurrentWord {currentWord, items} x y
            else
                {currentWord: [], items}
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> 
            newState = {currentWord: List.append currentWord c, items}
            # if we are at the end of the line, insert the current word
            if x == width - 1 then
               insertCurrentWord newState (x + 1) y 
            else
               newState
        _ -> if currentWord != [] then
            {currentWord: [], items: Dict.insert items (Point x y) (Symbol c)
                                    |> Dict.insert (Point (x - (List.len currentWord)) y) (PartNumber (Str.joinWith currentWord ""))}
            else 
            {currentWord: [], items: Dict.insert items (Point x y) (Symbol c)}


parseLine = \{ items }, line, y ->
    line
    |> Str.graphemes
    |> List.walkWithIndex { currentWord: [], items } (\s, c, x -> parseNextChar s c x y (strLen line))

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
    maxY = List.len input - 1
    maxX = (input |> List.first |> Result.withDefault "" |> strLen) - 1
    fullItems = Dict.walk items (Dict.empty {}) (\state, (Point x y), v ->
        when v is 
            PartNumber pn -> pn
                |> Str.graphemes
                |> List.walkWithIndex state (\s1, c, i -> Dict.insert s1 (Point (x + i) y) c)
            Symbol s -> Dict.insert state (Point x y) s
            _ -> state
    )
    drawLine = \y ->
        List.range { start: At 0, end: At maxX }
        |> List.map (\x -> 
                value = when Dict.get fullItems (Point x y) is
                    Ok s -> s
                    Err KeyNotFound -> "."
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
