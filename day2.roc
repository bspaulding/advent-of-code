app "day2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stderr, pf.Stdout, pf.File, pf.Path, pf.Task.{ Task, await }]
    provides [main] to pf

fileName = "day2.input.txt"

# only 12 red cubes, 13 green cubes, and 14 blue cubes
maxRed = 12
maxGreen = 13
maxBlue = 14

setIsPossible = \{ red, blue, green } -> red <= maxRed && blue <= maxBlue && green <= maxGreen
gameIsPossible = \{ sets } -> List.all sets setIsPossible

parseGameId = \line ->
    Str.splitFirst line ":"
    |> Result.withDefault { before: "", after: "" }
    |> .before
    |> Str.splitFirst " "
    |> Result.withDefault { before: "", after: "" }
    |> .after
    |> Str.toU32
    |> Result.withDefault 0
addColorCount = \counts, countStr ->
    tokens =
        Str.splitFirst countStr " "
        |> Result.withDefault { before: "0", after: "blue" }
    n =
        Str.toU32 tokens.before
        |> Result.withDefault 0
    when tokens.after is
        "blue" -> { counts & blue: n }
        "red" -> { counts & red: n }
        "green" -> { counts & green: n }
        _ -> counts
parseGameSet = \setStr ->
    Str.split setStr ","
    |> List.map Str.trim
    |> List.walk { red: 0, blue: 0, green: 0 } addColorCount
parseGameSets = \line ->
    Str.splitFirst line ":"
    |> Result.withDefault { before: "", after: "" }
    |> .after
    |> Str.split ";"
    |> List.map parseGameSet
parseGame = \line -> { id: parseGameId line, sets: parseGameSets line }

maxSet = \{ id, sets } -> {
    id,
    maxSet: sets
    |> List.walk { red: 0, blue: 0, green: 0 } (\{ red: r1, blue: b1, green: g1 }, { red: r2, blue: b2, green: g2 } -> { red: Num.max r1 r2, blue: Num.max b1 b2, green: Num.max g1 g2 }),
}

task =
    contents <- File.readUtf8 (Path.fromStr fileName) |> await
    lines =
        Str.split contents "\n"
        |> List.dropIf Str.isEmpty
    games = List.map lines parseGame
    dbg
        games

    possibleGames = List.keepIf games gameIsPossible
    possibleGameIds = List.map possibleGames .id
    dbg
        possibleGameIds

    sumPossibleIds = List.walk possibleGameIds 0 Num.add
    dbg
        SumPossibleIds sumPossibleIds

    maxSets = List.map games maxSet
    dbg
        MaxSets maxSets

    powers =
        maxSets
        |> List.map .maxSet
        |> List.map (\{ red, green, blue } -> red * green * blue)
    dbg
        powers

    sumOfPowers = List.walk powers 0 Num.add
    dbg
        SumOfPowers sumOfPowers

    Stdout.line "Done."

main =
    result <- Task.attempt task
    when result is
        Ok {} -> Task.ok {}
        Err _err -> Stderr.line "Error!"
