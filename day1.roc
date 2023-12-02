app "day1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stderr, pf.Stdout, pf.File, pf.Path, pf.Task.{ Task, await }]
    provides [main] to pf

digits =
    Set.single "0"
    |> Set.insert "1"
    |> Set.insert "2"
    |> Set.insert "3"
    |> Set.insert "4"
    |> Set.insert "5"
    |> Set.insert "6"
    |> Set.insert "7"
    |> Set.insert "8"
    |> Set.insert "9"
    |> Set.insert "one"
    |> Set.insert "two"
    |> Set.insert "three"
    |> Set.insert "four"
    |> Set.insert "five"
    |> Set.insert "six"
    |> Set.insert "seven"
    |> Set.insert "eight"
    |> Set.insert "nine"
    |> Set.toList

stringReverse = \str -> str
    |> Str.graphemes
    |> List.reverse
    |> Str.joinWith ""

reverseDigits = List.map digits stringReverse

mapSpelledWord = \str ->
    when str is
        "nine" -> "9"
        "eight" -> "8"
        "seven" -> "7"
        "six" -> "6"
        "five" -> "5"
        "four" -> "4"
        "three" -> "3"
        "two" -> "2"
        "one" -> "1"
        "zero" -> "0"
        _ -> str

findFirstOccurence = \haystack, needles ->
    if Str.isEmpty haystack then
        ""
    else
        needles
        |> List.keepIf (\needle -> Str.startsWith haystack needle)
        |> List.first
        |> Result.withDefault (findFirstOccurence (Str.joinWith (List.dropFirst (Str.graphemes haystack) 1) "") needles)

main =
    fileName = "day1.input.txt"
    task =
        contents <- File.readUtf8 (Path.fromStr fileName) |> await

        lines =
            Str.split contents "\n"
            |> List.dropIf Str.isEmpty

        numbers = List.map lines \line ->
            first = findFirstOccurence line digits
            last =
                findFirstOccurence (stringReverse line) reverseDigits
                |> stringReverse
            Str.joinWith [mapSpelledWord first, mapSpelledWord last] ""
            |> Str.toU32
            |> Result.withDefault 0

        sum = List.walk numbers 0 Num.add
        Stdout.line (Str.joinWith (List.map (List.append numbers sum) Num.toStr) "\n")

    result <- Task.attempt task
    when result is
        Ok {} -> Task.ok {}
        Err _err -> Stderr.line "Error!"

