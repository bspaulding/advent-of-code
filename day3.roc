app "day3"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stderr, pf.Stdout, pf.File, pf.Path, pf.Task.{ Task, await }]
    provides [main] to pf

fileName = "day3.example.txt"

parseNextChar = \s, c, x, y -> s
# when c is
#     "." -> if state.currentWord != []
parseLine = \{ items }, line, y ->
    line
    |> Str.graphemes
    |> List.walkWithIndex { currentWord: [], items } (\s, c, x -> parseNextChar s c x y)

task =
    contents <- File.readUtf8 (Path.fromStr fileName) |> await
    lines =
        Str.split contents "\n"
        |> List.dropIf Str.isEmpty

    dbg 
        lines

    items =
        List.walkWithIndex lines { currentWord: [], items: Dict.empty {} } parseLine
        |> .items

    dbg
        items

    Stdout.line (Str.joinWith lines "\n")

main =
    result <- Task.attempt task
    when result is
        Ok {} -> Task.ok {}
        Err _err -> Stderr.line "Error!"
