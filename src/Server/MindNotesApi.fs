module MindNotes.Api
open FsharpMyExtension
open FsharpMyExtension.Either

open Shared.MindNotes.Api

module Parser =
    open FParsec
    type 'a Parser = Parser<'a,unit>

    let skipSpaces' : _ Parser = skipManySatisfy (isAnyOf " \t")

    let ptag : _ Parser = pchar '#' >>. many1Satisfy (isNoneOf "\n ")

    let tags1 =
        many1 (ptag .>> skipSpaces')

    let pdate : _ Parser =
        notFollowedByString "#" >>. many1Satisfy (isNoneOf "\n")
        >>= fun x ->
            match System.DateTime.TryParse x with
            | true, x -> preturn x
            | _ -> failFatally "datetime" // TODO: чем отличается `fail` от `failFatally`?
    // "10.03.2019 0:35:28"
    let pdatetime : _ Parser =
        pipe4
            (pint32 .>> pchar '.')
            (pint32 .>> pchar '.')
            (pint32 .>> skipSpaces')
            (opt (tuple3
                    pint32
                    (pchar ':' >>. pint32)
                    (opt (pchar ':' >>. pint32)))
                )
            (fun day month year time ->
                match time with
                | Some(hour, min, sec) ->
                    let sec = sec |> Option.defaultValue 0
                    System.DateTime(year, month, day, hour, min, sec)
                | None -> System.DateTime(year, month, day))
    let ptext : _ Parser =
        many1Strings
            (
                notFollowedBy (pstring "***" .>> (skipNewline <|> eof))
                >>. (many1Satisfy ((<>) '\n') <|> newlineReturn "\n")
            )
    let textTest () =
        let test str =
            match run ptext str with
            | Success(x, _, _) -> Right x
            | Failure(x, _, _) -> Left x
        [
            test "* one" = Right "* one"
            test "**sdf" = Right "**sdf"
            test "***sdf" = Right "***sdf"
            Either.isLeft (test "***\nsdf\nsdf")
            test "**\nsdf\n***\nsdf" = Right "**\nsdf\n"
        ] |> List.forall id
    /// Дело в том, что если копировать дату-время файла с вкладки "Свойства" в Window 7, то он вставляет какие-то загадочные Unicode-разделители, выглядит это так: `\u200e6 \u200eдекабря \u200e2018 \u200eг., \u200f\u200e14:17:27`.
    let pdatetimeStrange : _ Parser =
        let sep = '\u200e' // LEFT-TO-RIGHT MARK
        let sep2 = '\u200f' // RIGHT-TO-LEFT MARK
        let iform = System.Globalization.CultureInfo("ru-RU", false)
        pchar sep
        >>. many1Strings (many1Satisfy (isNoneOf [sep; sep2; '\n']) <|> charReturn sep "" <|> charReturn sep2 "")
        >>= fun x ->
            try
                preturn (System.DateTime.Parse(x, iform))
            with e ->
                failFatally e.Message
    let dateConvert str =
        match run pdatetimeStrange str with
        | Success(x, _, _) -> sprintf "%A" x |> Clipboard.setText
        | Failure(x, _, _) -> failwithf "%s" x

    // run pdatetimeStrange "\u200e6 \u200eдекабря \u200e2018 \u200eг., \u200f\u200e14:17:27"
    let pnote ptext : _ Parser =
        let tags =
            // run (notFollowedByL (pchar '#' >>. satisfy (isAnyOf " \n")) "# <space|\\n>") "# some"
            let p = pstring "//" >>. skipSpaces'
            (p >>? tags1) <|> (notFollowedBy (pchar '#' >>. satisfy (isAnyOf " \n")) >>. tags1)
            .>> skipNewline
        pipe3
            (opt (pdatetimeStrange <|> pdatetime .>> skipNewline))
            // (opt (opt (pstring "//" >>. skipSpaces') >>? (tags1 .>> skipNewline)))
            (opt tags)
            ptext
            (fun datetime tags text ->
                { DateTime = datetime
                  Tags = tags |> Option.defaultValue []
                  Text = text })
    let test () =
        let str =
            [
                "\u200e6 \u200eдекабря \u200e2018 \u200eг., \u200f\u200e14:17:27"
                "#сюжет"
                "— А ты мне что скажешь? — нетерпеливо спросил царь."
            ] |> String.concat "\n"
        run (pnote ptext) str
    let start : _ Parser =
        let sep = pstring "***" .>> skipNewline
        optional sep
        >>. sepEndBy1 (pnote ptext) sep
        .>> eof
    // run (opt (pstring "//" >>. skipSpaces') >>? tags .>> skipNewline) "Настольные игры.\n"
    // run text "Настольные игры."
    let startOnFile path =
        match runParserOnFile start () path System.Text.Encoding.UTF8 with
        | Success(notes, _, _) -> notes
        | Failure(errMsg, _, _) -> failwithf "%s" errMsg
    let parseNoteOnFile notePath =
        match runParserOnFile (spaces >>. pnote (manySatisfy (fun _ -> true))) () notePath System.Text.Encoding.UTF8 with
        | Success(note, _, _) -> Right note
        | Failure(errMsg, _, _) -> Left errMsg

    let test2 notesPath =
        startOnFile notesPath
        |> notesPrint
        |> uncurry System.IO.File.WriteAllText "output\\output.txt"

    // test2 "notes.txt"
    // test2() |> allTags

open Parser

let backupPath randomNotesPath =
    System.IO.Path.ChangeExtension(randomNotesPath, ".bak")
// let proxyPath = "output\\outputFalse.txt"

let split randomNotesPath tagsToPaths =
    let randomNotes : Note list = startOnFile randomNotesPath

    let splitNotOptimize tagsToPaths randomNotes =
        tagsToPaths
        |> List.mapFold (fun randomNotes (tagsNeedles, path) ->
            let currentNotes, randomNotes =
                randomNotes
                |> List.partition (fun x ->
                    x.Tags
                    |> List.exists (flip Set.contains tagsNeedles)
                    )
            (currentNotes, path), randomNotes
            ) randomNotes
    let split tagsToPaths randomNotes =
        let tagsToPaths =
            tagsToPaths
            |> List.map (fun (tags, path) -> tags, ([], path))
        randomNotes
        |> List.chooseFold (fun tagsToPaths randomNote ->
            tagsToPaths
            |> List.mapFold (fun isFound x ->
                if isFound then x, isFound
                else
                    let (tagsNeedles, (acc, path)) = x
                    randomNote.Tags
                    |> List.exists (flip Set.contains tagsNeedles)
                    |> function
                        | true ->
                            (tagsNeedles, (randomNote :: acc, path)), true
                        | false -> x, false
                ) false
            |> fun (tagsToPaths, isFound) ->
                if isFound then
                    None, tagsToPaths
                else
                    Some randomNote, tagsToPaths
            ) tagsToPaths
        |> mapSnd (
            List.map (fun (_, (notes, path)) ->
                List.rev notes, path))
    let test () =
        let notesPaths, randomNotes2 = splitNotOptimize tagsToPaths randomNotes
        let randomNotes2', notesPaths' = split tagsToPaths randomNotes
        randomNotes2 = randomNotes2' && notesPaths = notesPaths'

    let randomNotes2, notesPaths = split tagsToPaths randomNotes

    if notesPaths |> List.forall (snd >> System.IO.File.Exists) then
        notesPaths
        |> List.iter (fun (notes, path) ->
            if not <| List.isEmpty notes then
                let backupPath = System.IO.Path.ChangeExtension(path, ".bak")
                System.IO.File.Copy(path, backupPath, true)
                notesPrint notes
                |> sprintf "\n***\n%s"
                |> uncurry System.IO.File.AppendAllText path
        )
        let backupPath = System.IO.Path.ChangeExtension(randomNotesPath, ".bak")
        System.IO.File.Copy(randomNotesPath, backupPath, true)
        notesPrint randomNotes2
        |> uncurry System.IO.File.WriteAllText randomNotesPath
    else
        failwith "some file not exists"

let sort path =
    if System.IO.File.Exists path then
        let notes = startOnFile path
        let noteWithNotDate =
            notes |> List.tryFind (fun x -> Option.isNone x.DateTime)
        match noteWithNotDate with
        | Some x ->
            failwithf "без даты:\n%A" x
        | None ->
            System.IO.File.Copy(path, System.IO.Path.ChangeExtension(path, ".bak"), true)

            notes
            |> List.sortBy (fun x -> x.DateTime |> Option.get)
            |> notesPrint
            |> uncurry System.IO.File.WriteAllText path
    else
        failwithf "'%s' not found" path

let putDateTimeAtBeginAndEnd path =
    let file = System.IO.FileInfo path

    let content = System.IO.File.ReadAllLines path
    [|
        yield file.CreationTime.ToString()
        yield! content
        yield sprintf "// %A" file.LastWriteTime
    |]
    |> uncurry System.IO.File.WriteAllLines path
let addTagsUniq xs note =
    { note with
        Tags =
            xs
            |> List.fold
                (fun st x -> Set.add x st)
                (Set.ofList note.Tags)
            |> Set.toList }

open FParsec
let datetimeFileFormat (d:System.DateTime) =
    d.ToString("yyyy-MM-dd_HH-mm-ss")
assert
    (System.DateTime.Parse "18.11.2020 13:12:22" |> datetimeFileFormat = "2020-11-18_13-12-22")

let toNewWorldOrder notesDir notesPath =
    let notes = startOnFile notesPath

    let validNotes, restNotes =
        notes |> List.partition (fun note -> Option.isSome note.DateTime)

    let validNotesPaths =
        validNotes
        |> List.map (fun note ->
            let path =
                let dt = note.DateTime.Value
                datetimeFileFormat dt
                |> sprintf "%s.md"
                |> fun fileName -> System.IO.Path.Combine(notesDir, fileName)
            if System.IO.File.Exists path then
                failwithf "%A already exists" path
            path, note
        )

    validNotesPaths
    |> List.iter (fun (path, note) ->
        System.IO.File.WriteAllText(path, notePrint note)
    )
    System.IO.File.Copy(notesPath, backupPath notesPath, true)
    notesPrint restNotes
    |> uncurry System.IO.File.WriteAllText notesPath



let createTagsMap pred notesDir =
    let notesPaths = System.IO.Directory.EnumerateFiles notesDir

    let tagNotes =
        notesPaths
        |> Seq.map (fun notePath ->
            parseNoteOnFile notePath
            |> Either.map (fun note -> notePath, note)
        )
        |> Seq.seqEither
        |> Either.map (fun notes ->
            notes
            |> List.filter pred
            |> List.fold (fun st (notePath, note) ->
                let shortDscr = getShortDscr note
                let fn st tags =
                    tags
                    |> List.fold (fun st tag ->
                        let x = notePath, shortDscr
                        st
                        |> Map.addOrMod tag [x] (fun xs -> x::xs)
                    ) st
                fn st note.Tags
            ) Map.empty
        )
    tagNotes
    |> Either.map
        (Seq.map (fun (KeyValue(tag, notePaths)) ->
            notePaths
            |> List.rev
            |> List.map (fun (notePath, shortDescr) ->
                let noteUri = System.Uri notePath |> fun x -> x.AbsoluteUri
                sprintf "    * [%s](%s)" shortDescr noteUri)
            |> String.concat "\n"
            |> sprintf "* %s\n%s" tag
        )
        >> String.concat "\n"
    )

let withoutTags notesDir =
    let notesFilterBy chooser notesDir =
        let notesPaths = System.IO.Directory.EnumerateFiles notesDir

        let tagNotes =
            notesPaths
            |> Seq.map (fun notePath ->
                parseNoteOnFile notePath
                |> Either.map (fun note -> notePath, note)
            )
            |> Seq.seqEither
            |> Either.map (fun notes ->
                notes
                |> List.choose (chooser)
            )
        tagNotes
        |> Either.map (
            List.map (fun (notePath, noteShortDscr) ->
                let noteUri = System.Uri notePath |> fun x -> x.AbsoluteUri
                sprintf "* [%s](%s)" noteShortDscr noteUri)
            >> String.concat "\n"
        )
    notesDir
    |> notesFilterBy (fun (notePath, note) ->
        if List.isEmpty note.Tags then
            Some(notePath, getShortDscr note)
        else None)

let modifyNotes fn notesPath =
    let notes = startOnFile notesPath
    let notes =
        notes |> List.map fn

    System.IO.File.Copy(notesPath, backupPath notesPath, true)
    notesPrint notes
    |> uncurry System.IO.File.WriteAllText notesPath
