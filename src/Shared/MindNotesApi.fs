module Shared.MindNotes.Api
open FsharpMyExtension.Either

module SharedParser =
    #if FABLE_COMPILER
    open FUniversalParser.Primitives
    open FUniversalParser.StringParser
    #else
    open FParsec
    #endif

    type 'a Parser = Parser<'a,unit>

    let skipSpaces' : _ Parser = skipManySatisfy (isAnyOf " \t")

type Tag = string
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Tag =
    let escape (tag: Tag) =
        tag
        |> fun x -> x.Trim()
        |> fun x -> x.Replace(" ", "_")

    module Parser =
        #if FABLE_COMPILER
        open FUniversalParser.Primitives
        open FUniversalParser.StringParser
        #else
        open FParsec
        #endif

        open SharedParser

        let ptag : _ Parser = pchar '#' >>. many1Satisfy (isNoneOf "\n ")

type TagsList = Tag list

module TagsList =
    module Parser =
        #if FABLE_COMPILER
        open FUniversalParser.Primitives
        open FUniversalParser.StringParser
        #else
        open FParsec
        #endif

        open SharedParser

        let ptags1 : TagsList Parser =
            many1 (Tag.Parser.ptag .>> skipSpaces')

type NoteDateTime = System.DateTime
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module NoteDateTime =
    module Parser =
        #if FABLE_COMPILER
        open FUniversalParser.Primitives
        open FUniversalParser.StringParser
        #else
        open FParsec
        #endif

        open SharedParser

        let pdate : _ Parser =
            notFollowedByString "#" >>. many1Satisfy (isNoneOf "\n")
            >>= fun x ->
                match System.DateTime.TryParse x with
                | true, x -> preturn x
                | _ -> fail "datetime"

        /// `10.03.2019 0:35:28`
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

        /// Дело в том, что если копировать дату-время файла с вкладки "Свойства" в Window 7, то он вставляет какие-то загадочные Unicode-разделители, выглядит это так: `\u200e6 \u200eдекабря \u200e2018 \u200eг., \u200f\u200e14:17:27`.
        let pdatetimeStrange : _ Parser =
            let leftToRightMark = '\u200e'
            let rightToLeftMark = '\u200f'
            pchar leftToRightMark
            >>. many1Strings (
                many1Satisfy (isNoneOf [leftToRightMark; rightToLeftMark; '\n'])
                <|> charReturn leftToRightMark ""
                <|> charReturn rightToLeftMark ""
            )
            >>= fun x ->
                try
                    let dateTime =
                        #if FABLE_COMPILER
                        System.DateTime.Parse(x)
                        #else
                        let ruFormatProvider = System.Globalization.CultureInfo("ru-RU", false)
                        System.DateTime.Parse(x, ruFormatProvider)
                        #endif

                    preturn dateTime
                with e ->
                    fail e.Message

        /// `yyyy-MM-dd_HH-mm-ss`
        let pdateTimeFileFormat: _ Parser =
            pipe5
                pint32
                (pchar '-' >>. pint32)
                (pchar '-' >>. pint32)
                (pchar '_' >>. pint32)
                (pchar '-' >>. pint32 .>>. (pchar '-' >>. pint32))
                (fun year month day hour (minute, sec) ->
                    System.DateTime(year, month, day, hour, minute, sec)
                )

    let serialize (dateTime: NoteDateTime) =
        dateTime.ToString("yyyy-MM-dd_HH-mm-ss")

type Note =
    {
        DateTime: NoteDateTime option
        Tags: TagsList
        Text: string
        Views: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Note =
    module Parser =
        #if FABLE_COMPILER
        open FUniversalParser.Primitives
        open FUniversalParser.StringParser
        #else
        open FParsec
        #endif

        open SharedParser

        let prawContent : _ Parser =
            manySatisfy (fun _ -> true)

        let parser pcontent : _ Parser =
            let tags =
                let p = pstring "//" >>. skipSpaces'
                (p >>? TagsList.Parser.ptags1)
                <|> (notFollowedBy (pchar '#' >>. satisfy (isAnyOf " \n")) >>. TagsList.Parser.ptags1)
                .>> skipNewline
            pipe4
                (opt (NoteDateTime.Parser.pdatetimeStrange <|> NoteDateTime.Parser.pdatetime))
                (opt (pchar '|' >>. pint32) .>> optional skipNewline)
                (opt tags)
                pcontent
                (fun datetime views tags text ->
                    {
                        DateTime = datetime
                        Views = views |> Option.defaultValue 0
                        Tags = tags |> Option.defaultValue []
                        Text = text
                    }
                )

        #if !FABLE_COMPILER
        let parseFile notePath =
            let p =
                spaces >>. parser prawContent
            match runParserOnFile p () notePath System.Text.Encoding.UTF8 with
            | Success(note, _, _) -> Right note
            | Failure(errMsg, _, _) -> Left errMsg
        #endif

    let serialize (note: Note) =
        let datetime =
            note.DateTime
            |> Option.map (fun x -> x.ToString("dd.MM.yyyy HH:mm:ss"))
            |> Option.defaultValue ""

        let tags =
            match note.Tags with
            | [] -> ""
            | xs ->
                xs |> List.map (sprintf "#%s")
                |> String.concat " " |> sprintf "%s\n"
        sprintf "%s|%d\n%s%s" datetime note.Views tags (note.Text.TrimEnd())

    let getShortInlineDescription (note:Note) =
        let dscr =
            note.Text.Replace("\n", "\\n")
                    .Replace("[","\\[")
                    .Replace("]","\\]")
        let length = 100
        if dscr.Length < length then
            dscr
        else
            sprintf "%s..." dscr.[..length - 1]

let notesPrint =
    List.map Note.serialize >> String.concat "\n***\n"

let allTags =
    List.collect (fun x -> x.Tags)
    >> Set.ofList
    >> String.concat "\n"
