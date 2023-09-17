module MindNotes.Api
open FsharpMyExtension
open FsharpMyExtension.Either

open Shared.MindNotes.Api

module SharedParser =
    open FParsec

    type 'a Parser = Parser<'a,unit>

    let skipSpaces' : _ Parser = skipManySatisfy (isAnyOf " \t")

module Tag =
    module Parser =
        open FParsec

        open SharedParser

        let ptag : _ Parser = pchar '#' >>. many1Satisfy (isNoneOf "\n ")

module TagsList =
    module Parser =
        open FParsec

        open SharedParser

        let ptags1 =
            many1 (Tag.Parser.ptag .>> skipSpaces')

module NoteDateTime =
    module Parser =
        open FParsec

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
                        let ruFormatProvider = System.Globalization.CultureInfo("ru-RU", false)
                        System.DateTime.Parse(x, ruFormatProvider)

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

module Note =
    module Parser =
        open FParsec

        open SharedParser

        let prawContent =
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

        let parseFile notePath =
            let p =
                spaces >>. parser prawContent
            match runParserOnFile p () notePath System.Text.Encoding.UTF8 with
            | Success(note, _, _) -> Right note
            | Failure(errMsg, _, _) -> Left errMsg
