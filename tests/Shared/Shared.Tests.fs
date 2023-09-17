module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif
open FsharpMyExtension
open FsharpMyExtension.Either

open Shared
open Shared.MindNotes.Api

let runParser =
    #if FABLE_COMPILER
    fun p str ->
        let parserState, result = FUniversalParser.Primitives.run str p
        result |> Either.toResult
    #else
    FParsecExt.runResult
    #endif

let parserTests =
    testList "parserTests" [
        testCase "pdatetime \"10.03.2019 0:35:28\" is success" <| fun _ ->
            Expect.equal
                (runParser NoteDateTime.Parser.pdatetime "10.03.2019 0:35:28")
                (Ok (System.DateTime(2019, 3, 10, 0, 35, 28)))
                ""

        #if !FABLE_COMPILER
        testCase "pdatetimeStrange \"\\u200e6 \\u200eдекабря \\u200e2018 \\u200eг., \\u200f\\u200e14:17:27\" is success" <| fun _ ->
            Expect.equal
                (runParser NoteDateTime.Parser.pdatetimeStrange "\u200e6 \u200eдекабря \u200e2018 \u200eг., \u200f\u200e14:17:27")
                (Ok (System.DateTime(2018, 12, 6, 14, 17, 27)))
                ""
        #endif

        testCase "pdateTimeFileFormat \"2023-09-17_15-15-46\" is success" <| fun _ ->
            Expect.equal
                (runParser NoteDateTime.Parser.pdateTimeFileFormat "2023-09-17_15-15-46")
                (Ok (System.DateTime(2023, 9, 17, 15, 15, 46)))
                ""

        testCase "ptag \"#foo_bar\" = \"foo_bar\"" <| fun _ ->
            Expect.equal
                (runParser Tag.Parser.ptag "#foo_bar")
                (Ok "foo_bar")
                ""

        testCase "ptag \"#foo bar\" = \"foo\"" <| fun _ ->
            Expect.equal
                (runParser Tag.Parser.ptag "#foo bar")
                (Ok "foo")
                ""

        testCase "ptag \"#foo\\nbar\" = \"foo\"" <| fun _ ->
            Expect.equal
                (runParser Tag.Parser.ptag "#foo\nbar")
                (Ok "foo")
                ""

        testCase "pnote is success" <| fun _ ->
            Expect.equal
                (runParser (Note.Parser.parser Note.Parser.prawContent) (String.concat "\n" [
                    "10.03.2019 0:35:28"
                    "#сюжет"
                    "— А ты мне что скажешь? — нетерпеливо спросил царь."
                 ]))
                (Ok {
                    DateTime = Some (System.DateTime(2019, 3, 10, 0, 35, 28))
                    Tags = [ "сюжет" ]
                    Text = "— А ты мне что скажешь? — нетерпеливо спросил царь."
                    Views = 0
                })
                ""
    ]

let shared = testList "Shared" [
    parserTests

    testCase "Empty string is not a valid description" <| fun _ ->
        let expected = "2020-11-18_13-12-22"

        let actual =
            #if FABLE_COMPILER
            System.DateTime.Parse "11/18/2020 13:12:22"
            #else
            System.DateTime.Parse "18.11.2020 13:12:22"
            #endif
            |> NoteDateTime.serialize

        Expect.equal actual expected "Should be false"
]