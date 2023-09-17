module Shared.Tests

open Expecto
open FsharpMyExtension

open MindNotes.Api

let parserTests =
    testList "parserTests" [
        testCase "pdatetime \"10.03.2019 0:35:28\" is success" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult NoteDateTime.Parser.pdatetime "10.03.2019 0:35:28")
                (Ok (System.DateTime(2019, 3, 10, 0, 35, 28)))
                ""

        testCase "pdatetimeStrange \"\\u200e6 \\u200eдекабря \\u200e2018 \\u200eг., \\u200f\\u200e14:17:27\" is success" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult NoteDateTime.Parser.pdatetimeStrange "\u200e6 \u200eдекабря \u200e2018 \u200eг., \u200f\u200e14:17:27")
                (Ok (System.DateTime(2018, 12, 6, 14, 17, 27)))
                ""

        testCase "pdateTimeFileFormat \"2023-09-17_15-15-46\" is success" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult NoteDateTime.Parser.pdateTimeFileFormat "2023-09-17_15-15-46")
                (Ok (System.DateTime(2023, 9, 17, 15, 15, 46)))
                ""

        testCase "ptag \"#foo_bar\" = \"foo_bar\"" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult Tag.Parser.ptag "#foo_bar")
                (Ok "foo_bar")
                ""

        testCase "ptag \"#foo bar\" = \"foo\"" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult Tag.Parser.ptag "#foo bar")
                (Ok "foo")
                ""

        testCase "ptag \"#foo\\nbar\" = \"foo\"" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult Tag.Parser.ptag "#foo\nbar")
                (Ok "foo")
                ""

        testCase "ptext \"* foo\" is success" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult NotesList.Parser.pcontentWithSeparator "* foo")
                (Ok "* foo")
                ""

        testCase "ptext \"**foo\" is success" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult NotesList.Parser.pcontentWithSeparator "**foo")
                (Ok "**foo")
                ""

        testCase "ptext \"***foo\" is success" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult NotesList.Parser.pcontentWithSeparator "***foo")
                (Ok "***foo")
                ""

        testCase "ptext \"**\\nfoo\\n***\\nbar\" is success" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult NotesList.Parser.pcontentWithSeparator "**\nfoo\n***\nbar")
                (Ok "**\nfoo\n")
                ""

        testCase "ptext \"***\\nfoo\\nbar\" is fail" <| fun _ ->
            Expect.equal
                (Result.isError <| FParsecExt.runResult NotesList.Parser.pcontentWithSeparator "***\nfoo\nbar")
                true
                ""

        testCase "pnote is success" <| fun _ ->
            Expect.equal
                (FParsecExt.runResult (Note.Parser.parser NotesList.Parser.pcontentWithSeparator) (String.concat "\n" [
                    "\u200e6 \u200eдекабря \u200e2018 \u200eг., \u200f\u200e14:17:27"
                    "#сюжет"
                    "— А ты мне что скажешь? — нетерпеливо спросил царь."
                 ]))
                (Ok {
                    DateTime = Some (System.DateTime (2018, 12, 6, 14, 17, 27))
                    Tags = [ "сюжет" ]
                    Text = "— А ты мне что скажешь? — нетерпеливо спросил царь."
                    Views = 0
                })
                ""
    ]
