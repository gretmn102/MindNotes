module Server.Tests

open Expecto

open Shared
open Server

let server =
    testList "MarkdownConverter.toMarkdown" [
        testCase "Title from <h>" <| fun _ ->
            let markdown = "# [sdf*24*](sdf)header"
            let res = MarkdownConverter.toMarkdown "noteId" markdown
            Expect.equal res.Title (Some "sdf24header") "Should be 'sdf24header'"
        testCase "Title from <p>" <| fun _ ->
            let markdown = "Some title"
            let res = MarkdownConverter.toMarkdown "noteId" markdown
            Expect.equal res.Title (Some "Some title") "Should be 'Some title'"
    ]

let all =
    testList "All"
        [
            Shared.Tests.shared
            Shared.Tests.parserTests
            server
        ]

[<EntryPoint>]
let main _ = runTests defaultConfig all