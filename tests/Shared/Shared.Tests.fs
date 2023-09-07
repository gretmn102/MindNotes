module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared
open Shared.MindNotes.Api

let shared = testList "Shared" [
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