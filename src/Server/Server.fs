module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

let notesDir = @"i:\Notes" // TODO: вынести в config
open FsharpMyExtension
open FsharpMyExtension.Either

module Result =
    open FsharpMyExtension.Either
    let ofEither = function
        | Right x -> Ok x
        | Left x -> Error x

let notesFilterByPattern pattern =
    let notesFilterByPattern pattern =
        let regex = System.Text.RegularExpressions.Regex(pattern, System.Text.RegularExpressions.RegexOptions.IgnoreCase)

        let notesPaths = System.IO.Directory.EnumerateFiles notesDir

        notesPaths
        |> Seq.map (fun notePath ->
            MindNotes.Api.Parser.parseNoteOnFile notePath
            |> Either.map (fun note -> notePath, note)
        )
        |> Seq.seqEither
        |> Either.map (fun notes ->
            notes
            |> List.filter (fun (_, note) ->
                regex.IsMatch note.Text
            )
        )
        |> Result.ofEither
    try
        notesFilterByPattern pattern
    with
        | exp -> Error (exp.Message)

let getNote id =
    let path = id
    let fi = System.IO.FileInfo path
    if fi.Exists then
        if fi.Length > 0L then
            MindNotes.Api.Parser.parseNoteOnFile path
            |> Either.map (fun note ->
                {
                    Path = path
                    Note = note
                    Html = MarkdownConverter.toMarkdown note.Text
                }
            )
            |> Result.ofEither
        else
            {
                Path = path
                Note =
                    { DateTime = None; Tags = []; Text = "" }
                Html = ""
            }
            |> Ok
    else
        Error (sprintf "%s not found" path)
let setNote (fullNote:FullNote) =
    try
        use fileWriter = System.IO.File.OpenWrite fullNote.Path
        let str = MindNotes.Api.notePrint fullNote.Note
        let bytes = System.Text.UTF8Encoding.UTF8.GetBytes str

        use m = new System.IO.MemoryStream(bytes)
        m.WriteTo fileWriter
        { fullNote with Html = MarkdownConverter.toMarkdown fullNote.Note.Text}
        |> Ok
    with e -> Error e.Message
let newNote () =
    let dateTime = System.DateTime.Now
    let path = System.IO.Path.Combine(notesDir, sprintf "%s.md" (MindNotes.Api.datetimeFileFormat dateTime))
    try
        use file = System.IO.File.Create path
        {
            Path = path
            Html = ""
            Note =
                { DateTime = Some dateTime; Tags = []; Text = "" }
        }
        |> Ok
    with e -> Error e.Message

let api =
    {
        notesFilterByPattern = fun pattern -> async { return notesFilterByPattern pattern }
        getNote = fun id -> async { return getNote id }
        setNote = fun fullNote -> async { return setNote fullNote }
        newNote = fun () -> async { return newNote () }
    }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    |> Remoting.buildHttpHandler

let combinedApi = Giraffe.Core.choose [ webApp; Note.myApis; ]
let app =
    application {
        url "http://0.0.0.0:8085"
        use_router combinedApi
        memory_cache
        use_static "public"
        use_gzip
    }

run app
