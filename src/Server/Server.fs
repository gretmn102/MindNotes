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

let notesFilter pred =
    try
        let notesPaths = System.IO.Directory.EnumerateFiles notesDir

        notesPaths
        |> Seq.map (fun notePath ->
            MindNotes.Api.Parser.parseNoteOnFile notePath
            |> Either.map (fun note -> notePath, note)
        )
        |> Seq.seqEither
        |> Either.map (fun notes ->
            notes
            |> List.filter pred
        )
        |> Result.ofEither
    with
        | exp -> Error (exp.Message)

let notesFilterByPattern (filterPattern:FilterPattern) =
    let regEx =
        let patt = filterPattern.SearchPattern.Pattern
        if System.String.IsNullOrEmpty patt then
            fun _ -> true
        else
            if filterPattern.SearchPattern.IsRegex then
                let regex =
                    if filterPattern.SearchPattern.MatchCase then
                        System.Text.RegularExpressions.Regex patt
                    else
                        System.Text.RegularExpressions.Regex
                            (patt,
                             System.Text.RegularExpressions.RegexOptions.IgnoreCase)
                fun (note:MindNotes.Api.Note) -> regex.IsMatch note.Text
            else
                fun note ->
                    if filterPattern.SearchPattern.MatchCase then
                        note.Text.Contains patt
                    else
                        note.Text.Contains
                            (patt,
                             System.StringComparison.OrdinalIgnoreCase)
    let tagsFilter =
        match filterPattern.Tags with
        | [] -> fun _ -> true
        | [tag] ->
            fun (note:MindNotes.Api.Note) ->
                List.contains tag note.Tags
        | tags ->
            fun note ->
                let set = Set.ofList note.Tags
                tags
                |> List.forall (flip Set.contains set)

    notesFilter (fun (_, note) ->
        tagsFilter note && regEx note
    )

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
        use sw = new System.IO.StreamWriter(fullNote.Path, false)
        let str = MindNotes.Api.notePrint fullNote.Note
        sw.Write str

        { fullNote with Html = MarkdownConverter.toMarkdown fullNote.Note.Text}
        |> Ok
    with e -> Error e.Message
let newNote () =
    let dateTime = System.DateTime.Now
    let path = System.IO.Path.Combine(notesDir, sprintf "%s.md" (MindNotes.Api.datetimeFileFormat dateTime))
    try
        use fs = System.IO.File.Create path

        let note: MindNotes.Api.Note =
            { Tags = []; DateTime = Some dateTime; Text = "" }
        let str = MindNotes.Api.notePrint note
        let bytes = System.Text.UTF8Encoding.UTF8.GetBytes str
        fs.Write(bytes, 0, bytes.Length)

        {
            Path = path
            Html = ""
            Note = note
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
