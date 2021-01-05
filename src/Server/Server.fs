module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

let notesDir = @"i:\Notes" // TODO: вынести в config
open FsharpMyExtension
open FsharpMyExtension.Either

let idToPath (id:Shared.NoteId) = System.IO.Path.Combine(notesDir, id + ".md")
let pathToId (path:string): Shared.NoteId = System.IO.Path.GetFileNameWithoutExtension path

module Result =
    open FsharpMyExtension.Either
    let ofEither = function
        | Right x -> Ok x
        | Left x -> Error x

type State =
    | GetTags of AsyncReplyChannel<MindNotes.Api.Tag Set>
    | SetTags of MindNotes.Api.Tag Set
let mail =
    MailboxProcessor.Start (fun r ->
        let rec loop state =
            async {
                let! m = r.Receive()
                match m with
                | GetTags r ->
                    r.Reply state
                    return! loop state
                | SetTags xs ->
                    return! loop xs
            }
        loop Set.empty
    )
let setTags xs = mail.Post (SetTags xs)
let getTags () = mail.PostAndReply (fun r -> GetTags r)

let notesFilter pred =
    try
        let d = System.IO.DirectoryInfo(notesDir)
        let notesPaths = d.EnumerateFiles "*"

        notesPaths
        |> Seq.map (fun fi ->
            let notePath = fi.FullName
            MindNotes.Api.Parser.parseNoteOnFile notePath
            |> Either.map (fun note ->
                {
                    Id = pathToId notePath
                    Path = notePath
                    Note = note
                    Html = ""
                    LastWriteTime = fi.LastWriteTime
                })
        )
        |> Seq.seqEither
        |> Either.map (fun notes ->
            notes
            |> List.chooseFold
                (fun st note ->
                    let res = if pred note then Some note else None
                    let st = note.Note.Tags |> List.fold (fun st x -> Set.add x st) st
                    res, st)
                Set.empty
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

    notesFilter (fun x ->
        let note = x.Note
        tagsFilter note && regEx note
    )

let getNote id =
    let path = idToPath id
    let fi = System.IO.FileInfo path
    if fi.Exists then
        if fi.Length > 0L then
            MindNotes.Api.Parser.parseNoteOnFile path
            |> Either.map (fun note ->
                {
                    Id = id
                    Path = path
                    Note = note
                    Html = MarkdownConverter.toMarkdown note.Text
                    LastWriteTime = fi.LastWriteTime
                }
            )
            |> Result.ofEither
        else
            {
                Id = id
                Path = path
                Note =
                    { DateTime = None; Tags = []; Text = "" }
                Html = ""
                LastWriteTime = fi.LastWriteTime
            }
            |> Ok
    else
        Error (sprintf "%s not found" path)
let setNote (fullNote:FullNote) =
    let f () =
        try
            use sw = new System.IO.StreamWriter(fullNote.Path, false)
            let str = MindNotes.Api.notePrint fullNote.Note
            sw.Write str
            sw.Close()

            { fullNote with
                Html = MarkdownConverter.toMarkdown fullNote.Note.Text
                LastWriteTime =
                    System.IO.File.GetLastWriteTime fullNote.Path
            }
            |> Ok
        with e -> Error e.Message
    let fi = System.IO.FileInfo fullNote.Path
    if fi.Exists then
        if fi.LastWriteTime.Ticks - fullNote.LastWriteTime.Ticks < 10000L then
            f()
        else
            Error (sprintf "%A <> %A" fi.LastWriteTime fullNote.LastWriteTime)
    else
        f ()

let newNote () =
    let dateTime = System.DateTime.Now
    let id = MindNotes.Api.datetimeFileFormat dateTime
    let path = idToPath id
    try
        use fs = System.IO.File.Create path

        let note: MindNotes.Api.Note =
            { Tags = []; DateTime = Some dateTime; Text = "" }
        let str = MindNotes.Api.notePrint note
        let bytes = System.Text.UTF8Encoding.UTF8.GetBytes str
        fs.Write(bytes, 0, bytes.Length)

        {
            Id = id
            Path = path
            Html = ""
            Note = note
            LastWriteTime = dateTime
        }
        |> Ok
    with e -> Error e.Message

let api =
    {
        notesFilterByPattern = fun pattern ->
            async {
                match notesFilterByPattern pattern with
                | Ok(notes, tags) ->
                    setTags tags
                    return Ok notes
                | Error err ->
                    return Error err
            }
        getNote = fun id -> async { return getNote id }
        setNote = fun fullNote ->
            async {
                let fullNote =
                    match fullNote.Note.DateTime with // local time is sent to the client, but the client returns UTC
                    | Some dateTimeUniversal ->
                        { fullNote with
                            Note =
                                { fullNote.Note with
                                    DateTime = Some (dateTimeUniversal.ToLocalTime()) }
                        }
                    | None -> fullNote
                return setNote fullNote
            }
        newNote = fun () -> async { return newNote () }
        getTags = fun () -> async { return getTags () |> Set.toList }
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
