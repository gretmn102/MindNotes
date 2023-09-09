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

type State =
    | GetTags of AsyncReplyChannel<Map<MindNotes.Api.Tag, Shared.NoteId Set>>
    | SetTags of Map<MindNotes.Api.Tag, Shared.NoteId Set>

    | UpdateTagsFor of Shared.NoteId * MindNotes.Api.Tag Set

type TagsState =
    {
        Tags: Map<MindNotes.Api.Tag, Shared.NoteId Set>
        NoteIds: Map<Shared.NoteId, MindNotes.Api.Tag Set>
    }
    static member Empty =
        {
            Tags = Map.empty
            NoteIds = Map.empty
        }

let parseNotes (noteFiles:seq<System.IO.FileInfo>) =
    noteFiles
    |> Seq.map (fun fi ->
        let notePath = fi.FullName
        try
            MindNotes.Api.Parser.parseNoteOnFile notePath
            |> Either.map (fun note ->
                {
                    Id = pathToId notePath
                    Path = notePath
                    Note = note
                    Html = ""
                    Title = None
                    LastWriteTime = fi.LastWriteTime
                })
        with e ->
            Left e.Message
    )

let mail =
    MailboxProcessor.Start (fun r ->
        let rec loop (state:TagsState) =
            async {
                let! m = r.Receive()
                match m with
                | GetTags r ->
                    let tags =
                        if Map.isEmpty state.Tags then
                            let tags =
                                let d = System.IO.DirectoryInfo(notesDir)
                                if d.Exists then
                                    let noteFiles = d.EnumerateFiles "*"

                                    parseNotes noteFiles
                                    |> Seq.fold
                                        (fun st note ->
                                            match note with
                                            | Right note ->
                                                let st =
                                                    note.Note.Tags
                                                    |> List.fold
                                                        (fun st tag ->
                                                            st
                                                            |> Map.addOrModWith
                                                                tag
                                                                (fun () -> Set.singleton note.Id)
                                                                (Set.add note.Id)
                                                        )
                                                        st
                                                st
                                            | Left errMsg -> st
                                        )
                                        Map.empty
                                else
                                    Map.empty
                            tags
                        else
                            state.Tags
                    r.Reply tags
                    return! loop { state with Tags = tags }
                | SetTags xs ->
                    let state =
                        {
                            Tags = xs
                            NoteIds =
                                xs
                                |> Map.fold (fun st tag noteIds ->
                                    noteIds
                                    |> Set.fold (fun st noteId ->
                                        Map.addOrModWith
                                            noteId
                                            (fun () -> Set.singleton tag)
                                            (fun st -> Set.add tag st)
                                            st
                                    ) st
                                ) Map.empty
                        }
                    return! loop state
                | UpdateTagsFor(noteId, newTags) ->
                    let notesIds =
                        match Map.tryFind noteId state.NoteIds with
                        | Some oldTags ->
                            // let oldTags = Set [ "перестройка"; "коммунизм"; "люди" ]
                            // let newTags = Set [ "90-е"; "люди" ]
                            let removes = Set.difference oldTags newTags
                            let adds = Set.difference newTags oldTags

                            let st =
                                removes
                                |> Set.fold
                                    (fun st tag ->
                                        match Map.tryFind tag st with
                                        | Some noteIds ->
                                            let v = Set.remove noteId noteIds
                                            Map.add tag v st
                                        | None -> st
                                    )
                                    state.Tags
                            let st =
                                adds
                                |> Set.fold
                                    (fun st tag ->
                                        st
                                        |> Map.addOrModWith
                                            tag
                                            (fun () -> Set.singleton noteId)
                                            (Set.add noteId)
                                    )
                                    st
                            st
                        | None ->
                            newTags
                            |> Set.fold
                                (fun st tag ->
                                    st
                                    |> Map.addOrModWith
                                        tag
                                        (fun () -> Set.singleton noteId)
                                        (Set.add noteId)
                                )
                                state.Tags

                    let state =
                        {
                            Tags = notesIds
                            NoteIds = Map.add noteId newTags state.NoteIds
                        }
                    return! loop state
            }

        loop TagsState.Empty
    )
let setTags xs = mail.Post (SetTags xs)
let getTags () = mail.PostAndReply (fun r -> GetTags r)
let updateTags(noteId, tags) = mail.Post (UpdateTagsFor(noteId, tags))

let notesFilter pred =
    try
        let d = System.IO.DirectoryInfo(notesDir)
        let noteFiles = d.EnumerateFiles "*"

        parseNotes noteFiles
        |> Seq.seqEither
        |> Either.map (fun notes ->
            notes
            |> List.chooseFold
                (fun st note ->
                    let res = if pred note then Some note else None
                    let st =
                        note.Note.Tags
                        |> List.fold
                            (fun st tag ->
                                st
                                |> Map.addOrModWith
                                    tag
                                    (fun () -> Set.singleton note.Id)
                                    (Set.add note.Id)
                            )
                            st
                    res, st)
                Map.empty
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
let setNote (fullNote:FullNote) =
    let f () =
        try
            use sw = new System.IO.StreamWriter(fullNote.Path, false)
            let str = MindNotes.Api.Note.serialize fullNote.Note
            sw.Write str
            sw.Close()
            let x = MarkdownConverter.toMarkdown fullNote.Id fullNote.Note.Text

            updateTags(fullNote.Id, Set.ofList fullNote.Note.Tags)

            let res =
                { fullNote with
                    Html = x.Result
                    Title = x.Title
                    LastWriteTime =
                        System.IO.File.GetLastWriteTime fullNote.Path
                }
            res |> Right
        with e -> Left e.Message
    let fi = System.IO.FileInfo fullNote.Path
    if fi.Exists then
        if fi.LastWriteTime.Ticks - fullNote.LastWriteTime.Ticks < 10000L then
            f()
        else
            Left (sprintf "%A <> %A" fi.LastWriteTime fullNote.LastWriteTime)
    else
        f ()

let getNote id =
    let path = idToPath id
    let fi = System.IO.FileInfo path
    if fi.Exists then
        if fi.Length > 0L then
            MindNotes.Api.Parser.parseNoteOnFile path
            |> Either.bind (fun note ->
                let markdownRender = MarkdownConverter.toMarkdown id note.Text
                {
                    Id = id
                    Path = path
                    Note =
                        { note with
                            Views = note.Views + 1 }
                    Html = markdownRender.Result
                    Title = markdownRender.Title
                    LastWriteTime = fi.LastWriteTime
                }
                |> setNote
            )
        else
            updateTags(id, Set.empty)

            {
                Id = id
                Path = path
                Note =
                    { DateTime = None; Tags = []; Text = ""; Views = 0 }
                Html = ""
                Title = None
                LastWriteTime = fi.LastWriteTime
            }
            |> Right
    else
        Left (sprintf "%s not found" path)

let removeNote id =
    let path = idToPath id
    let fi = System.IO.FileInfo path
    if fi.Exists then
        try
            fi.Delete()
            Ok ()
        with e ->
            Error e.Message
    else
        Error "Note not exist"

let newNote () =
    let dateTime = System.DateTime.Now
    let id = MindNotes.Api.NoteDateTime.serialize dateTime
    let path = idToPath id
    try
        use fs = System.IO.File.Create path

        let note: MindNotes.Api.Note =
            {
                Tags = []
                DateTime = Some dateTime
                Text = ""
                Views = 0
            }
        let str = MindNotes.Api.Note.serialize note
        let bytes = System.Text.UTF8Encoding.UTF8.GetBytes str
        fs.Write(bytes, 0, bytes.Length)

        {
            Id = id
            Path = path
            Html = ""
            Title = None
            Note = note
            LastWriteTime = dateTime
        }
        |> Ok
    with e -> Error e.Message

let getSuggestions pattern =
    let r = System.Text.RegularExpressions.Regex(pattern, System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    let tags = getTags ()

    tags
    |> Seq.choose (fun (KeyValue(tag, _)) ->
        if r.IsMatch tag then
            Some tag
        else None
    )
    |> Seq.map (fun tag ->
        tag, StringComparison.ComparisonMetrics.JaroDistance(pattern, tag)
    )
    |> Seq.sortByDescending snd
    |> Seq.map fst
    |> Seq.truncate 10
    |> Array.ofSeq

let api =
    {
        notesFilterByPattern = fun pattern ->
            async {
                if System.String.IsNullOrEmpty pattern.SearchPattern.Pattern then
                    let tags = getTags ()

                    let f tags xs =
                        let rec f acc = function
                            | tag::tags' ->
                                match Map.tryFind tag tags with
                                | Some xs ->
                                    // let xs = Set[1; 2; 3]
                                    // let ys = Set[2; 3; 4]
                                    let res = Set.intersect xs acc
                                    if Set.isEmpty res then
                                        res
                                    else
                                        f res tags'
                                | None ->
                                    Set.empty
                            | [] -> acc
                        match xs with
                        | tag::tags' ->
                            match Map.tryFind tag tags with
                            | Some xs ->
                                f xs tags'
                            | None ->
                                Set.empty
                        | [] -> Set.empty

                    let result =
                        f tags pattern.Tags
                        |> Set.toSeq
                        |> Seq.map (fun x ->
                            let path = idToPath x
                            System.IO.FileInfo path
                        )
                        |> parseNotes
                    return Seq.seqEither result |> Result.ofEither
                else
                    match notesFilterByPattern pattern with
                    | Ok(notes, tags) ->
                        setTags tags
                        return Ok notes
                    | Error err ->
                        return Error err
            }
        getNote = fun id ->
            async {
                return getNote id |> Result.ofEither
            }
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

                return setNote fullNote |> Result.ofEither
            }
        removeNote = fun id -> async { return removeNote id }
        newNote = fun () -> async { return newNote () }
        getTags = fun () ->
            async {
                let tags =
                    getTags ()
                    |> Seq.map (fun (KeyValue(k, v)) -> k)
                    |> Array.ofSeq
                return tags
            }
        renameTag = fun (targetTag, destinationTag) ->
            async {
                let res =
                    match Map.tryFind targetTag (getTags ()) with
                    | Some noteIds ->
                        noteIds
                        |> Seq.map (fun noteId ->
                            getNote noteId
                            |> Either.bind (fun fullNote ->
                                { fullNote with
                                    Note =
                                        let note = fullNote.Note
                                        { note with
                                            Tags =
                                                let filter tags =
                                                    tags
                                                    |> List.filter (fun x ->
                                                        not (x = targetTag
                                                             || x = destinationTag)) // to avoid duplicates
                                                // let targetTag = "рыба"
                                                // let destinationTag = "масло"
                                                // let tags = [ "рыба"; "мясо" ]
                                                // filter tags = ["мясо"]

                                                destinationTag :: filter note.Tags
                                        }
                                }
                                |> setNote
                            )
                        )
                        |> List.ofSeq
                    | None -> [Left "not found target tag '%s'"]
                return res |> List.map Result.ofEither
            }
        getSuggestions = fun pattern -> async { return getSuggestions pattern }
    }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
