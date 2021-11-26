module Index

open Elmish

open Shared
open Utils

open Feliz.Router

[<Literal>]
let CreateNoteRoute = "createNote"
[<Literal>]
let GetTagsRoute = "getTags"

open Fulma.Extensions

type SavingState =
    | NotSaved
    | Saved
    | SavingInProgress
    | SavingError of string
type RemovingState =
    | NotRemoved
    | Removed
    | RemovingInProgress
    | RemovingError of string
type EditModeState =
    {
        FullNoteTemp: FullNote
        SetFullNoteResult: SavingState
        InputTagsState: InputTags.State
        RemoveResult: RemovingState
    }
type NotePageMode =
    | ViewMode
    | EditMode of EditModeState
type IdForJump = string

type NotePageState =
    {
        FullNote : FullNote
        Mode : NotePageMode
    }

type Page =
    | NotePage of IdForJump option * Result<NotePageState,string> Deferred
    | SearchPage of SearchPage.SearchState
    | TagsPage of Deferred<TagsPage.State []>

type State =
    {
        CurrentPage: Page
    }

type NoteId = string
type NoteMsg =
    /// А сам `GetNote` воплощается через Router
    | GetNoteResult of Result<FullNote,string> * editMode:bool
    | SetNote
    | RemoveNote
    | RemoveNoteResult of Result<unit, string>
    | SetNoteResult of Result<FullNote, string>

    | ChangeNotePageMode of NotePageMode
    | ChangeNotePageTags of InputTags.Msg

type Msg =
    | SearchMsg of SearchPage.SearchMsg
    | NoteMsg of NoteMsg
    | ChangeUrl of string list

    | CreateNoteResult of Result<NoteId, string>
    | GetTagsResult of MindNotes.Api.Tag []

    | TagRenameMsg of int * TagsPage.Msg

let parseUrl state segments =
    match segments with
    | [] ->
        let searchState =
            match state.CurrentPage with
            | SearchPage searchState -> searchState
            | _ -> SearchPage.init []

        { state with CurrentPage = SearchPage searchState}, Cmd.none
    | NoteRoute::noteId::query ->
        match query with
        | [Route.Query (("mode", mode)::_)] ->
            let editMode =
                match mode with
                | "edit" -> true
                | _ -> false
            let cmd =
                Cmd.OfAsync.perform todosApi.getNote noteId
                    (fun fullNote -> GetNoteResult(fullNote, editMode) |> NoteMsg)

            { state with CurrentPage = NotePage (None, InProgress) }, cmd
        | xs ->
            let idForJump =
                match xs with
                | x::_ ->
                    if x.StartsWith '#' then
                        Some x
                    else
                        None
                | _ -> None
            let cmd =
                Cmd.OfAsync.perform todosApi.getNote noteId
                    (fun fullNote -> GetNoteResult(fullNote, false) |> NoteMsg)
            { state with CurrentPage = NotePage (idForJump, InProgress) }, cmd

    | TagRoute::tag::_ ->
        let searchState = SearchPage.SearchState.Init [tag]
        let searchState, cmd = SearchPage.search searchState
        let state =
            { state with
                CurrentPage =
                    SearchPage searchState
            }
        state, cmd |> Cmd.map SearchMsg

    | CreateNoteRoute::_ ->
        let cmd =
            Cmd.OfAsync.perform todosApi.newNote ()
                (function
                    | Ok x ->
                        CreateNoteResult (Ok x.Id)
                    | Error errMsg -> CreateNoteResult (Error errMsg) )

        let state =
            { state with
                CurrentPage =
                    NotePage (None, InProgress) }
        state, cmd
    | GetTagsRoute::_ ->
        let cmd =
            Cmd.OfAsync.perform todosApi.getTags () GetTagsResult

        let state =
            { state with
                CurrentPage =
                    TagsPage InProgress }
        state, cmd
    | _ ->
        state, Cmd.none

let init (): State * Cmd<Msg> =
    let state =
        {
            CurrentPage =
                SearchPage (SearchPage.init [])
        }
    Router.currentUrl()
    |> parseUrl state

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | SearchMsg msg ->
        match state.CurrentPage with
        | SearchPage searchPageState ->
            let state', cmd = SearchPage.update searchPageState msg
            let state =
                { state with
                    CurrentPage =
                        SearchPage state'
                }
            state, cmd |> Cmd.map SearchMsg

        | x -> failwithf "expected SearchPage, but %A" x

    | ChangeUrl segments ->
        parseUrl state segments
    | NoteMsg x ->
        match x with
        | GetNoteResult(fullNoteResult, editMode) ->
            let x =
                fullNoteResult
                |> Result.map (fun fullNote ->
                    {
                        FullNote = fullNote
                        Mode =
                            if editMode then
                                {
                                    FullNoteTemp = fullNote
                                    SetFullNoteResult = Saved
                                    InputTagsState = InputTags.init fullNote.Note.Tags
                                    RemoveResult = NotRemoved
                                }
                                |> EditMode
                            else
                                ViewMode
                    }
                )
            let idForJump =
                match state.CurrentPage with
                | NotePage(idForJump, _) -> idForJump
                | _ -> None
            { state with CurrentPage = NotePage (idForJump, Resolved x)}, Cmd.none
        | SetNote ->
            let st, cmd =
                match state.CurrentPage with
                | NotePage(idForJump, st) ->
                    match st with
                    | Resolved st ->
                        let st, cmd =
                            match st with
                            | Ok notePageState ->
                                match notePageState.Mode with
                                | EditMode x ->
                                    let fullNoteTemp =
                                        { x.FullNoteTemp with
                                            Note =
                                                { x.FullNoteTemp.Note with
                                                    Tags = x.InputTagsState.InputTagsState.Tags
                                                }
                                        }

                                    let st =
                                        { notePageState with
                                            Mode =
                                                EditMode
                                                    { x with
                                                        FullNoteTemp = fullNoteTemp
                                                        SetFullNoteResult = SavingInProgress }
                                        }

                                    let cmd = Cmd.OfAsync.perform todosApi.setNote fullNoteTemp (SetNoteResult >> NoteMsg)
                                    Resolved(Ok st), cmd
                                | ViewMode -> failwith "Not Implemented"
                            | Error(errorValue) -> failwith "Not Implemented"
                        NotePage (idForJump, st), cmd
                    | HasNotStartedYet
                    | InProgress -> failwith "Not Implemented"
                | SearchPage(_) -> failwith "Not Implemented"
                | TagsPage(_) -> failwith "Not Implemented"
            let state =
                { state with
                    CurrentPage = st }
            state, cmd
        | SetNoteResult res ->
            let st, cmd =
                match state.CurrentPage with
                | NotePage(idForJump, st) ->
                    match st with
                    | Resolved st ->
                        let st, cmd =
                            match st with
                            | Ok notePageState ->
                               match notePageState.Mode with
                               | EditMode x ->
                                   let st =
                                        match res with
                                        | Error msg ->
                                            { notePageState with
                                                Mode =
                                                    { x with
                                                        SetFullNoteResult = SavingError msg }
                                                    |> EditMode
                                            }
                                        | Ok fullNote ->
                                            { notePageState with
                                                FullNote = fullNote
                                                Mode =
                                                    { x with
                                                        FullNoteTemp = fullNote
                                                        SetFullNoteResult = Saved
                                                        RemoveResult = NotRemoved
                                                    }
                                                    |> EditMode
                                            }
                                   let cmd = Cmd.none
                                   Resolved(Ok st), cmd
                               | ViewMode -> failwith "Not Implemented"
                            | Error(errorValue) -> failwith "Not Implemented"
                        NotePage(idForJump, st), cmd
                    | HasNotStartedYet
                    | InProgress -> failwith "Not Implemented"
                | SearchPage(_) -> failwith "Not Implemented"
                | TagsPage(_) -> failwith "Not Implemented"
            let state =
                { state with
                    CurrentPage = st }
            state, cmd
        | ChangeNotePageMode mode ->
            let st, cmd =
                match state.CurrentPage with
                | NotePage(idForJump, st) ->
                    match st with
                    | Resolved st ->
                        let st, cmd =
                            match st with
                            | Ok notePageState ->
                               let st =
                                    { notePageState with
                                        Mode = mode
                                    }
                               let cmd = Cmd.none
                               Resolved(Ok st), cmd
                            | Error(errorValue) -> failwith "Not Implemented"
                        NotePage(None, st), cmd
                    | HasNotStartedYet
                    | InProgress -> failwith "Not Implemented"
                | SearchPage(_) -> failwith "Not Implemented"
                | TagsPage(_) -> failwith "Not Implemented"
            let state =
                { state with
                    CurrentPage = st }
            state, cmd
        | RemoveNote ->
            let st, cmd =
                match state.CurrentPage with
                | NotePage(idForJump, st) ->
                    match st with
                    | Resolved st ->
                        let st, cmd =
                            match st with
                            | Ok notePageState ->
                               match notePageState.Mode with
                               | EditMode x ->
                                   let st =
                                        { notePageState with
                                            Mode =
                                                EditMode { x with RemoveResult = RemovingInProgress } }

                                   let cmd = Cmd.OfAsync.perform todosApi.removeNote notePageState.FullNote.Id (RemoveNoteResult >> NoteMsg)
                                   Resolved(Ok st), cmd
                               | ViewMode -> Resolved(Ok notePageState), Cmd.none
                            | x ->
                                Resolved x, Cmd.none
                        NotePage(idForJump, st), cmd
                    | x -> NotePage(idForJump, st), Cmd.none
                | SearchPage(_)
                | TagsPage(_) as x -> x, Cmd.none
            let state =
                { state with
                    CurrentPage = st }
            state, cmd
        | RemoveNoteResult res ->
            let st, cmd =
                match state.CurrentPage with
                | NotePage(idForJump, st) ->
                    match st with
                    | Resolved st ->
                        let st, cmd =
                            match st with
                            | Ok notePageState ->
                                match notePageState.Mode with
                                | EditMode x ->
                                    let st =
                                        match res with
                                        | Error msg ->
                                            { notePageState with
                                                Mode =
                                                    { x with
                                                        RemoveResult = RemovingError msg
                                                    }
                                                    |> EditMode
                                            }
                                        | Ok () ->
                                            { notePageState with
                                                Mode =
                                                    { x with
                                                        RemoveResult = Removed
                                                        SetFullNoteResult = NotSaved
                                                    }
                                                    |> EditMode
                                            }
                                    let cmd = Cmd.none
                                    Resolved(Ok st), cmd
                                | ViewMode ->
                                    Resolved(Ok notePageState), Cmd.none
                            | Error _ as x ->
                                Resolved x, Cmd.none
                        NotePage(idForJump, st), cmd
                    | x -> NotePage(idForJump, st), Cmd.none
                | SearchPage(_)
                | TagsPage(_) as x -> x, Cmd.none
            let state =
                { state with
                    CurrentPage = st }
            state, cmd

        | ChangeNotePageTags msg ->
            let st, cmd =
                match state.CurrentPage with
                | NotePage(idForJump, st) ->
                    match st with
                    | Resolved st ->
                        let st, cmd =
                            match st with
                            | Ok notePageState ->
                                match notePageState.Mode with
                                | EditMode x ->
                                    let normalizeTag =
                                        String.collect
                                            (function
                                             | ' ' -> "_"
                                             | '#' -> ""
                                             | x -> string x)

                                    let msg =
                                        match msg with
                                        | InputTags.SetInputTagsState state ->
                                            InputTags.SetInputTagsState
                                                { state with
                                                    CurrentTag =
                                                        normalizeTag state.CurrentTag
                                                }
                                        | x -> x

                                    let state', cmd =
                                        InputTags.update
                                            todosApi.getSuggestions
                                            msg
                                            x.InputTagsState

                                    let state =
                                        { notePageState with
                                            Mode =
                                                EditMode
                                                    { x with
                                                        SetFullNoteResult = NotSaved
                                                        InputTagsState = state'
                                                    }
                                        }

                                    let cmd =
                                        Cmd.map (ChangeNotePageTags >> NoteMsg) cmd

                                    Resolved(Ok state), cmd
                                | ViewMode ->
                                    Resolved(Ok notePageState), Cmd.none
                            | Error _ as x ->
                                Resolved x, Cmd.none
                        NotePage(idForJump, st), cmd
                    | x -> NotePage(idForJump, st), Cmd.none
                | SearchPage(_)
                | TagsPage(_) as x -> x, Cmd.none
            let state =
                { state with
                    CurrentPage = st }
            state, cmd

    | CreateNoteResult noteIdResult ->
        match noteIdResult with
        | Ok noteId ->
            let cmd =
                Feliz.Router.Cmd.navigate
                    [|
                        NoteRoute
                        noteId
                        Router.encodeQueryString ["mode", "edit"]
                    |]
            state, cmd
        | Error errMsg ->
            let state =
                { state with
                    CurrentPage =
                        NotePage (None, Resolved (Error errMsg)) }
            state, Cmd.none
    | GetTagsResult tags ->
        let tags =
            tags
            |> Array.map TagsPage.init
        let state =
            { state with
                CurrentPage = TagsPage (Resolved tags) }
        state, Cmd.none
    | TagRenameMsg(idx, msg) ->
        match state.CurrentPage with
        | TagsPage (Resolved tags) ->
            let inputTagsState, cmd =
                TagsPage.update msg tags.[idx]
            let state =
                { state with
                    CurrentPage =
                        tags.[idx] <- inputTagsState
                        TagsPage (Resolved tags)
                }
            let cmd = Cmd.map (fun msg -> TagRenameMsg (idx, msg)) cmd
            state, cmd
        | _ -> state, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Feliz

let navBrand (state:State) =
    Navbar.Brand.div [ ] [
        let isNoteSaved =
            match state.CurrentPage with
            | NotePage(_, s) ->
                match s with
                | Resolved (Ok x) ->
                    match x.Mode with
                    | EditMode x ->
                        match x.SetFullNoteResult with
                        | NotSaved -> false
                        | Saved -> true
                        | SavingInProgress -> false
                        | SavingError(_) -> true
                    | _ -> true
                | _ -> true
            | _ -> true

        Navbar.Item.a [
            let isSearchPage =
                match state.CurrentPage with
                | SearchPage _ -> true
                | _ -> false
            Navbar.Item.IsActive isSearchPage
            Navbar.Item.Props [
                if not isSearchPage && isNoteSaved then
                    Href (Router.format "")
            ]
        ] [
            Fa.i [ Fa.Solid.Search ] []
        ]
        Navbar.Item.a [
            Navbar.Item.Props [
                if isNoteSaved then
                    Href (Router.format CreateNoteRoute)
            ]
        ] [
            Fa.i [ Fa.Solid.FileAlt ] []
        ]
        Navbar.Item.a [
            let isActive =
                match state.CurrentPage with
                | TagsPage _ -> true
                | _ -> false
            Navbar.Item.IsActive isActive
            Navbar.Item.Props [
                if not isActive && isNoteSaved then
                    Href (Router.format GetTagsRoute)
            ]
        ] [
            Fa.i [ Fa.Solid.Tags ] []
        ]
    ]

let view (state : State) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.IsFullHeight
        Hero.Props [
            Style [
                // Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
            Ref (fun x ->
                if isNull x then ()
                else
                    match state.CurrentPage with
                    | NotePage(_, r) ->
                        match r with
                        | Resolved(Ok x) ->
                            x.FullNote.Title
                            |> Option.iter (fun x ->
                                Browser.Dom.document.title <- x
                            )
                        | _ ->
                            Browser.Dom.document.title <- "Loading..."
                    | SearchPage _ ->
                        Browser.Dom.document.title <- "Search"
                    | TagsPage _ ->
                        Browser.Dom.document.title <- "Tags"
            )
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ navBrand state ]
            ]
        ]

        Hero.body [ ] [
            let activePage =
                match state.CurrentPage with
                | SearchPage searchState ->
                    Container.container [ ] [
                        Column.column [
                            Column.Width (Screen.All, Column.Is6)
                            Column.Offset (Screen.All, Column.Is3)
                        ] [
                            Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "SAFE" ]

                            SearchPage.searchBox searchState (SearchMsg >> dispatch)
                        ]
                    ]
                | NotePage(idForJump, notePageState) ->
                    match notePageState with
                    | InProgress ->
                        div [ Class ("block " + Fa.Classes.Size.Fa3x) ]
                            [ Fa.i [ Fa.Solid.Spinner
                                     Fa.Spin ]
                                []
                            ]
                    | Resolved res ->
                        match res with
                        | Ok note ->
                            Container.container [] [
                                Level.level [
                                ] [
                                    Level.left [] [
                                        Level.item [] [
                                            Field.div [Field.HasAddons] [
                                                Control.p [] [
                                                    Button.button [
                                                        Button.IsStatic true
                                                        Button.Color IsBlack
                                                        Button.Props [ TabIndex -1 ]
                                                    ] [
                                                        match note.FullNote.Note.DateTime with
                                                        | Some dateTime ->
                                                            str (dateTime.ToString("dd.MM.yyyy HH:mm:ss"))
                                                        | None ->
                                                            str note.FullNote.Path
                                                    ]
                                                ]
                                                match Browser.Navigator.navigator.clipboard with
                                                | Some clipboard ->
                                                    Control.p [] [
                                                        Button.button [
                                                            Button.OnClick (fun _ ->
                                                                clipboard.writeText note.FullNote.Path
                                                                |> ignore
                                                            )
                                                        ] [
                                                            Fa.span [ Fa.Solid.Clipboard
                                                                      Fa.FixedWidth
                                                                    ]
                                                                [ ]
                                                        ]
                                                    ]
                                                | None -> ()
                                            ]
                                        ]
                                        match note.Mode with
                                        | ViewMode ->
                                            Level.item [] [
                                                Button.button
                                                    [
                                                        Button.OnClick (fun _ ->
                                                            {
                                                                FullNoteTemp = note.FullNote
                                                                SetFullNoteResult = Saved
                                                                InputTagsState = InputTags.init note.FullNote.Note.Tags
                                                                RemoveResult = NotRemoved
                                                            }
                                                            |> EditMode
                                                            |> ChangeNotePageMode
                                                            |> NoteMsg
                                                            |> dispatch
                                                        )
                                                    ]
                                                    [ Fa.i [ Fa.Solid.Edit ] [] ]
                                            ]
                                        | EditMode editModeState ->
                                            Level.item [] [
                                                Button.button
                                                    [
                                                        match editModeState.SetFullNoteResult with
                                                        | Saved ->
                                                            Button.OnClick (fun _ ->
                                                                ViewMode
                                                                |> ChangeNotePageMode
                                                                |> NoteMsg
                                                                |> dispatch
                                                            )
                                                        | _ ->
                                                            Button.Disabled true
                                                    ]
                                                    [ Fa.i [ Fa.Regular.WindowClose ] [] ]
                                            ]
                                        Level.item [] [
                                            div [] [
                                                span [] [ Fa.i [ Fa.Regular.Eye ] [] ]
                                                span [] [ str (sprintf " %d" note.FullNote.Note.Views) ]
                                            ]
                                        ]
                                    ]
                                    Level.right [] [
                                        Level.item [] [
                                            Field.div [] [
                                                Field.label [] [ str "Last write time:" ]
                                                Field.body [] [
                                                    Button.button [
                                                        Button.IsStatic true
                                                        Button.Color IsBlack
                                                        Button.Props [ TabIndex -1 ]
                                                    ] [
                                                        let dateTime = note.FullNote.LastWriteTime
                                                        str (dateTime.ToString("dd.MM.yyyy HH:mm:ss"))
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                                match note.Mode with
                                | ViewMode ->
                                    activatedTagsRender note.FullNote.Note.Tags

                                    Content.content [] [
                                        div [
                                            DangerouslySetInnerHTML { __html = note.FullNote.Html }
                                            match idForJump with
                                            | Some idForJump ->
                                                Ref (fun e ->
                                                    match e with
                                                    | null -> ()
                                                    | e ->
                                                        match e.querySelector idForJump with
                                                        | null -> ()
                                                        | e ->
                                                            let e = e :?> Browser.Types.HTMLElement
                                                            e.scrollIntoView()
                                                )
                                            | None -> ()
                                        ] []
                                    ]
                                | EditMode editModeState ->
                                    match editModeState.SetFullNoteResult with
                                    | NotSaved ->
                                        Browser.Dom.window.onbeforeunload <- (fun e ->
                                            Browser.Dom.window.confirm ()
                                        )
                                    | _ ->
                                        Browser.Dom.window.onbeforeunload <- ignore

                                    InputTags.view
                                        editModeState.InputTagsState
                                        (ChangeNotePageTags >> NoteMsg >> dispatch)

                                    Textarea.textarea
                                        [ Textarea.DefaultValue editModeState.FullNoteTemp.Note.Text
                                          Textarea.Size IsLarge
                                          Textarea.Props [ Rows 8 ]
                                          Textarea.OnChange (fun x ->
                                            { editModeState with
                                                FullNoteTemp =
                                                    { editModeState.FullNoteTemp with
                                                        Note = { editModeState.FullNoteTemp.Note
                                                                    with Text = x.Value }}
                                                SetFullNoteResult = NotSaved
                                            }
                                            |> EditMode
                                            |> ChangeNotePageMode
                                            |> NoteMsg
                                            |> dispatch
                                          )
                                          Textarea.Props [
                                              OnKeyDown (fun e ->
                                                  let key =
                                                      // Why just not use `e.key`?
                                                      System.Convert.ToChar(e.which)
                                                      |> System.Char.ToLower
                                                  if e.ctrlKey && key = 's' then
                                                      e.preventDefault()

                                                      match editModeState.SetFullNoteResult with
                                                      | NotSaved | SavingError _ ->
                                                          SetNote
                                                          |> NoteMsg
                                                          |> dispatch
                                                      | Saved | SavingInProgress -> ()
                                              )
                                          ]
                                        ] []
                                    match editModeState.SetFullNoteResult with
                                    | NotSaved ->
                                        Button.button
                                            [
                                                Button.OnClick (fun _ ->
                                                    SetNote
                                                    |> NoteMsg
                                                    |> dispatch
                                                )
                                            ]
                                            [  Fa.i [ Fa.Solid.Save ] [] ]
                                    | Saved ->
                                        Button.button
                                            [
                                                Button.Disabled true
                                            ]
                                            [  Fa.i [ Fa.Solid.Save ] [] ]
                                    | SavingInProgress ->
                                        Button.button
                                            [
                                                Button.IsLoading true
                                            ]
                                            []
                                    | SavingError errMsg ->
                                        Button.button
                                            [
                                                Button.Color IsDanger
                                                Button.OnClick (fun _ ->
                                                    SetNote
                                                    |> NoteMsg
                                                    |> dispatch
                                                )
                                            ]
                                            [ str errMsg ]

                                    match editModeState.RemoveResult with
                                    | NotRemoved ->
                                        Button.button
                                            [
                                                Button.Color IsDanger
                                                Button.OnClick (fun _ ->
                                                    RemoveNote
                                                    |> NoteMsg
                                                    |> dispatch
                                                )
                                            ]
                                            [ Fa.i [ Fa.Solid.Recycle ] [] ]
                                    | Removed ->
                                        Button.button
                                            [
                                                Button.Disabled true
                                            ]
                                            [ Fa.i [ Fa.Solid.Recycle ] [] ]
                                    | RemovingInProgress ->
                                        Button.button
                                            [
                                                Button.IsLoading true
                                            ]
                                            []
                                    | RemovingError errMsg ->
                                        Button.button
                                            [
                                                Button.Color IsDanger
                                                Button.OnClick (fun _ ->
                                                    SetNote
                                                    |> NoteMsg
                                                    |> dispatch
                                                )
                                            ]
                                            [ str errMsg ]
                            ]
                        | Error errMsg ->
                            p [] [str errMsg]
                    | HasNotStartedYet -> p [] [str "HasNotStartedYet"]
                | TagsPage x ->
                    match x with
                    | Resolved tags ->
                        tags
                        |> Array.mapi (fun idx tag ->
                            li [] [
                                TagsPage.view tag (fun msg -> TagRenameMsg(idx, msg) |> dispatch)
                            ]
                        )
                        |> ul []
                    | InProgress ->
                        div [ Class ("block " + Fa.Classes.Size.Fa3x) ]
                            [ Fa.i [ Fa.Solid.Spinner
                                     Fa.Spin ]
                                []
                            ]
                    | HasNotStartedYet -> p [] [str "HasNotStartedYet"]
            Feliz.React.router [
                router.onUrlChanged (ChangeUrl >> dispatch)
                router.children [ activePage ]
            ]
        ]
    ]
