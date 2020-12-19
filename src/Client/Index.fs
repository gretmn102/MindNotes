module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type SavingState =
    | NotSaved
    | Saved
    | SavingInProgress
    | SavingError of string
type EditModeState =
    {
        FullNoteTemp: FullNote
        SetFullNoteResult: SavingState
        InputTagsState: InputTags.State
    }
type NotePageMode =
    | ViewMode
    | EditMode of EditModeState

type NotePageState =
    {
        FullNote : FullNote
        Mode : NotePageMode
    }

type SearchState =
    {
        SearchResult: Result<(string * MindNotes.Api.Note) list,string> Deferred
        SearchPattern: string
    }
    static member Empty =
        {
            SearchResult = HasNotStartedYet
            SearchPattern = ""
        }
type Page =
    | NotePage of Result<NotePageState,string> Deferred
    | SearchPage of SearchState
type State =
    {
        CurrentPage: Page
    }

type SearchMsg =
    | ChangeSearchPattern of string
    | Search
    | SearchResult of Result<(string * MindNotes.Api.Note) list,string>
type NoteId = string
type NoteMsg =
    /// А сам `GetNote` воплощается через Router
    | GetNoteResult of Result<FullNote,string> * editMode:bool
    | SetNote
    | SetNoteResult of Result<FullNote, string>

    | ChangeNotePageMode of NotePageMode

type Msg =
    | SearchMsg of SearchMsg
    | NoteMsg of NoteMsg
    | ChangeUrl of string list
    | CreateNote
    | CreateNoteResult of Result<NoteId, string>

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

open Feliz.Router

[<Literal>]
let NoteRoute = "note"

let parseUrl state segments =
    match segments with
    | [] ->
        let searchState =
            match state.CurrentPage with
            | SearchPage searchState -> searchState
            | _ -> SearchState.Empty
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

            { state with CurrentPage = NotePage InProgress }, cmd
        | _ ->
            let cmd =
                Cmd.OfAsync.perform todosApi.getNote noteId
                    (fun fullNote -> GetNoteResult(fullNote, false) |> NoteMsg)
            { state with CurrentPage = NotePage InProgress }, cmd
    | _ ->
        state, Cmd.none

let init(): State * Cmd<Msg> =
    let state =
        {
            CurrentPage =
                SearchPage SearchState.Empty
        }
    Router.currentUrl()
    |> parseUrl state

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | SearchMsg x ->
        match state.CurrentPage with
        | SearchPage searchPageState ->
            match x with
            | ChangeSearchPattern value ->
                let state =
                    { state with
                        CurrentPage =
                            SearchPage { searchPageState with SearchPattern = value }}
                state, Cmd.none
            | Search ->
                let todo = Todo.create searchPageState.SearchPattern
                let cmd = Cmd.OfAsync.perform todosApi.notesFilterByPattern todo.Description (SearchResult >> SearchMsg)
                let state =
                    { state with
                        CurrentPage =
                            SearchPage
                                { searchPageState with
                                    SearchPattern = ""
                                    SearchResult = InProgress
                                }
                    }
                state, cmd
            | SearchResult searchResult ->
                let state =
                    { state with
                        CurrentPage =
                            SearchPage
                                { searchPageState with
                                    SearchResult = Resolved searchResult }}
                state, Cmd.none
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
                                    InputTagsState = InputTags.State.Empty
                                }
                                |> EditMode
                            else
                                ViewMode
                    }
                )
            { state with CurrentPage = NotePage (Resolved x)}, Cmd.none
        | SetNote ->
            let st, cmd =
                match state.CurrentPage with
                | NotePage st ->
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
                                                EditMode { x with SetFullNoteResult = SavingInProgress } }

                                   let cmd = Cmd.OfAsync.perform todosApi.setNote x.FullNoteTemp (SetNoteResult >> NoteMsg)
                                   Resolved(Ok st), cmd
                               | ViewMode -> failwith "Not Implemented"
                            | Error(errorValue) -> failwith "Not Implemented"
                        NotePage st, cmd
                    | HasNotStartedYet
                    | InProgress -> failwith "Not Implemented"
                | SearchPage(_) -> failwith "Not Implemented"
            let state =
                { state with
                    CurrentPage = st }
            state, cmd
        | SetNoteResult res ->
            let st, cmd =
                match state.CurrentPage with
                | NotePage st ->
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
                                                    }
                                                    |> EditMode
                                            }
                                   let cmd = Cmd.none
                                   Resolved(Ok st), cmd
                               | ViewMode -> failwith "Not Implemented"
                            | Error(errorValue) -> failwith "Not Implemented"
                        NotePage st, cmd
                    | HasNotStartedYet
                    | InProgress -> failwith "Not Implemented"
                | SearchPage(_) -> failwith "Not Implemented"
            let state =
                { state with
                    CurrentPage = st }
            state, cmd
        | ChangeNotePageMode mode ->
            let st, cmd =
                match state.CurrentPage with
                | NotePage st ->
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
                        NotePage st, cmd
                    | HasNotStartedYet
                    | InProgress -> failwith "Not Implemented"
                | SearchPage(_) -> failwith "Not Implemented"
            let state =
                { state with
                    CurrentPage = st }
            state, cmd
    | CreateNote ->
        let cmd =
            Cmd.OfAsync.perform todosApi.newNote ()
                // (fun fullNote -> GetNoteResult(fullNote, true) |> NoteMsg)
                (function
                    | Ok x ->
                        CreateNoteResult (Ok x.Path)
                    | Error errMsg -> CreateNoteResult (Error errMsg) )

        let state =
            { state with
                CurrentPage =
                    NotePage InProgress }
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
                        NotePage (Resolved (Error errMsg)) }
            state, Cmd.none
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let navBrand dispatch =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
        Navbar.Item.div [
        ] [
            Button.button [
                Button.OnClick (fun _ ->
                    dispatch CreateNote
                )
            ] [
                Fa.i [ Fa.Solid.FileAlt ] []
            ]
        ]
    ]

let containerBox (searchState : SearchState) (dispatch : SearchMsg -> unit) =
    Box.box' [ ] [
        Field.div [ Field.HasAddons ] [
            Control.p [ Control.IsExpanded ] [
                Input.text [
                    Input.Value searchState.SearchPattern
                    Input.Placeholder "Find"
                    Input.OnChange (fun x -> ChangeSearchPattern x.Value |> dispatch) ]
            ]
            Control.p [ ] [
                Button.a [
                    let disabled = System.String.IsNullOrWhiteSpace searchState.SearchPattern
                    Button.Disabled disabled
                    if not disabled then
                        Button.OnClick (fun _ -> dispatch Search)
                ] [
                    Fa.i [ Fa.Solid.Search ] []
                ]
            ]
        ]
        Content.content [ ] [
            match searchState.SearchResult with
            | InProgress ->
                div [ Class ("block " + Fa.Classes.Size.Fa3x) ]
                    [ Fa.i [ Fa.Solid.Spinner
                             Fa.Spin ]
                        []
                    ]
            | HasNotStartedYet -> ()
            | Resolved x ->
                match x with
                | Error x ->
                    str (sprintf "error: %A" x)
                | Ok xs ->
                    if List.isEmpty xs then
                        Text.p [] [str "not found"]
                    else
                        ul [ ] [
                            for path, note in xs do
                                li [ ] [
                                    div [] [
                                        Button.a [
                                            Button.Props [ Href (Router.format [NoteRoute; path]) ]
                                        ] [
                                            str path
                                        ]

                                        match Browser.Navigator.navigator.clipboard with
                                        | Some clipboard ->
                                            Button.span [
                                                Button.OnClick (fun _ ->
                                                    clipboard.writeText path
                                                    |> ignore
                                                )
                                            ] [
                                            Fa.span [ Fa.Solid.Clipboard
                                                      Fa.FixedWidth
                                                    ]
                                                [ ]
                                            ]
                                        | None -> ()

                                        note.Tags |> List.map (fun x -> Tag.tag [] [str x])
                                        |> Tag.list []

                                        str (MindNotes.Api.getShortDscr note)
                                    ]

                                ]
                        ]
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
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ navBrand dispatch ]
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

                            containerBox searchState (SearchMsg >> dispatch)
                        ]
                    ]
                | NotePage notePageState ->
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
                                Button.a [
                                    Button.Props [ Href (Router.format "") ]
                                ] [
                                    str "Search"
                                ]
                                Field.div [Field.HasAddons] [
                                    Control.p [] [
                                        Button.button [ Button.IsStatic true; Button.Color IsBlack ] [str note.FullNote.Path]
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

                                match note.Mode with
                                | ViewMode ->
                                    note.FullNote.Note.Tags |> List.map (fun x -> Tag.tag [] [str x])
                                    |> Tag.list []

                                    Button.button
                                        [
                                            Button.OnClick (fun _ ->
                                                {
                                                    FullNoteTemp = note.FullNote
                                                    SetFullNoteResult = Saved
                                                    InputTagsState = InputTags.State.Empty
                                                }
                                                |> EditMode
                                                |> ChangeNotePageMode
                                                |> NoteMsg
                                                |> dispatch
                                            )
                                        ]
                                        [ Fa.i [ Fa.Solid.Edit ] [] ]
                                    Content.content [] [
                                        div [ DangerouslySetInnerHTML { __html = note.FullNote.Html } ] []
                                    ]
                                | EditMode editModeState ->
                                    editModeState.FullNoteTemp.Note.Tags
                                    |> InputTags.inputTags
                                        "inputTagsId"
                                        editModeState.InputTagsState
                                        (fun tag ->
                                            { editModeState with
                                                FullNoteTemp =
                                                    { editModeState.FullNoteTemp with
                                                        Note =
                                                            { editModeState.FullNoteTemp.Note with
                                                                Tags =
                                                                    editModeState.FullNoteTemp.Note.Tags
                                                                    |> List.filter ((<>) tag) }}
                                                SetFullNoteResult = NotSaved
                                            }
                                            |> EditMode
                                            |> ChangeNotePageMode
                                            |> NoteMsg
                                            |> dispatch
                                        )
                                        (fun st ->
                                            { editModeState with
                                                InputTagsState = st
                                            }
                                            |> EditMode
                                            |> ChangeNotePageMode
                                            |> NoteMsg
                                            |> dispatch
                                        )
                                        (fun (st, tag) ->
                                            { editModeState with
                                                InputTagsState = st
                                                FullNoteTemp =
                                                    { editModeState.FullNoteTemp with
                                                        Note =
                                                            { editModeState.FullNoteTemp.Note with
                                                                Tags =
                                                                    List.foldBack
                                                                        (fun x st ->
                                                                            if x = tag then st
                                                                            else x::st
                                                                        )
                                                                        editModeState.FullNoteTemp.Note.Tags
                                                                        [tag]
                                                            }
                                                    }
                                                SetFullNoteResult = NotSaved
                                            }
                                            |> EditMode
                                            |> ChangeNotePageMode
                                            |> NoteMsg
                                            |> dispatch
                                        )

                                    Textarea.textarea
                                        [ Textarea.DefaultValue editModeState.FullNoteTemp.Note.Text
                                          Textarea.Size IsLarge
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
                                            [ str "Save" ]
                                    | Saved ->
                                        Button.button
                                            [
                                                Button.Disabled true
                                            ]
                                            [ str "Saved" ]
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

                                    Button.button
                                        [
                                            Button.OnClick (fun _ ->
                                                ViewMode
                                                |> ChangeNotePageMode
                                                |> NoteMsg
                                                |> dispatch
                                            )
                                        ]
                                        [ str "Close" ]
                            ]
                        | Error errMsg ->
                            p [] [str errMsg]
                    | HasNotStartedYet -> p [] [str "HasNotStartedYet"]
            Feliz.React.router [
                router.onUrlChanged (ChangeUrl >> dispatch)
                router.children [ activePage ]
            ]
        ]
    ]
