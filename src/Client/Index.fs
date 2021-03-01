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

type SearchState =
    {
        SearchResult: Result<FullNote list,string> Deferred
        FilterPattern: FilterPattern
        InputTagsState: InputTags.State
    }
    static member Empty =
        {
            SearchResult = HasNotStartedYet
            FilterPattern = FilterPattern.Empty
            InputTagsState = InputTags.State.Empty
        }
type Page =
    | NotePage of IdForJump option * Result<NotePageState,string> Deferred
    | SearchPage of SearchState
    | TagsPage of Deferred<MindNotes.Api.Tag list>
type State =
    {
        CurrentPage: Page
        TagsSuggestions: Deferred<MindNotes.Api.Tag []>
    }

type SearchMsg =
    | ChangeSearchPattern of SearchState
    | Search
    | SearchResult of Result<FullNote list,string>
type NoteId = string
type NoteMsg =
    /// А сам `GetNote` воплощается через Router
    | GetNoteResult of Result<FullNote,string> * editMode:bool
    | SetNote
    | RemoveNote
    | RemoveNoteResult of Result<unit, string>
    | SetNoteResult of Result<FullNote, string>

    | ChangeNotePageMode of NotePageMode

type Msg =
    | SearchMsg of SearchMsg
    | NoteMsg of NoteMsg
    | ChangeUrl of string list

    | CreateNoteResult of Result<NoteId, string>
    | GetTagsResult of MindNotes.Api.Tag list
    | GetSuggestions of pattern:string
    | GetSuggestionsResult of MindNotes.Api.Tag []
let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

open Feliz.Router

[<Literal>]
let NoteRoute = "note"
[<Literal>]
let TagRoute = "tag"
[<Literal>]
let CreateNoteRoute = "createNote"

[<Literal>]
let GetTagsRoute = "getTags"

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
        let filterPattern =
            {
                SearchPattern = SearchPattern.Empty
                Tags = [tag]
            }
        let cmd =
            Cmd.OfAsync.perform todosApi.notesFilterByPattern
                filterPattern
                (SearchResult >> SearchMsg)
        let state =
            { state with
                CurrentPage =
                    SearchPage
                        {
                            FilterPattern = filterPattern
                            SearchResult = InProgress
                            InputTagsState = InputTags.State.Empty
                        }
            }
        state, cmd
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

let init(): State * Cmd<Msg> =
    let state =
        {
            CurrentPage =
                SearchPage SearchState.Empty
            TagsSuggestions = HasNotStartedYet
        }
    Router.currentUrl()
    |> parseUrl state

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | SearchMsg x ->
        match x with
        | ChangeSearchPattern value ->
            let state =
                { state with
                    CurrentPage =
                        SearchPage value
                    TagsSuggestions = HasNotStartedYet
                }
            state, Cmd.none
        | x ->
            match state.CurrentPage with
            | SearchPage searchPageState ->
                match x with
                | Search ->
                    let cmd =
                        Cmd.OfAsync.perform todosApi.notesFilterByPattern
                            searchPageState.FilterPattern
                            (SearchResult >> SearchMsg)
                    let state =
                        { state with
                            CurrentPage =
                                SearchPage
                                    { searchPageState with
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
                | ChangeSearchPattern(_) -> failwith "Not Implemented"
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
                                   let st =
                                        { notePageState with
                                            Mode =
                                                EditMode { x with SetFullNoteResult = SavingInProgress } }

                                   let cmd = Cmd.OfAsync.perform todosApi.setNote x.FullNoteTemp (SetNoteResult >> NoteMsg)
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
        let state =
            { state with
                CurrentPage = TagsPage (Resolved tags) }
        state, Cmd.none
    | GetSuggestions pattern ->
        if pattern = "" then
            let state =
                { state with
                    TagsSuggestions = Resolved [||] }
            state, Cmd.none
        else
            let cmd =
                Cmd.OfAsync.perform todosApi.getSuggestions pattern GetSuggestionsResult
            let state =
                { state with
                    TagsSuggestions = InProgress }
            state, cmd
    | GetSuggestionsResult tags ->
        let state =
            { state with
                TagsSuggestions = Resolved tags }
        state, Cmd.none
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Feliz
open Elmish.HMR.Common
let navBrand (state:State) =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            let isSearchPage =
                match state.CurrentPage with
                | SearchPage _ -> true
                | _ -> false
            Navbar.Item.IsActive isSearchPage
            if not isSearchPage then
                Navbar.Item.Props [ Href (Router.format "") ]
        ] [
            Fa.i [ Fa.Solid.Search ] []
        ]
        Navbar.Item.a [
            Navbar.Item.Props [ Href (Router.format CreateNoteRoute) ]
        ] [
            Fa.i [ Fa.Solid.FileAlt ] []
        ]
        Navbar.Item.a [
            let isActive =
                match state.CurrentPage with
                | TagsPage _ -> true
                | _ -> false
            Navbar.Item.IsActive isActive
            if not isActive then
                Navbar.Item.Props [ Href (Router.format GetTagsRoute) ]
        ] [
            Fa.i [ Fa.Solid.Tags ] []
        ]
    ]
let activatedTagsRender tags =
    tags
    |> List.map (fun x ->
        Tag.tag [
        ] [
            a [Href (Router.format [TagRoute; x])] [str x]
        ])
    |> Tag.list []

let searchBox (searchState : SearchState) tagsSuggestions getSuggestions (dispatch : SearchMsg -> unit) =
    Box.box' [ ] [
        searchState.FilterPattern.Tags
        |> InputTags.inputTags
            "inputTagsId"
            searchState.InputTagsState
            (fun tag ->
                { searchState with
                    FilterPattern =
                        { searchState.FilterPattern with
                            Tags = searchState.FilterPattern.Tags |> List.filter ((<>) tag)
                        }
                }
                |> ChangeSearchPattern
                |> dispatch
            )
            (fun st ->
                { searchState with InputTagsState = st }
                |> ChangeSearchPattern
                |> dispatch

                getSuggestions st.CurrentTag
            )
            (fun (st, tag) ->
                { searchState with
                    InputTagsState = st
                    FilterPattern =
                        { searchState.FilterPattern with
                            Tags =
                                List.foldBack
                                    (fun x st ->
                                        if x = tag then st
                                        else x::st
                                    )
                                    searchState.FilterPattern.Tags
                                    [tag]
                        }
                }
                |> ChangeSearchPattern
                |> dispatch
            )
            tagsSuggestions
        Field.div [ Field.HasAddons ] [
            let disabled =
                System.String.IsNullOrWhiteSpace searchState.FilterPattern.SearchPattern.Pattern
                && List.isEmpty searchState.FilterPattern.Tags
            Control.p [ Control.IsExpanded ] [
                Input.text [
                    Input.Value searchState.FilterPattern.SearchPattern.Pattern
                    Input.Placeholder "Find"
                    Input.OnChange (fun x ->
                        { searchState with
                            FilterPattern =
                            { searchState.FilterPattern with
                                SearchPattern =
                                    { searchState.FilterPattern.SearchPattern with
                                        Pattern = x.Value }
                            }
                        }
                        |> ChangeSearchPattern
                        |> dispatch)
                    if not disabled then
                        Input.Props [
                            OnKeyDown (fun e ->
                                if e.key = "Enter" then
                                    dispatch Search
                            )
                        ]
                ]
            ]
            Control.p [ ] [
                Button.a [
                    Button.Disabled disabled
                    if not disabled then
                        Button.OnClick (fun _ -> dispatch Search)
                ] [
                    Fa.i [ Fa.Solid.Search ] []
                ]
            ]
        ]
        div [ ] [
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
                        let f (x:FullNote) =
                            let note = x.Note
                            [
                                Level.level [
                                    Level.Level.Props [
                                        Style [
                                            MarginBottom 0
                                        ]
                                    ]
                                ] [
                                    Level.left [] [
                                        Level.item [] [
                                            Field.div [Field.HasAddons] [
                                                Control.div [] [
                                                    Button.a [
                                                        Button.Props [ Href (Router.format [NoteRoute; x.Id]) ]
                                                    ] [
                                                        str x.Path
                                                    ]
                                                ]
                                                Control.div [] [
                                                    match Browser.Navigator.navigator.clipboard with
                                                    | Some clipboard ->
                                                        Button.span [
                                                            Button.OnClick (fun _ ->
                                                                clipboard.writeText x.Path
                                                                |> ignore
                                                            )
                                                        ] [
                                                        Fa.span [ Fa.Solid.Clipboard
                                                                  Fa.FixedWidth
                                                                ]
                                                            [ ]
                                                        ]
                                                    | None -> ()
                                                ]
                                            ]
                                        ]

                                        Level.item [] [
                                            div [] [
                                                span [] [ Fa.i [ Fa.Regular.Eye ] [] ]
                                                span [] [ str (sprintf " %d" note.Views) ]
                                            ]
                                        ]
                                    ]
                                ]
                                activatedTagsRender note.Tags

                                str (MindNotes.Api.getShortDscr note)
                            ]
                        for x in xs do
                            Card.card [] [
                                Card.content [] (f x)
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

                            let getSuggestions pattern =
                                GetSuggestions pattern
                                |> dispatch
                            let tagsSuggestions =
                                match state.TagsSuggestions with
                                | Resolved xs -> xs
                                | _ -> [||]
                            searchBox searchState tagsSuggestions getSuggestions (SearchMsg >> dispatch)
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
                                                                InputTagsState = InputTags.State.Empty
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

                                    let tagsSuggestions =
                                        match state.TagsSuggestions with
                                        | Resolved xs -> xs
                                        | _ -> [||]
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

                                            GetSuggestions st.CurrentTag
                                            |> dispatch
                                        )
                                        (fun (st, tag) ->
                                            let tag =
                                                tag
                                                |> String.collect
                                                    (function
                                                     | ' ' -> "_"
                                                     | '#' -> ""
                                                     | x -> string x)
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

                                            GetSuggestions ""
                                            |> dispatch
                                        )
                                        tagsSuggestions
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
                                          match editModeState.SetFullNoteResult with
                                          | NotSaved | SavingError _ ->
                                                Textarea.Props [
                                                    OnKeyDown (fun e ->
                                                        if e.ctrlKey && e.key = "Enter" then
                                                            SetNote
                                                            |> NoteMsg
                                                            |> dispatch
                                                    )
                                                ]
                                          | Saved | SavingInProgress -> ()
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
                        |> List.map (fun tag ->
                            li [] [
                                a [Href (Router.format [TagRoute; tag])] [str tag]
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
