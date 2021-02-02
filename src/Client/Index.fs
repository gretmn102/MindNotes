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
    | NotePage of Result<NotePageState,string> Deferred
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

            { state with CurrentPage = NotePage InProgress }, cmd
        | _ ->
            let cmd =
                Cmd.OfAsync.perform todosApi.getNote noteId
                    (fun fullNote -> GetNoteResult(fullNote, false) |> NoteMsg)
            { state with CurrentPage = NotePage InProgress }, cmd
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
                    NotePage InProgress }
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
                | TagsPage(_) -> failwith "Not Implemented"
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
                | TagsPage(_) -> failwith "Not Implemented"
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
                | TagsPage(_) -> failwith "Not Implemented"
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
                        NotePage (Resolved (Error errMsg)) }
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
                            for x in xs do
                                let note = x.Note
                                li [ ] [
                                    div [] [
                                        Button.a [
                                            Button.Props [ Href (Router.format [NoteRoute; x.Id]) ]
                                        ] [
                                            str x.Path
                                        ]

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

                                        activatedTagsRender note.Tags

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
                                Level.level [
                                ] [
                                    Level.left [] [

                                        Level.item [] [
                                            Field.div [Field.HasAddons] [
                                                Control.p [] [
                                                    Button.button [ Button.IsStatic true; Button.Color IsBlack ] [
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
                                        if note.Mode = ViewMode then
                                            Level.item [] [
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
                                            ]
                                    ]
                                ]
                                match note.Mode with
                                | ViewMode ->
                                    activatedTagsRender note.FullNote.Note.Tags

                                    Content.content [] [
                                        div [ DangerouslySetInnerHTML { __html = note.FullNote.Html } ] []
                                    ]
                                | EditMode editModeState ->
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
