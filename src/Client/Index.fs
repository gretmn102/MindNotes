module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type SearchState =
    {
        SearchResult: Result<(string * MindNotes.Api.Note) list,string> Deferred
        Input: string
    }
    static member Empty =
        {
            SearchResult = HasNotStartedYet
            Input = ""
        }
type Page =
    | NotePage of Result<FullNote,string> Deferred
    | SearchPage of SearchState
type State =
    {
        CurrentPage: Page
    }

type SearchMsg =
    | SetInput of string
    | Search
    | SearchResultChange of Result<(string * MindNotes.Api.Note) list,string>
type NoteId = string
type NoteMsg =
    | SetNote of Result<FullNote,string>
type Msg =
    | SearchMsg of SearchMsg
    | NoteMsg of NoteMsg
    | UrlChange of string list

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

open Feliz.Router

let parseUrl state segments =
    match segments with
    | [] ->
        let searchState =
            match state.CurrentPage with
            | SearchPage searchState -> searchState
            | _ -> SearchState.Empty
        { state with CurrentPage = SearchPage searchState}, Cmd.none
    | "notes"::noteId::_ ->
        let cmd = Cmd.OfAsync.perform todosApi.getNote noteId (SetNote >> NoteMsg)
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
            | SetInput value ->
                let state =
                    { state with
                        CurrentPage =
                            SearchPage { searchPageState with Input = value }}
                state, Cmd.none
            | Search ->
                let todo = Todo.create searchPageState.Input
                let cmd = Cmd.OfAsync.perform todosApi.notesFilterByPattern todo.Description (SearchResultChange >> SearchMsg)
                let state =
                    { state with
                        CurrentPage =
                            SearchPage
                                { searchPageState with
                                    Input = ""
                                    SearchResult = InProgress
                                }
                    }
                state, cmd
            | SearchResultChange searchResult ->
                let state =
                    { state with
                        CurrentPage =
                            SearchPage
                                { searchPageState with
                                    SearchResult = Resolved searchResult }}
                state, Cmd.none
        | x -> failwithf "expected SearchPage, but %A" x
    | UrlChange segments ->
        parseUrl state segments
    | NoteMsg x ->
        match x with
        | SetNote x ->
            { state with CurrentPage = NotePage (Resolved x)}, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let navBrand =
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
    ]

let containerBox (searchState : SearchState) (dispatch : SearchMsg -> unit) =
    Box.box' [ ] [
        Field.div [ Field.IsGrouped ] [
            Control.p [ Control.IsExpanded ] [
                Input.text [
                  Input.Value searchState.Input
                  Input.Placeholder "Find"
                  Input.OnChange (fun x -> SetInput x.Value |> dispatch) ]
            ]
            Control.p [ ] [
                Button.a [
                    Button.Color IsPrimary
                    let disabled = System.String.IsNullOrWhiteSpace searchState.Input
                    Button.Disabled disabled
                    if disabled then ()
                    else
                        Button.OnClick (fun _ -> dispatch Search)
                ] [
                    str "Add"
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
                                            Button.Props [ Href (Router.format ["notes"; path]) ]
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
                Container.container [ ] [ navBrand ]
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
                            Content.content [] [
                                p [] [str "notes"]
                                Button.a [
                                    Button.Props [ Href (Router.format "") ]
                                ] [
                                    str "Search"
                                ]

                                div [ DangerouslySetInnerHTML { __html = note.Html } ] []
                            ]
                        | Error errMsg ->
                            p [] [str errMsg]
                    | HasNotStartedYet -> p [] [str "HasNotStartedYet"]
            Feliz.React.router [
                router.onUrlChanged (UrlChange >> dispatch)
                router.children [ activePage ]
            ]
        ]
    ]
