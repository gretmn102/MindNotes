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

type Page =
    | NotePage of NotePage.State
    | SearchPage of SearchPage.SearchState
    | TagsPage of Deferred<TagsPage.State []>

type State =
    {
        CurrentPage: Page
    }

type Msg =
    | SearchMsg of SearchPage.SearchMsg
    | NoteMsg of NotePage.NoteMsg
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
                    (fun fullNote -> NotePage.GetNoteResult(fullNote, editMode) |> NoteMsg)

            let state =
                { state with
                    CurrentPage =
                        NotePage
                            {
                                IdForJump = None
                                Content = InProgress
                            }
                }
            state, cmd

        | xs ->
            let idForJump =
                match xs with
                | x::_ ->
                    if x.StartsWith "#" then
                        Some x
                    else
                        None
                | _ -> None

            let cmd =
                Cmd.OfAsync.perform todosApi.getNote noteId
                    (fun fullNote -> NotePage.GetNoteResult(fullNote, false) |> NoteMsg)

            let state =
                { state with
                    CurrentPage =
                        NotePage
                            {
                                IdForJump = idForJump
                                Content = InProgress
                            }
                }
            state, cmd

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
                    NotePage
                        {
                            IdForJump = None
                            Content = InProgress
                        }
            }
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

    | NoteMsg msg ->
        match state.CurrentPage with
        | NotePage notePageState ->
            let state', cmd =
                NotePage.update msg notePageState
            let state =
                { state with
                    CurrentPage = NotePage state'
                }
            state, cmd |> Cmd.map NoteMsg
        | x -> failwithf "expected NotePage, but %A" x

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
                        NotePage
                            {
                                IdForJump = None
                                Content = Resolved (Error errMsg)
                            }
                }
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
            | NotePage noteState ->
                match noteState.Content with
                | Resolved (Ok x) ->
                    match x.Mode with
                    | NotePage.EditMode x ->
                        match x.SetFullNoteResult with
                        | NotePage.NotSaved -> false
                        | NotePage.Saved -> true
                        | NotePage.SavingInProgress -> false
                        | NotePage.SavingError(_) -> true
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
                    | NotePage state ->
                        match state.Content with
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
                | NotePage noteState ->
                    NotePage.view noteState (NoteMsg >> dispatch)
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
