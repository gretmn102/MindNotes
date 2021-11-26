module SearchPage
open Fulma.Extensions
open Elmish

open Shared
open Utils

type SearchState =
    {
        SearchResult: Result<FullNote list,string> Deferred
        SearchPattern: SearchPattern
        InputTagsState: InputTags.State
    }
    static member Init tags =
        {
            SearchResult = HasNotStartedYet
            SearchPattern = SearchPattern.Empty
            InputTagsState = InputTags.init tags
        }

let init tags =
    let state = SearchState.Init tags
    state

type SearchMsg =
    | SetSearchPattern of string
    | Search
    | SearchResult of Result<FullNote list,string>
    | InputSearchMsg of InputTags.Msg

let search (searchState: SearchState) =
    let filterPattern =
        {
            SearchPattern = searchState.SearchPattern
            Tags = searchState.InputTagsState.InputTagsState.Tags
        }
    let cmd =
        Cmd.OfAsync.perform todosApi.notesFilterByPattern
            filterPattern
            SearchResult

    let searchState =
        { searchState with
            SearchResult = InProgress
        }

    searchState, cmd

let update (state: SearchState) (msg: SearchMsg) =
    match msg with
    | InputSearchMsg msg ->
        let state', cmd =
            InputTags.update
                todosApi.getSuggestions
                msg
                state.InputTagsState

        let state =
            { state with
                InputTagsState = state'
            }

        state, Cmd.map InputSearchMsg cmd
    | SetSearchPattern value ->
        let state =
            { state with
                SearchPattern =
                    { state.SearchPattern with
                        Pattern = value
                    }
            }

        state, Cmd.none
    | Search ->
        search state
    | SearchResult searchResult ->
        let state =
            { state with
                SearchResult = Resolved searchResult }

        state, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Feliz
open Feliz.Router

let searchBox (searchState: SearchState) (dispatch: SearchMsg -> unit) =
    Box.box' [ ] [
        InputTags.view searchState.InputTagsState (InputSearchMsg >> dispatch)

        Field.div [ Field.HasAddons ] [
            let disabled =
                System.String.IsNullOrWhiteSpace searchState.SearchPattern.Pattern
                && List.isEmpty searchState.InputTagsState.InputTagsState.Tags

            Control.p [ Control.IsExpanded ] [
                Input.text [
                    Input.Value searchState.SearchPattern.Pattern
                    Input.Placeholder "Find"
                    Input.OnChange (fun x ->
                        SetSearchPattern x.Value
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