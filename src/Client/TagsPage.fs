module TagsPage
open Elmish
open Feliz.Router
open Fulma.Extensions

open Shared
open Utils

type ErrorMsg = string
type EditState =
    | View
    | Edit of {| Input: InputWithSuggestions.State; Submit: Deferred<Result<unit, ErrorMsg list>> |}

type State =
    {
        InitTag: string
        EditState: EditState
    }
let init initTag =
    {
        InitTag = initTag
        EditState = View
    }
type Msg =
    | ChangeToEditMode
    | Cancel
    | Submit
    | SubmitResult of Result<FullNote,string> list
    | InputTagsMsg of InputWithSuggestions.Msg

let update (msg: Msg) (state: State) =
    match msg with
    | ChangeToEditMode ->
        let state =
            { state with
                EditState =
                    {|
                        Input = InputWithSuggestions.init state.InitTag
                        Submit = HasNotStartedYet |}
                    |> Edit
            }
        state, Cmd.none
    | Submit ->
        let emoji = state
        match emoji.EditState with
        | Edit edit ->
            let cmd =
                Cmd.OfAsync.perform
                    todosApi.renameTag
                    (state.InitTag, edit.Input.Input)
                    SubmitResult
            let state =
                { emoji with
                    EditState =
                        {| edit with
                            Submit = InProgress
                        |}
                        |> Edit
                }
            state, cmd
        | View ->
            state, Cmd.none
    | SubmitResult(res) ->
        let emoji = state
        match emoji.EditState with
        | Edit edit ->
            let errors =
                res
                |> List.choose (function
                    | Ok _ -> None
                    | Error x -> Some x)
            if List.isEmpty errors then
                let state =
                    {
                        InitTag = edit.Input.Input
                        EditState = View
                    }
                state, Cmd.none
            else
                let state =
                    { emoji with
                        EditState =
                            Edit {| edit with Submit = Resolved (Error errors) |}
                    }
                state, Cmd.none
        | View ->
            state, Cmd.none
    | InputTagsMsg msg ->
        let emoji = state
        match emoji.EditState with
        | Edit edit ->
            let inputTagsState, cmd =
                InputWithSuggestions.update todosApi.getSuggestions msg edit.Input
            let state =
                { emoji with
                    EditState =
                        Edit {| edit with Input = inputTagsState |}
                }
            let cmd = Cmd.map (fun msg -> InputTagsMsg msg) cmd
            state, cmd
        | View ->
            state, Cmd.none
    | Cancel ->
        let state =
            { state with
                EditState = View
            }
        state, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Feliz

let view (emoji:State) dispatch =
    match emoji.EditState with
    | View ->
        Tag.list [] [
            a [Href (Router.format [ TagRoute; emoji.InitTag ])] [ str emoji.InitTag ]

            Button.button [
                Button.OnClick (fun _ ->
                    dispatch ChangeToEditMode
                )
            ] [
                Fa.i [ Fa.Solid.Edit ] []
            ]
        ]

    | Edit edit ->
        match edit.Submit with
        | HasNotStartedYet | Resolved _ ->
            div [] [
                InputWithSuggestions.view "Tag" (fun _ -> dispatch Submit) edit.Input (fun msg ->
                    InputTagsMsg msg
                    |> dispatch)

                Button.button [
                    let isDisabled =
                        match edit.Submit with
                        | Resolved(Error x) -> true
                        | _ -> false
                    if isDisabled then
                        Button.Color Color.IsDanger
                    Button.Disabled isDisabled
                    Button.OnClick (fun _ ->
                        if not isDisabled then
                            dispatch Submit
                    )
                ] [
                    match edit.Submit with
                    | Resolved(Error errorMsg) ->
                        str (sprintf "%A" errorMsg)
                    | _ ->
                        Fa.i [ Fa.Solid.Check ] []
                ]

                Button.button [
                    Button.OnClick (fun _ ->
                        dispatch Cancel
                    )
                ] [
                    Fa.i [ Fa.Solid.Ban ] []
                ]
            ]
        | InProgress -> spinner
