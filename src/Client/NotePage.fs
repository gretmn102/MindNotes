module NotePage
open Elmish
open Fulma.Extensions

open Shared
open Utils

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

type State =
    {
        IdForJump: IdForJump option
        Content: Result<NotePageState, string> Deferred
    }

type NoteId = string
type NoteMsg =
    /// А сам `GetNote` воплощается через Router
    | GetNoteResult of Result<FullNote, string> * editMode:bool
    | SetNote
    | RemoveNote
    | RemoveNoteResult of Result<unit, string>
    | SetNoteResult of Result<FullNote, string>

    | ChangeNotePageMode of NotePageMode
    | ChangeNotePageTags of InputTags.Msg


let update (x: NoteMsg) (state: State) =
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

        let state =
            { state with
                Content = Resolved x
            }

        state, Cmd.none

    | SetNote ->
        match state.Content with
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

                        let cmd = Cmd.OfAsync.perform todosApi.setNote fullNoteTemp SetNoteResult
                        Resolved(Ok st), cmd
                    | ViewMode -> failwith "Not Implemented"
                | Error(errorValue) -> failwith "Not Implemented"
            let state =
                { state with
                    Content = st
                }
            state, cmd
        | x -> state, Cmd.none

    | SetNoteResult res ->
        match state.Content with
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
            let state =
                { state with
                    Content = st
                }
            state, cmd
        | x -> state, Cmd.none

    | ChangeNotePageMode mode ->
        match state.Content with
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
            let state =
                { state with
                    IdForJump = None
                    Content = st
                }
            state, cmd
        | x ->
            state, Cmd.none

    | RemoveNote ->
        match state.Content with
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

                       let cmd = Cmd.OfAsync.perform todosApi.removeNote notePageState.FullNote.Id RemoveNoteResult
                       Resolved(Ok st), cmd
                   | ViewMode -> Resolved(Ok notePageState), Cmd.none
                | x ->
                    Resolved x, Cmd.none
            let state =
                { state with
                    Content = st
                }
            state, cmd
        | x -> state, Cmd.none

    | RemoveNoteResult res ->
        match state.Content with
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
            let state =
                { state with
                    Content = st
                }
            state, cmd
        | x -> state, Cmd.none

    | ChangeNotePageTags msg ->
        match state.Content with
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
                            Cmd.map ChangeNotePageTags cmd

                        Resolved(Ok state), cmd
                    | ViewMode ->
                        Resolved(Ok notePageState), Cmd.none
                | Error _ as x ->
                    Resolved x, Cmd.none
            let state =
                { state with
                    Content = st
                }
            state, cmd
        | x -> state, Cmd.none


open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let view (state: State) (dispatch: NoteMsg -> unit) =
    match state.Content with
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
                            match state.IdForJump with
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
                            Browser.Dom.window.confirm () |> ignore
                        )
                    | _ ->
                        Browser.Dom.window.onbeforeunload <- ignore

                    InputTags.view
                        editModeState.InputTagsState
                        (ChangeNotePageTags >> dispatch)

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
                                    |> dispatch
                                )
                            ]
                            [ str errMsg ]
            ]
        | Error errMsg ->
            p [] [str errMsg]
    | InProgress -> spinner
    | HasNotStartedYet -> p [] [str "HasNotStartedYet"]
