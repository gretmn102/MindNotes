module InputTags
open Elmish
open Feliz
open Browser

open Zanaptak.TypedCssClasses
open Fable.FontAwesome

type Bulma = CssClasses<"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.1/css/bulma.min.css", Naming.PascalCase>

type State =
    {
        CurrentTag: string
        IsActive: bool
        Suggestions: string list
    }
    static member Empty =
        {
            CurrentTag = ""
            IsActive = false
            Suggestions = []
        }

let dropdown state addTag items =
    Html.div [
        prop.className [
            Bulma.DropdownMenu
        ]
        if state.IsActive then
            // Bulma.IsActive
            prop.style [
                style.display.block
            ]
        prop.custom("id", Bulma.DropdownMenu)
        prop.custom("role", "menu")
        prop.children [
            Html.div [
                prop.className [
                    Bulma.DropdownContent
                ]
                items
                |> List.map (fun (tag:string) ->
                    Html.a [
                        prop.tabIndex -1 // necessarily prop
                        prop.className [
                            Bulma.DropdownItem
                        ]
                        prop.children [
                            Html.text tag
                        ]
                        prop.onClick (fun _ ->
                            let newState =
                                { state with
                                    // IsActive = false
                                    CurrentTag = ""
                                }
                            (newState, tag)
                            |> addTag
                        )
                    ]
                )
                |> prop.children
            ]
        ]
    ]
let inputTags (inputId:string) (state:State) removeTag changeState addTag suggestions tags =
    let createTag (name:string) =
        Html.div [
            prop.className [
                Bulma.Control
            ]
            prop.style [
                style.marginBottom (length.em 0.1)
                style.marginTop (length.em 0.1)
            ]
            prop.custom ("data-tag", name)
            prop.children [
                Html.div [
                    prop.className [
                        Bulma.Tags
                        Bulma.HasAddons
                    ]
                    prop.children [
                        Html.span [
                            prop.className [
                                Bulma.Tag
                                Bulma.IsActive
                            ]
                            prop.text name
                        ]
                        Html.a [
                            prop.className [
                                Bulma.Tag
                                Bulma.IsDelete
                            ]
                            prop.onClick (fun _ ->
                                removeTag name
                            )
                        ]
                    ]
                ]
            ]
        ]
    let inputFieldId = "inputFieldId"
    Html.div [
        prop.id inputFieldId
        prop.className [
            Bulma.Field
            Bulma.IsGrouped
            Bulma.IsGroupedMultiline
            Bulma.Input
            if state.IsActive then
                Bulma.IsActive
        ]
        prop.style [
            style.height length.auto
        ]

        if not state.IsActive then
            prop.onClick (fun e ->
                if e.currentTarget = e.target then
                    // let x = document.getElementById inputId
                    // x.focus()
                    { state with
                        IsActive = true
                    }
                    |> changeState
            )

        prop.children [
            yield! Seq.map createTag tags

            Html.div [
                prop.className [
                    Bulma.Field
                    Bulma.HasAddons
                ]
                prop.children [
                    Html.div [
                        prop.className [
                            Bulma.Control
                        ]
                        prop.children [
                            Html.input [
                                prop.id inputId
                                prop.type' "text"
                                prop.placeholder "Add Tag"
                                prop.classes [
                                    Bulma.Input
                                    if state.IsActive then
                                        Bulma.IsActive
                                ]
                                prop.style [
                                    style.width 172
                                    style.marginBottom (length.em 0.1)
                                    style.marginTop (length.em 0.1)
                                ]
                                prop.ref (fun x ->
                                    if not <| isNull x then
                                        if state.IsActive then
                                            let x = x :?> Types.HTMLElement
                                            x.focus ()
                                )
                                if state.IsActive then
                                    prop.onBlur (fun e ->
                                        match e.relatedTarget :?> Types.HTMLElement with
                                        | null ->
                                            { state with
                                                IsActive = false
                                            }
                                            |> changeState
                                        | related ->
                                            if related.classList.contains Bulma.DropdownItem then
                                                ()
                                            else
                                                { state with
                                                    IsActive = false
                                                }
                                                |> changeState
                                    )
                                else
                                    prop.onFocus (fun _ ->
                                        { state with
                                            IsActive = true
                                        }
                                        |> changeState
                                    )

                                // prop.spellcheck true // for some reason it doesn't work
                                prop.custom ("spellCheck", true)
                                prop.onTextChange (fun (tagName:string) ->
                                    { state with
                                        CurrentTag = tagName
                                    }
                                    |> changeState
                                )
                                prop.onKeyDown (fun e ->
                                    if e.key = "Enter" then
                                        let e = e.target :?> Types.HTMLInputElement
                                        let x = e.parentNode :?> Types.HTMLDivElement
                                        x.classList.remove Bulma.IsActive

                                        let newState =
                                            { state with
                                                CurrentTag = ""
                                            }
                                        (newState, state.CurrentTag)
                                        |> addTag
                                )
                            ]
                            match suggestions with
                            | [] -> ()
                            | suggestions ->
                                dropdown state addTag suggestions
                        ]
                    ]
                    Html.div [
                        prop.className [
                            Bulma.Control
                        ]
                        prop.children [
                            Html.a [
                                prop.className [
                                    Bulma.Button
                                ]
                                prop.style [
                                    style.marginBottom (length.em 0.1)
                                    style.marginTop (length.em 0.1)
                                ]
                                prop.onClick (fun _ ->
                                    let newState =
                                        { state with
                                            IsActive = true
                                            CurrentTag = ""
                                        }
                                    (newState, state.CurrentTag)
                                    |> addTag
                                )
                                prop.children [
                                    Fa.i [ Fa.Solid.Check ] []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]