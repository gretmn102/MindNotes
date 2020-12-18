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
    }
    static member Empty =
        {
            CurrentTag = ""
        }

let inputTags (inputId:string) removeTag changeState addTag tags =
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
        ]
        prop.style [
            style.height length.auto
        ]
        prop.onClick (fun e ->
            let x = e.currentTarget :?> Types.HTMLDivElement
            x.classList.add Bulma.IsActive

            let x = document.getElementById inputId
            x.focus()
            ()
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
                                ]
                                prop.style [
                                    style.width 172
                                    style.marginBottom (length.em 0.1)
                                    style.marginTop (length.em 0.1)
                                ]

                                prop.onClick (fun _ ->
                                    let x = document.getElementById inputFieldId
                                    x.classList.add Bulma.IsActive
                                )
                                prop.onBlur (fun _ ->
                                    let x = document.getElementById inputFieldId
                                    x.classList.remove Bulma.IsActive
                                )

                                // prop.spellcheck true // for some reason it doesn't work
                                prop.custom ("spellCheck", true)
                                prop.onTextChange (fun (tagName:string) ->
                                    {
                                        CurrentTag = tagName
                                    }
                                    |> changeState
                                )
                                prop.onKeyDown (fun e ->
                                    if e.key = "Enter" then
                                        let e = e.target :?> Types.HTMLInputElement
                                        let x = e.parentNode :?> Types.HTMLDivElement
                                        x.classList.remove Bulma.IsActive

                                        fun state ->
                                            let newState =
                                                {
                                                    CurrentTag = ""
                                                }
                                            newState, state.CurrentTag
                                        |> addTag
                                )
                            ]
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
                                    fun state ->
                                        let newState =
                                            {
                                                CurrentTag = ""
                                            }
                                        newState, state.CurrentTag
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