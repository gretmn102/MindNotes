module Utils
open Shared

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

[<Literal>]
let TagRoute = "tag"
[<Literal>]
let NoteRoute = "note"

open Fable.Remoting.Client

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

open Fulma
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Feliz.Router

let spinner =
    div [ Class ("block " + Fa.Classes.Size.Fa3x) ] [
        Fa.i [ Fa.Solid.Spinner; Fa.Spin ] []
    ]

let activatedTagsRender tags =
    tags
    |> List.map (fun x ->
        Tag.tag [
        ] [
            a [Href (Router.format [TagRoute; x])] [str x]
        ])
    |> Tag.list []
