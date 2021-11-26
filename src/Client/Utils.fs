module Utils

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

[<Literal>]
let TagRoute = "tag"

open Fable.Remoting.Client

open Shared

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

open Fable.React
open Fable.React.Props
open Fable.FontAwesome

let spinner =
    div [ Class ("block " + Fa.Classes.Size.Fa3x) ] [
        Fa.i [ Fa.Solid.Spinner; Fa.Spin ] []
    ]

