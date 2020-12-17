module Shared.MindNotes.Api

type Note = { DateTime:System.DateTime option; Tags:string list; Text:string }

let notePrint (x:Note) =
    let datetime =
        x.DateTime
        |> Option.map (fun x -> x.ToString() |> sprintf "%s\n")
        |> Option.defaultValue ""
    let tags =
        match x.Tags with
        | [] -> ""
        | xs ->
            xs |> List.map (sprintf "#%s")
            |> String.concat " " |> sprintf "%s\n"
    sprintf "%s%s%s" datetime tags (x.Text.TrimEnd())
let notesPrint =
    List.map notePrint >> String.concat "\n***\n"

let allTags =
    List.collect (fun x -> x.Tags)
    >> Set.ofList
    >> String.concat "\n"

let tagEscape (str:string) =
    str
    |> fun x -> x.Trim()
    |> fun x -> x.Replace(" ", "_")

let getShortDscr (note:Note) =
    let dscr =
        note.Text.Replace("\n", "\\n")
                 .Replace("[","\\[")
                 .Replace("]","\\]")
    let length = 100
    if dscr.Length < length then
        dscr
    else
        sprintf "%s..." dscr.[..length - 1]