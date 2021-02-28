module Shared.MindNotes.Api
type Tag = string
type Note =
    {
        DateTime:System.DateTime option
        Tags:Tag list
        Text:string
        Views:int
    }

let notePrint (note:Note) =
    let datetime =
        note.DateTime
        |> Option.map (fun x -> x.ToString("dd.MM.yyyy HH:mm:ss"))
        |> Option.defaultValue ""

    let tags =
        match note.Tags with
        | [] -> ""
        | xs ->
            xs |> List.map (sprintf "#%s")
            |> String.concat " " |> sprintf "%s\n"
    sprintf "%s|%d\n%s%s" datetime note.Views tags (note.Text.TrimEnd())
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

let datetimeFileFormat (d:System.DateTime) =
    d.ToString("yyyy-MM-dd_HH-mm-ss")
