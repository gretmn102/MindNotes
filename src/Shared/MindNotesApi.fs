module Shared.MindNotes.Api

type Tag = string
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Tag =
    let escape (tag: Tag) =
        tag
        |> fun x -> x.Trim()
        |> fun x -> x.Replace(" ", "_")

type NoteDateTime = System.DateTime
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module NoteDateTime =
    let serialize (dateTime: NoteDateTime) =
        dateTime.ToString("yyyy-MM-dd_HH-mm-ss")

type Note =
    {
        DateTime: NoteDateTime option
        Tags: Tag list
        Text: string
        Views: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Note =
    let serialize (note: Note) =
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

    let getShortInlineDescription (note:Note) =
        let dscr =
            note.Text.Replace("\n", "\\n")
                    .Replace("[","\\[")
                    .Replace("]","\\]")
        let length = 100
        if dscr.Length < length then
            dscr
        else
            sprintf "%s..." dscr.[..length - 1]

let notesPrint =
    List.map Note.serialize >> String.concat "\n***\n"

let allTags =
    List.collect (fun x -> x.Tags)
    >> Set.ofList
    >> String.concat "\n"
