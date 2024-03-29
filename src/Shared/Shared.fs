namespace Shared

open System

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
type NoteId = string
type FullNote =
    {
        Id: string
        Path:string
        Note:MindNotes.Api.Note
        Html:string
        Title:string option
        LastWriteTime:System.DateTime
    }

type SearchPattern =
    {
        Pattern:string
        IsRegex:bool
        MatchCase:bool
    }
    static member Empty =
        { Pattern = ""; IsRegex = false; MatchCase = false }
type FilterPattern =
    {
        SearchPattern: SearchPattern
        Tags: string list
    }
    static member Empty =
        {
            SearchPattern = SearchPattern.Empty
            Tags = []
        }
type ITodosApi =
    {
        notesFilterByPattern : FilterPattern -> Async<Result<list<FullNote>,string>>
        getNote : NoteId -> Async<Result<FullNote, string>>
        setNote : FullNote -> Async<Result<FullNote, string>>
        removeNote : NoteId -> Async<Result<unit, string>>
        newNote : unit -> Async<Result<FullNote, string>>
        getTags : unit -> Async<MindNotes.Api.Tag []>
        renameTag : (string * string) -> Async<list<Result<FullNote,string>>>
        getSuggestions : string -> Async<MindNotes.Api.Tag []>
    }