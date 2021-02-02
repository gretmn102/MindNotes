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
        newNote : unit -> Async<Result<FullNote, string>>
        getTags : unit -> Async<MindNotes.Api.Tag list>
        getSuggestions : string -> Async<MindNotes.Api.Tag []>
    }