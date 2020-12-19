namespace Shared

open System

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type FullNote =
    { Path:string; Note:MindNotes.Api.Note; Html:string }

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
        notesFilterByPattern : FilterPattern -> Async<Result<list<string * MindNotes.Api.Note>,string>>
        getNote : string -> Async<Result<FullNote, string>>
        setNote : FullNote -> Async<Result<FullNote, string>>
        newNote : unit -> Async<Result<FullNote, string>>
    }