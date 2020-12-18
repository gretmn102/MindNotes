namespace Shared

open System

type Todo =
    { Id : Guid
      Description : string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type FullNote =
    { Path:string; Note:MindNotes.Api.Note; Html:string }

type ITodosApi =
    {
        notesFilterByPattern : string -> Async<Result<list<string * MindNotes.Api.Note>,string>>
        getNote : string -> Async<Result<FullNote, string>>
        setNote : FullNote -> Async<Result<FullNote, string>>
    }