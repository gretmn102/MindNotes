module MarkdownConverter
open Markdig
open FsharpMyExtension
open FsharpMyExtension.Either

type NoteContentLink =
    {
        NoteId: Shared.MindNotes.Api.NoteDateTime option
        Fragment: string option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module NoteContentLink =
    module Parser =
        open FParsec
        open Shared.MindNotes.Api

        let parser : Parser<_, unit> =
            let pfragment =
                skipChar '#' >>. manySatisfy (fun _ -> true)

            let pnoteIdAndFragment =
                pipe2
                    (NoteDateTime.Parser.pdateTimeFileFormat .>> skipString ".md" |>> Some)
                    (optional (skipChar '/') >>. opt pfragment)
                    (fun noteId fragment ->
                        {
                            NoteId = noteId
                            Fragment = fragment
                        }
                    )

            let pfragmentOnly =
                (skipChar '/' >>. pfragment) <|> pfragment
                |>> fun fragment ->
                    {
                        NoteId = None
                        Fragment = Some fragment
                    }

            pnoteIdAndFragment <|> pfragmentOnly

    let toUrl currentNoteId rawUrl =
        FParsecExt.runEither Parser.parser rawUrl
        |> Either.map (fun internalLink ->
            let noteId =
                match internalLink.NoteId with
                | Some noteId ->
                    Shared.MindNotes.Api.NoteDateTime.serialize noteId
                | None ->
                    currentNoteId

            let fragment =
                match internalLink.Fragment with
                | Some fragment -> "/#" + fragment.Replace("%20", "-")
                | None -> ""

            // TODO: refact: use `note` route from client side
            sprintf "#/note/%s%s" noteId fragment
        )

type NoteMarkdownContent =
    {
        Title: string option
        Html: string
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module NoteMarkdownContent =
    let getTitle (markdownDocument: Syntax.MarkdownDocument) =
        let toString (containerInline: Syntax.Inlines.ContainerInline)=
            containerInline
            |> Seq.map (function
                | :? Syntax.Inlines.LinkInline as x ->
                    x
                    |> Seq.map (function
                        | :? Syntax.Inlines.EmphasisInline as x ->
                            x.FirstChild.ToString()
                        | x -> x.ToString()
                    )
                    |> String.concat ""
                | x -> x.ToString()
            )
            |> String.concat ""

        let rec getTitle (blocks: Syntax.Block list) =
            match blocks with
            | (:? Syntax.ParagraphBlock as x)::_ ->
                x.Inline
                |> toString
                |> Some
            | (:? Syntax.HeadingBlock as x)::_ ->
                x.Inline
                |> toString
                |> Some
            | _::xs -> getTitle xs
            | [] -> None

        markdownDocument
        |> List.ofSeq
        |> getTitle

    let convertNoteContentLinkToUrls currentNoteId (markdownDocument: Syntax.MarkdownDocument) =
        let rec mapInlines (inlines: Syntax.Inlines.Inline seq) =
            inlines
            |> Seq.iter (
                function
                | :? Syntax.Inlines.LinkInline as linkInline ->
                    NoteContentLink.toUrl currentNoteId linkInline.Url
                    |> Either.iter (fun resultUrl ->
                        linkInline.Url <- resultUrl
                    )
                | :? Syntax.Inlines.ContainerInline as containerInline ->
                    mapInlines containerInline
                | _ -> ()
            )

        let mapLeafBlock (leafBlock : Syntax.LeafBlock) =
            match leafBlock.Inline with
            | null -> ()
            | x -> mapInlines x

        let rec mapBlocks (blocks: Syntax.Block seq) =
            blocks
            |> Seq.iter (function
                | :? Syntax.LeafBlock as leafBlock ->
                    mapLeafBlock leafBlock
                | :? Syntax.ContainerBlock as x ->
                    mapBlocks x
                | _ -> ()
            )

        mapBlocks markdownDocument

    let private markdownPipeline =
        let pipe = MarkdownPipelineBuilder()

        pipe.UseSoftlineBreakAsHardlineBreak() |> ignore

        let opt = Extensions.AutoLinks.AutoLinkOptions()
        opt.OpenInNewWindow <- true
        opt.UseHttpsForWWWLinks <- true
        pipe.UseAutoLinks(opt) |> ignore

        pipe.UseCitations() |> ignore
        pipe.UseFigures() |> ignore
        pipe.UseFooters() |> ignore

        pipe.UseFootnotes() |> ignore

        pipe.UseAutoIdentifiers(Extensions.AutoIdentifiers.AutoIdentifierOptions.AutoLink) |> ignore

        pipe.UseMediaLinks() |> ignore

        pipe.UsePipeTables() |> ignore

        pipe.UseGenericAttributes() |> ignore

        pipe.UseEmphasisExtras() |> ignore // for ~~strike~~

        pipe.Build()

    let deserialize currentNoteId rawMarkdown : NoteMarkdownContent =
        let document = Markdig.Markdown.Parse(rawMarkdown, markdownPipeline)
        convertNoteContentLinkToUrls currentNoteId document

        {
            Title = getTitle document
            Html =
                use writer = new System.IO.StringWriter()
                let render = Renderers.HtmlRenderer(writer)
                render.Render(document).ToString()
        }
