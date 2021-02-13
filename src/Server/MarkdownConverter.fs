module MarkdownConverter
open Markdig
// Наверное, в будущем лучше перенести его на клиентскую часть

open FsharpMyExtension
open FsharpMyExtension.Either
module UrlParse =
    open FParsec
    open MindNotes.Api.Parser

    /// `System.DateTime * fragment:string option`
    let parseInternalUrl str =
        let sharp =
            skipChar '#' >>. manySatisfy (fun _ -> true)
        let p =
            pdateTimeFileFormat .>> skipString ".md" |>> Some
            .>>. (optional (skipChar '/')
                  >>. opt sharp)
            <|> ((skipChar '/' >>. sharp) <|> sharp |>> fun x -> None, Some x)

        match run p str with
        | Success(res, _, _) -> Right res
        | Failure(errMsg, _, _) -> Left errMsg

let toMarkdown =
    let pipe = MarkdownPipelineBuilder()

    // Самое важное (в порядке возрастания):
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

    let pipe = pipe.Build()
    fun currentNoteId markdown ->
        // let reader = "sdf*sdg* *[](https://www.youtube.com/watch?v=ss0-HGGh9Yo)*"
        use writer = new System.IO.StringWriter()
        let render = Renderers.HtmlRenderer(writer)
        pipe.Setup(render)

        let markdig = Markdig.Markdown.Parse(markdown, pipe)

        let rec f (markdig:Syntax.Block seq) =
            markdig
            |> Seq.iter (function
                | :? Syntax.LeafBlock as y ->
                    let rec f (xs:seq<Syntax.Inlines.Inline>) =
                        xs
                        |> Seq.iter (
                            function
                            | :? Syntax.Inlines.LinkInline as x ->
                                match UrlParse.parseInternalUrl x.Url with
                                | Left _ ->
                                    ()
                                | Right (res, chapter) ->
                                    let chapter =
                                        match chapter with
                                        | Some chapter -> "/#" + chapter.Replace("%20", "-")
                                        | None -> ""
                                    let res =
                                        match res with
                                        | Some res -> Shared.MindNotes.Api.datetimeFileFormat res
                                        | None -> currentNoteId
                                    x.Url <-
                                        sprintf "#/note/%s%s"
                                            res
                                            chapter
                            | :? Syntax.Inlines.ContainerInline as x ->
                                f x
                            | x -> ()
                        )
                    match y.Inline with
                    | null -> ()
                    | x -> f x
                | :? Syntax.ContainerBlock as x ->
                    f x
                | _ -> ()
            )
        f markdig

        render.Render(markdig) |> fun x -> x.ToString()
