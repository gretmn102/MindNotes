module MarkdownConverter
open Markdig
// Наверное, в будущем лучше перенести его на клиентскую часть

let toMarkdown =
    let pipe = MarkdownPipelineBuilder()

    // Самое важное (в порядке возрастания):
    pipe.UseSoftlineBreakAsHardlineBreak() |> ignore

    let opt = Extensions.AutoLinks.AutoLinkOptions()
    opt.OpenInNewWindow <- true
    opt.UseHttpsForWWWLinks <- true
    pipe.UseAutoLinks(opt) |> ignore

    pipe.UseCitations() |> ignore

    pipe.UseAutoIdentifiers(Extensions.AutoIdentifiers.AutoIdentifierOptions.AutoLink) |> ignore

    pipe.UseFigures() |> ignore

    pipe.UseMediaLinks() |> ignore

    pipe.UseFooters() |> ignore
    pipe.UseFootnotes() |> ignore

    // let opt = Extensions.Tables.PipeTableOptions()
    pipe.UsePipeTables() |> ignore

    pipe.UseGenericAttributes() |> ignore
    let pipe = pipe.Build()
    fun reader -> Markdig.Markdown.ToHtml(reader, pipe)
