module Note

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

// TODO: и вот сюда надо как-то вклинить клиент-программу
let getNote i (next:Giraffe.Core.HttpFunc) (ctx:Microsoft.AspNetCore.Http.HttpContext) =
    let html =
        [
            "<!doctype html>"
            "<html>"
            "<head>"
            sprintf "    <title>%i</title>" i
            "    <meta charset=\"utf-8\">"
            "    <link rel=\"shortcut icon\" type=\"image/png\" href=\"/favicon.png\"/>"
            "    <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bulma@0.9.0/css/bulma.min.css\">"
            "    <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.14.0/css/all.min.css\">"
            "</head>"
            "<body>"
            "    <div id=\"elmish-app\"></div>"
            "</body>"
            "</html>"
            ""
        ] |> String.concat "\n"

    Giraffe.ResponseWriters.htmlString html next ctx

// http://localhost:8085/note/1

// https://safe-stack.github.io/docs/recipes/client-server/messaging/#im-using-the-minimal-template-raw-http
let myApis = router {
    getf "/note/%i" getNote
}
