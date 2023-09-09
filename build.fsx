#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators

Target.initEnvironment ()

let clientPath = Path.getFullName "./src/Client"
let clientFableOutput = clientPath </> "bin" </> "fable"
let sharedPath = Path.getFullName "./src/Shared"
let serverPath = Path.getFullName "./src/Server"
let deployDir = Path.getFullName "./deploy"
let clientTestsPath = Path.getFullName "./tests/Client"
let clientTestsFableOutput = clientTestsPath </> "bin" </> "fable"
let sharedTestsPath = Path.getFullName "./tests/Shared"
let serverTestsPath = Path.getFullName "./tests/Server"

let npm args workingDir =
    let npmPath =
        match ProcessUtils.tryFindFileOnPath "npm" with
        | Some path -> path
        | None ->
            "npm was not found in path. Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
            |> failwith

    let arguments = args |> String.split ' ' |> Arguments.OfArgs

    Command.RawCommand (npmPath, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

module Fable =
    type BuildOption =
        {
            Output: string
            IsWatch: bool
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module BuildOption =
        let empty: BuildOption =
            {
                Output = ""
                IsWatch = false
            }

    let run fableArgs =
        dotnet (sprintf "fable %s" (String.concat " " fableArgs)) "."

    let build (fableOption: BuildOption) projectPath =
        [
            yield projectPath

            yield
                match fableOption.Output with
                | null | "" -> ""
                | output -> sprintf "-o %s" output

            yield
                if fableOption.IsWatch then
                    "--watch"
                else
                    ""
        ]
        |> run

    let clean outputPath =
        if Shell.testDir outputPath then
            [
                "clean"
                sprintf "-o %s" outputPath
                "--yes"
            ]
            |> run

let removeBinAndObj path =
    Shell.cleanDirs [
        path </> "bin"
        path </> "obj"
    ]

Target.create "SharedClean" (fun _ ->
    removeBinAndObj sharedPath
)

Target.create "ServerClean" (fun _ ->
    removeBinAndObj serverPath
)

Target.create "ClientClean" (fun _ ->
    removeBinAndObj clientPath
)

Target.create "DeployClean" (fun _ ->
    Shell.cleanDir deployDir
)

Target.create "SharedTestsClean" (fun _ ->
    removeBinAndObj sharedTestsPath
)

Target.create "ServerTestsClean" (fun _ ->
    removeBinAndObj serverTestsPath
)

Target.create "ClientTestsClean" (fun _ ->
    removeBinAndObj clientTestsPath
)

Target.create "DotnetClean" (fun _ -> ())

Target.create "ClientInstall" (fun _ -> npm "install" ".")

// the test project includes the main project,
// so the dependencies of the main project will be included
Target.create "Femto" (fun _ ->
    dotnet (sprintf "femto %s --resolve" clientTestsPath) "."
)

Target.create "ClientFableBuild" (fun _ ->
    clientPath
    |> Fable.build
        { Fable.BuildOption.empty with
            Output = clientFableOutput
        }
)

Target.create "ClientFableWatch" (fun _ ->
     clientPath
    |> Fable.build
        { Fable.BuildOption.empty with
            Output = clientFableOutput
            IsWatch = true
        }
)

Target.create "ClientFableClean" (fun _ ->
    Fable.clean clientFableOutput
)

Target.create "ClientViteWatch" (fun _ ->
    npm "run vite:watch" "."
)

Target.create "ClientViteBuild" (fun _ ->
    npm "run vite:build" "."
)

Target.create "ClientDeploy" (fun _ -> ())

Target.create "ClientTestsFableBuild" (fun _ ->
    clientTestsPath
    |> Fable.build
        { Fable.BuildOption.empty with
            Output = clientTestsFableOutput
        }
)

Target.create "ClientTestsFableWatch" (fun _ ->
    clientTestsPath
    |> Fable.build
        { Fable.BuildOption.empty with
            Output = clientTestsFableOutput
            IsWatch = true
        }
)

Target.create "ClientTestsFableClean" (fun _ ->
    Fable.clean clientTestsFableOutput
)

Target.create "ClientTestsViteWatch" (fun _ ->
    npm "run test:vite:watch" "."
)

Target.create "ClientTestsWatchInternal" (fun _ -> ())

Target.create "ClientTestsWatch" (fun _ ->
    Target.run 2 "ClientTestsWatchInternal" []
)

Target.create "ServerBuild" (fun _ ->
    dotnet "build" serverPath
)

Target.create "ServerDeploy" (fun _ ->
    dotnet (sprintf "publish -c Release -o \"%s\"" deployDir) serverPath
)

Target.create "ServerWatch" (fun _ ->
    dotnet "watch run" serverPath
)

Target.create "ServerTestsRun" (fun _ ->
    dotnet "run" serverTestsPath
)

Target.create "ClientDotnetBuild" (fun _ ->
    dotnet "build" clientPath
)

Target.create "ServerTestsBuild" (fun _ ->
    dotnet "build" serverTestsPath
)

Target.create "ClientTestsDotnetBuild" (fun _ ->
    dotnet "build" clientTestsPath
)

Target.create "DotnetBuild" (fun _ -> ())

Target.create "DotnetCleanBuild" (fun _ -> ())

Target.create "Deploy" (fun _ -> ())

open Fake.Core.TargetOperators


"SharedClean" ==> "DotnetClean"
"SharedTestsClean" ==> "DotnetClean"
"SharedClean" ?=> "SharedTestsClean"
"ClientClean" ==> "DotnetClean"
"ClientTestsClean" ==> "DotnetClean"
"ClientClean" ?=> "ClientTestsClean"
"ServerClean" ==> "DotnetClean"
"ServerTestsClean" ==> "DotnetClean"
"ServerClean" ?=> "ServerTestsClean"
"SharedClean" ?=> "ClientClean"

"ServerTestsClean" ?=> "ServerTestsBuild"
"ClientTestsClean" ?=> "ClientTestsDotnetBuild"

"ServerTestsBuild" ==> "DotnetBuild"
"ClientTestsDotnetBuild" ==> "DotnetBuild"

"DeployClean" ==> "Deploy"
"ClientInstall" ==> "ClientFableBuild"
"ClientFableClean" ==> "ClientFableBuild"
"ClientInstall" ?=> "ClientFableClean"
"ClientFableBuild"
    ==> "ClientViteBuild"
    ==> "ClientDeploy"
    ==> "Deploy"
"SharedClean" ==> "ServerDeploy"
"ServerClean" ==> "ServerDeploy"
"SharedClean" ?=> "ServerClean"
"ServerDeploy" ==> "Deploy"

"ClientTestsFableWatch" ==> "ClientTestsWatchInternal"
"ClientTestsViteWatch" ==> "ClientTestsWatchInternal"

"DotnetClean" ==> "DotnetCleanBuild"
"DotnetBuild" ==> "DotnetCleanBuild"
"DotnetClean" ?=> "DotnetBuild"

Target.runOrDefaultWithArguments "Deploy"
