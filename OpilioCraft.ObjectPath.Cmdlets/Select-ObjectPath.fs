namespace OpilioCraft.ObjectPath.Cmdlets

open System
open System.Management.Automation
open OpilioCraft.ObjectPath

type private ObjectPathRuntime =
    | NotInitialized
    | Loaded of IRuntime * ObjectPath

[<Cmdlet(VerbsCommon.Select, "ObjectPath")>]
[<OutputType(typeof<obj>)>]
type public SelectObjectPathCommand () =
    inherit PSCmdlet ()

    let mutable objectPath = NotInitialized

    [<Parameter(Position=0, Mandatory=true, ValueFromPipeline=true)>]
    member val InputObject : obj = String.Empty with get,set

    [<Parameter(Position=1, Mandatory=true)>]
    [<ValidateNotNullOrEmpty>]
    member val ObjectPath : string = String.Empty with get,set

    [<Parameter>]
    member val UnwrapPSObject = SwitchParameter(false) with get,set

    // cmdlet behaviour
    override x.BeginProcessing () =
        let runtime = new DefaultRuntime () :> OpilioCraft.ObjectPath.IRuntime

        match runtime.TryParse x.ObjectPath with
        | Some expr -> objectPath <- Loaded (runtime, expr)
        | _ -> failwith "invalid ObjectPath expression"

    override x.ProcessRecord () =
        try
            match objectPath with
            | Loaded (runtime, expr) ->
                match x.InputObject with
                | :? PSObject as psObj when x.UnwrapPSObject.IsPresent -> psObj.BaseObject
                | anObj -> anObj
                |> fun inputObj -> runtime.ObjectData <- inputObj

                expr |> runtime.Eval |> x.WriteObject

            | NotInitialized -> failwith "[FATAL] uninitizialized ObjectPath runtime"
        with
            | exn ->
                ErrorRecord(exn, null, ErrorCategory.NotSpecified, x)
                |> x.WriteError
