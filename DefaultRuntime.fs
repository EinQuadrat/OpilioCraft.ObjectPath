﻿namespace OpilioCraft.ObjectPath

open FParsec

type DefaultRuntime () =
    let mutable objectPathCache : Map<string, ObjectPath> = Map.empty
    let mutable objectData : obj option = None

    let parseObjectPath opString : ObjectPath =
        FParsec.CharParsers.run Parser.pObjectPath opString
        |> function
            | Success(objectPath, _, _) -> objectPath
            | Failure(_, _, _) -> raise <| InvalidObjectPathException opString
        
    let getParsedObjectPath opString =
        objectPathCache.TryFind opString
        |> Option.defaultWith
            (fun _ ->
                let parsedObjectPath = parseObjectPath opString in // might throw InvalidObjectPathException
                objectPathCache <- objectPathCache |> Map.add opString parsedObjectPath
                parsedObjectPath
            )

    let getPropertyValue (name : string) (theObj : obj) =
        theObj.GetType().GetProperty(name)
        |> Option.ofObj
        |> Option.defaultWith (fun _ -> failwith $"{theObj.GetType().FullName} has no property \"{name}\"")

        |> fun pi ->
            try
                pi.GetValue(theObj)
            with
            | exn -> failwith $"cannot access value of property {name}: {exn.Message}"

    let rec evalObjectPath (objectPath : ObjectPath) (theObj : obj) : obj =
        match objectPath with
        | [] -> theObj

        | (ObjectPathElement.Property name) :: tail ->
            theObj
            |> getPropertyValue name
            |> (fun value -> if value.GetType().IsEnum then value.ToString() :> obj else value) // stringify enum types to facilitate wrapping
            |> evalObjectPath tail
            
        | (ObjectPathElement.DictionaryKey key) :: tail ->
            theObj
            |> getPropertyValue "Item"
            |> evalObjectPath tail
    
    // interface helpers
    member private x.AsIRuntime = x :> IRuntime
    member x.Eval = x.AsIRuntime.Eval
    member x.TryEval = x.AsIRuntime.TryEval
    member x.EvalWithDefault = x.AsIRuntime.EvalWithDefault
    member x.Run = x.AsIRuntime.Run
    member x.TryRun = x.AsIRuntime.TryRun
    member x.RunWithDefault = x.AsIRuntime.RunWithDefault

    // interface implementation
    interface IRuntime with
        member _.ObjectData
            with set value = objectData <- Some value

        member _.Parse opString =
            opString |> getParsedObjectPath // memoize on-the-fly

        member _.TryParse opString = // does not (!) memoize
            try
                opString |> parseObjectPath |> Some
            with
                | _ -> None

        member _.Eval objectPath =
            objectData
            |> Option.map (evalObjectPath objectPath)
            |> Option.defaultWith (fun _ -> raise MissingObjectDataException)

        member x.TryEval objectPath =
            try
                x.Eval objectPath |> Some
            with
                | _ -> None

        member x.EvalWithDefault objectPath defaultValue =
            objectPath |> x.TryEval |> Option.defaultValue defaultValue

        member x.Run opString =
            opString |> getParsedObjectPath |> x.Eval

        member x.TryRun opString =
            opString |> getParsedObjectPath |> x.TryEval
            // object path itself has to be syntactically correct

        member x.RunWithDefault opString defaultValue =
            opString |> x.TryRun |> Option.defaultValue defaultValue
            // object path has to be syntactically correct
