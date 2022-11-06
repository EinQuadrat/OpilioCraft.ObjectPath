namespace OpilioCraft.ObjectPath

// exceptions
exception InvalidObjectPathException of Path:string
    with override x.ToString () = $"invalid object path: {x.Path}"

exception MissingObjectDataException
    with override x.ToString () = "missing object data"

// AST nodes
[<RequireQualifiedAccess>]
type ObjectPathElement =
    | Property of string
    | DictionaryKey of string

type ObjectPath = ObjectPathElement list

// Runtime
type IRuntime =
    abstract member ObjectData : obj with set

    abstract member Parse : string -> ObjectPath
    abstract member TryParse : string -> ObjectPath option

    abstract member Eval : ObjectPath -> obj
    abstract member TryEval : ObjectPath -> obj option
    abstract member EvalWithDefault : ObjectPath -> obj -> obj

    abstract member Run : string -> obj
    abstract member TryRun : string -> obj option
    abstract member RunWithDefault : string -> obj -> obj
