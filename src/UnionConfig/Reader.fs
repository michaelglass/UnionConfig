/// Reading config vars from environment variables.
module UnionConfig.Reader

open System
open UnionConfig.Types

/// Read a config var from environment, parse to typed value.
/// Returns Result for composable error handling:
/// - Required missing/empty → Error with message
/// - Required invalid → Error with parse message
/// - Optional missing/empty → Ok None
/// - Optional invalid → Error with parse message
/// - Valid → Ok (Some value)
let read (def: ConfigVarDef) : Result<ConfigValue option, string> =
    // fsharplint:disable-next-line Hints
    let rawValue = Environment.GetEnvironmentVariable(def.Name) |> Option.ofObj

    match def.Requirement, rawValue with
    | Required, None -> Error $"%s{def.Name} is required but not set"
    | Required, Some value when String.IsNullOrWhiteSpace(value) -> Error $"%s{def.Name} is required but empty"
    | Required, Some value ->
        match parseValue def.ValueType value with
        | Ok cv -> Ok(Some cv)
        | Error msg -> Error $"%s{def.Name}: %s{msg}"
    | Optional, None -> Ok None
    | Optional, Some value when String.IsNullOrWhiteSpace(value) -> Ok None
    | Optional, Some value ->
        match parseValue def.ValueType value with
        | Ok cv -> Ok(Some cv)
        | Error msg -> Error $"%s{def.Name}: %s{msg}"

/// Unwrap a Result from read, throwing on Error for convenience functions.
let private unwrap (result: Result<ConfigValue option, string>) : ConfigValue option =
    match result with
    | Ok v -> v
    | Error msg -> failwithf "Configuration error: %s" msg

/// Read string config var - returns empty string if not set
let readString (def: ConfigVarDef) =
    read def |> unwrap |> ConfigValue.stringOption |> Option.defaultValue ""

/// Read int config var - fails if not set or invalid
let readInt (def: ConfigVarDef) = read def |> unwrap |> ConfigValue.int

/// Read bool config var - fails if not set or invalid
let readBool (def: ConfigVarDef) = read def |> unwrap |> ConfigValue.bool

/// Read int with default (for optional vars or missing values)
let readIntOrDefault (def: ConfigVarDef) (defaultValue: int) =
    read def |> unwrap |> ConfigValue.intOption |> Option.defaultValue defaultValue

/// Read bool with default (for optional vars or missing values)
let readBoolOrDefault (def: ConfigVarDef) (defaultValue: bool) =
    read def |> unwrap |> ConfigValue.boolOption |> Option.defaultValue defaultValue

/// Validate all required vars are set and have valid values.
/// Returns a list of error messages for any missing or invalid vars.
let validateRequired (defs: ConfigVarDef seq) : string list =
    defs
    |> Seq.filter (fun d -> d.Requirement = Required)
    |> Seq.choose (fun def ->
        match read def with
        | Ok _ -> None
        | Error msg -> Some msg)
    |> Seq.toList
