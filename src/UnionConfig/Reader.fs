/// Reading config vars from environment variables.
module UnionConfig.Reader

open System
open UnionConfig.Types

/// Read a config var from environment, parse to typed value.
/// - Required vars: Some value (fails if missing/invalid)
/// - Optional vars: Some value or None if missing
let read (def: ConfigVarDef) : ConfigValue option =
    // fsharplint:disable-next-line Hints
    let rawValue = Environment.GetEnvironmentVariable(def.Name) |> Option.ofObj

    match def.Requirement, rawValue with
    | Required, None -> failwithf "Configuration error: %s is required but not set" def.Name
    | Required, Some value when String.IsNullOrWhiteSpace(value) ->
        failwithf "Configuration error: %s is required but empty" def.Name
    | Required, Some value ->
        match parseValue def.ValueType value with
        | Ok cv -> Some cv
        | Error msg -> failwithf "Configuration error: %s - %s" def.Name msg
    | Optional, None -> None
    | Optional, Some value when String.IsNullOrWhiteSpace(value) -> None
    | Optional, Some value ->
        match parseValue def.ValueType value with
        | Ok cv -> Some cv
        | Error msg -> failwithf "Configuration error: %s - %s" def.Name msg

/// Read string config var - returns empty string if not set
let readString (def: ConfigVarDef) =
    read def |> ConfigValue.stringOption |> Option.defaultValue ""

/// Read int config var - fails if not set or invalid
let readInt (def: ConfigVarDef) = read def |> ConfigValue.int

/// Read bool config var - fails if not set or invalid
let readBool (def: ConfigVarDef) = read def |> ConfigValue.bool

/// Read int with default (for optional vars or missing values)
let readIntOrDefault (def: ConfigVarDef) (defaultValue: int) =
    read def |> ConfigValue.intOption |> Option.defaultValue defaultValue

/// Read bool with default (for optional vars or missing values)
let readBoolOrDefault (def: ConfigVarDef) (defaultValue: bool) =
    read def |> ConfigValue.boolOption |> Option.defaultValue defaultValue

/// Validate all required vars are set and have valid values.
/// Returns a list of error messages for any missing or invalid vars.
let validateRequired (defs: ConfigVarDef seq) : string list =
    defs
    |> Seq.filter (fun d -> d.Requirement = Required)
    |> Seq.choose (fun def ->
        // fsharplint:disable-next-line Hints
        let rawValue = Environment.GetEnvironmentVariable(def.Name) |> Option.ofObj

        match rawValue with
        | None -> Some $"%s{def.Name}: required but not set"
        | Some value when String.IsNullOrWhiteSpace(value) -> Some $"%s{def.Name}: required but empty"
        | Some value ->
            match parseValue def.ValueType value with
            | Ok _ -> None
            | Error msg -> Some $"%s{def.Name}: %s{msg}")
    |> Seq.toList
