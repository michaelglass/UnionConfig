/// Reading config vars from environment variables.
module UnionConfig.Reader

open System
open UnionConfig.Types

/// Read a config var from environment, parse to typed value.
/// Returns Result for composable error handling:
/// - Required missing/empty → Error with message (or the `Default` runtime fallback if set)
/// - Required invalid → Error with parse message
/// - Optional missing/empty → Ok None (or the `Default` runtime fallback if set)
/// - Optional invalid → Error with parse message
/// - Valid → Ok (Some value)
///
/// Only the runtime-fallback part of `Default` (`RuntimeFallback` / `SeedAndFallback`)
/// applies here, and only when the environment variable is missing or whitespace-only.
/// A set env var always wins. The fallback itself is parsed through `parseValue`, so a
/// malformed default surfaces as a parse Error.
let read (def: ConfigVarDef<'FetchSource>) : Result<ConfigValue option, string> =
    // fsharplint:disable-next-line Hints
    let rawValue = Environment.GetEnvironmentVariable(def.Name) |> Option.ofObj

    let isAbsent =
        match rawValue with
        | None -> true
        | Some v -> String.IsNullOrWhiteSpace v

    // When no runtime fallback is configured, keep the raw value so the match below can
    // still distinguish "required but not set" from "required but empty".
    let effective =
        match Default.runtimeFallback def.Default with
        | Some d when isAbsent -> Some d
        | _ -> rawValue

    match def.Requirement, effective with
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
let readString (def: ConfigVarDef<'FetchSource>) =
    read def |> unwrap |> ConfigValue.stringOption |> Option.defaultValue ""

/// Read int config var - fails if not set or invalid
let readInt (def: ConfigVarDef<'FetchSource>) = read def |> unwrap |> ConfigValue.int

/// Read bool config var - fails if not set or invalid
let readBool (def: ConfigVarDef<'FetchSource>) = read def |> unwrap |> ConfigValue.bool

/// Read int with default (for optional vars or missing values)
let readIntOrDefault (def: ConfigVarDef<'FetchSource>) (defaultValue: int) =
    read def |> unwrap |> ConfigValue.intOption |> Option.defaultValue defaultValue

/// Read bool with default (for optional vars or missing values)
let readBoolOrDefault (def: ConfigVarDef<'FetchSource>) (defaultValue: bool) =
    read def |> unwrap |> ConfigValue.boolOption |> Option.defaultValue defaultValue

/// Validate all required vars are set and have valid values.
/// Returns a list of error messages for any missing or invalid vars.
let validateRequired (defs: ConfigVarDef<'FetchSource> seq) : string list =
    defs
    |> Seq.filter (fun d -> d.Requirement = Required)
    |> Seq.choose (fun def ->
        match read def with
        | Ok _ -> None
        | Error msg -> Some msg)
    |> Seq.toList
