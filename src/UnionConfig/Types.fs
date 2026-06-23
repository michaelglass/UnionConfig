/// Reusable configuration types and utilities.
/// This module contains generic types for building type-safe configuration systems.
module UnionConfig.Types

open System
open System.Globalization

// =============================================================================
// Provenance Model (ADR 0049)
// =============================================================================
//
// Provenance — *where a value comes from* — is the root axis of the model. It
// DERIVES whether a value is persisted, how an operator may change it, and (when
// fetched live) where it is read. The orthogonal per-var axes — `ConfigRequirement`,
// `Default`, `IsSecret`, `ConfigValueType` — are independent and never folded into
// provenance. The old flat `ConfigVarKind` conflated all of these and let one case
// (`External`) double as a "don't-persist" flag; the shapes below make those bad
// states unrepresentable.

/// How a *provisioned* value is retrieved when (re)assembling config.
///
/// `'FetchSource` is supplied by the consumer so UnionConfig stays platform-neutral:
/// the consumer names its own concrete sources (CloudFormation stack outputs, CFN
/// pseudo-parameters, …). The parameter threads through `Provenance` and `ConfigVarDef`.
type Retrieval<'FetchSource> =
    /// The provisioning source can't be re-read, so the captured value is cached in the
    /// config store and read back from there. Pick this when nothing can re-derive the
    /// value; it changes only by re-provisioning.
    | Cached
    /// The provisioning source IS re-readable, so the value is fetched live from its
    /// `'FetchSource` on every assembly and is NEVER cached. Pick this for stack outputs
    /// and pseudo-parameters. The compiler forces every such var to name its source, so
    /// no hand-maintained fetch list can be forgotten.
    | Fetched of source: 'FetchSource

/// Where a configuration variable's value comes from — the root axis of the model.
/// Choose the case that matches *who authors the value and how it changes*; the
/// derived persistence / mutation / fetch behaviour follows automatically via
/// `Provenance.behavior`.
type Provenance<'FetchSource> =
    /// Authored by a human operator and hand-edited via `env edit`. Cached in the store
    /// because there is no other source to re-read. Changes by editing.
    | Operator
    /// A secret the system mints itself (signing keys, cookie secrets, upload tokens).
    /// Cached in the store; changes only by *rotation*, never hand-edited.
    | SystemGenerated
    /// Produced by an external provisioning step (infrastructure, a third-party setup
    /// script). `Retrieval` decides whether the captured value is `Cached` (source not
    /// re-readable) or `Fetched` live (source re-readable). Changes by re-provisioning,
    /// never hand-edited.
    | Provisioned of retrieval: Retrieval<'FetchSource>
    /// Supplied by the runtime/injector at read time (e.g. an RDS IAM auth token).
    /// Read live from the environment, never persisted, never editable.
    | Ambient

/// How an operator changes a config variable's value — DERIVED from provenance,
/// never set by hand. Drives what `env edit` exposes versus what it routes to a
/// rotate / re-provision path.
type AllowedMutation =
    /// Hand-edited by an operator (`env edit`). (`Operator` provenance.)
    | Edit
    /// Rotated — a fresh secret is minted to replace the old one. (`SystemGenerated`.)
    | Rotate
    /// Re-provisioned by re-running the step that produced it. (`Provisioned`.)
    | Reprovision
    /// Not changeable through any config tool — supplied live at runtime with nothing to
    /// mutate. (`Ambient` only; a `Provisioned (Fetched …)` value changes by re-provisioning
    /// its upstream source, so it derives `Reprovision`, not this.)
    | NoMutation

/// The persistence / mutation / fetch behaviour a `Provenance` derives.
/// Produced by `Provenance.behavior`; this is the single place that maps the root
/// axis to its consequences so writers, editors, and seeders never re-derive them
/// inconsistently.
[<NoComparison; NoEquality>]
type ProvenanceBehavior<'FetchSource> =
    {
        /// True when the canonical value lives in the config store (.env / SSM) and is
        /// read back from there. False when fetched live or injected at runtime.
        IsPersisted: bool
        /// How an operator changes this variable (or that it can't be changed).
        AllowedMutation: AllowedMutation
        /// `Some source` when the value is fetched live from that source on every
        /// assembly; `None` when cached, operator-edited, generated, or ambient.
        FetchSource: 'FetchSource option
    }

/// Derivations from a variable's provenance. The `.env` writer, `env edit`, and the
/// store seeders all route off these helpers rather than re-pattern-matching the cases.
module Provenance =
    /// Map a provenance to its derived persistence / mutation / fetch behaviour.
    let behavior (provenance: Provenance<'FetchSource>) : ProvenanceBehavior<'FetchSource> =
        match provenance with
        | Operator ->
            { IsPersisted = true
              AllowedMutation = Edit
              FetchSource = None }
        | SystemGenerated ->
            { IsPersisted = true
              AllowedMutation = Rotate
              FetchSource = None }
        | Provisioned Cached ->
            { IsPersisted = true
              AllowedMutation = Reprovision
              FetchSource = None }
        | Provisioned(Fetched source) ->
            { IsPersisted = false
              AllowedMutation = Reprovision
              FetchSource = Some source }
        | Ambient ->
            { IsPersisted = false
              AllowedMutation = NoMutation
              FetchSource = None }

    /// True when the variable's canonical value is persisted to the config store.
    /// Replaces the old `ConfigVarKind.isPersistable`: filter defs with this before
    /// seeding SSM or writing the operator-editable `.env`.
    let isPersisted (provenance: Provenance<'FetchSource>) : bool = (behavior provenance).IsPersisted

    /// `Some source` when the value is fetched live, otherwise `None`. The `.env`
    /// writer pattern-matches this to pull stack-derived values from their real source.
    let fetchSource (provenance: Provenance<'FetchSource>) : 'FetchSource option = (behavior provenance).FetchSource

    /// How an operator changes this variable — derived from provenance.
    let allowedMutation (provenance: Provenance<'FetchSource>) : AllowedMutation = (behavior provenance).AllowedMutation

/// The runtime type of a configuration variable's value
[<NoComparison; NoEquality>]
type ConfigValueType =
    /// String value (default)
    | StringType
    /// Integer value
    | IntType
    /// Boolean value (accepts true/false/1/0)
    | BoolType
    /// Float/double value
    | FloatType
    /// Custom type with validation function (typeName, validator that returns error message on failure)
    | CustomType of typeName: string * validate: (string -> string option)

/// Whether a configuration variable is required for the app to start
type ConfigRequirement =
    /// App will fail to start if this var is missing or empty
    | Required
    /// Var can be missing; read returns None
    | Optional

/// What a variable's static default is used for. An orthogonal axis: a default can be
/// a runtime fallback, a store seed, both, or neither — independent of provenance.
/// (Replaces the old dual-purpose `DefaultValue: string option`, which silently did
/// both jobs, and absorbs consumers' separate `seedValueFor` shadow tables.)
type Default =
    /// No static default. A `Required` var with no default errors when unset.
    | NoDefault
    /// Seeded into a fresh store entry, but NOT used as a runtime fallback.
    | SeedOnly of seed: string
    /// Used as a runtime fallback when the env var is unset, but NOT seeded into a store.
    | RuntimeFallback of fallback: string
    /// Used both as the store seed value and the runtime fallback.
    | SeedAndFallback of value: string

/// Accessors that split a `Default` into its two independent jobs.
module Default =
    /// The runtime fallback `Reader.read` returns when the env var is unset, if any.
    let runtimeFallback (d: Default) : string option =
        match d with
        | RuntimeFallback v
        | SeedAndFallback v -> Some v
        | NoDefault
        | SeedOnly _ -> None

    /// The value a store seeder (e.g. `populateDefaults`) writes when the store has no
    /// entry for this var, if any.
    let seed (d: Default) : string option =
        match d with
        | SeedOnly v
        | SeedAndFallback v -> Some v
        | NoDefault
        | RuntimeFallback _ -> None

// =============================================================================
// Parsed Configuration Values
// =============================================================================

/// Parsed configuration value with its type
[<NoComparison; NoEquality>]
type ConfigValue =
    | StringValue of string
    | IntValue of int
    | BoolValue of bool
    | FloatValue of float

// =============================================================================
// Documentation Metadata
// =============================================================================

/// Documentation metadata for a config variable
[<NoComparison; NoEquality>]
type ConfigVarDoc =
    { Description: string
      HowToFind: string
      ManagementUrl: Uri option }

/// Core definition of a config variable (enough to read, parse, validate, and route).
///
/// Generic over `'FetchSource` (via `Provenance`) so the consumer's concrete fetch
/// sources flow all the way into the def — letting a writer pattern-match a var's
/// provenance and have the compiler force every `Fetched` var to name a source.
[<NoComparison; NoEquality>]
type ConfigVarDef<'FetchSource> =
    {
        Name: string
        /// Where the value comes from (root axis). Derives persistence / mutation /
        /// fetch via `Provenance.behavior`.
        Provenance: Provenance<'FetchSource>
        ValueType: ConfigValueType
        Requirement: ConfigRequirement
        IsSecret: bool
        /// What the static default (if any) is used for — runtime fallback, store seed,
        /// both, or neither. See `Default`.
        Default: Default
        Doc: ConfigVarDoc
    }

// =============================================================================
// Parsing Utilities
// =============================================================================

/// Parse a string as a boolean value
/// Accepts: "true", "false", "1", "0" (case-insensitive, trimmed)
let parseBool (s: string) : bool option =
    match s.Trim().ToLowerInvariant() with
    | "true"
    | "1" -> Some true
    | "false"
    | "0" -> Some false
    | _ -> None

/// Parse a string value into the expected type.
let parseValue (valueType: ConfigValueType) (rawValue: string) : Result<ConfigValue, string> =
    match valueType with
    | StringType -> Ok(StringValue rawValue)
    | IntType ->
        match Int32.TryParse(rawValue) with
        | true, i -> Ok(IntValue i)
        | false, _ -> Error $"Expected integer, got '%s{rawValue}'"
    | BoolType ->
        match parseBool rawValue with
        | Some b -> Ok(BoolValue b)
        | None -> Error $"Expected true/false/1/0, got '%s{rawValue}'"
    | FloatType ->
        match Double.TryParse(rawValue, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, f -> Ok(FloatValue f)
        | false, _ -> Error $"Expected float, got '%s{rawValue}'"
    | CustomType(typeName, validate) ->
        match validate rawValue with
        | None -> Ok(StringValue rawValue) // Valid custom type, store as string
        | Some errorMsg -> Error $"Invalid %s{typeName}: %s{errorMsg}"

// =============================================================================
// ConfigValue Extraction Helpers
// =============================================================================

/// Helpers for extracting typed values from ConfigValue.
/// Use these with a `read` function that returns ConfigValue option:
///   let port = read PORT |> ConfigValue.int
///   let apiKey = read XAI_API_KEY |> ConfigValue.stringOption
module ConfigValue =
    /// Convert any ConfigValue to string representation
    let private valueToString =
        function
        | StringValue s -> s
        | IntValue i -> string<int> i
        | BoolValue b -> if b then "true" else "false"
        | FloatValue f -> string<float> f

    /// Name of the ConfigValue case actually received, for mismatch messages.
    let private caseName =
        function
        | StringValue _ -> "StringValue"
        | IntValue _ -> "IntValue"
        | BoolValue _ -> "BoolValue"
        | FloatValue _ -> "FloatValue"

    /// Extract string from Required config (converts any type to string)
    let string (cv: ConfigValue option) : string =
        match cv with
        | Some v -> valueToString v
        | None -> failwith "Configuration value is None for required var"

    /// Extract string from Optional config (converts any type to string)
    let stringOption (cv: ConfigValue option) : string option = cv |> Option.map valueToString

    /// Extract int from Required config
    let int (cv: ConfigValue option) : int =
        match cv with
        | Some(IntValue i) -> i
        | Some(StringValue s) ->
            match Int32.TryParse(s) with
            | true, i -> i
            | false, _ -> failwithf "Cannot convert '%s' to int" s
        | Some other -> failwithf "Type mismatch: expected IntValue, got %s" (caseName other)
        | None -> failwith "Configuration value is None for required var"

    /// Extract int from Optional config
    let intOption (cv: ConfigValue option) : int option =
        match cv with
        | Some(IntValue i) -> Some i
        | Some(StringValue s) ->
            match Int32.TryParse(s) with
            | true, i -> Some i
            | false, _ -> failwithf "Cannot convert '%s' to int" s
        | Some other -> failwithf "Type mismatch: expected IntValue, got %s" (caseName other)
        | None -> None

    /// Extract bool from Required config
    let bool (cv: ConfigValue option) : bool =
        match cv with
        | Some(BoolValue b) -> b
        | Some other -> failwithf "Type mismatch: expected BoolValue, got %s" (caseName other)
        | None -> failwith "Configuration value is None for required var"

    /// Extract bool from Optional config
    let boolOption (cv: ConfigValue option) : bool option =
        match cv with
        | Some(BoolValue b) -> Some b
        | Some other -> failwithf "Type mismatch: expected BoolValue, got %s" (caseName other)
        | None -> None

    /// Extract float from Required config
    let float (cv: ConfigValue option) : float =
        match cv with
        | Some(FloatValue f) -> f
        | Some(StringValue s) ->
            match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, f -> f
            | false, _ -> failwithf "Cannot convert '%s' to float" s
        | Some other -> failwithf "Type mismatch: expected FloatValue, got %s" (caseName other)
        | None -> failwith "Configuration value is None for required var"

    /// Extract float from Optional config
    let floatOption (cv: ConfigValue option) : float option =
        match cv with
        | Some(FloatValue f) -> Some f
        | Some(StringValue s) ->
            match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, f -> Some f
            | false, _ -> failwithf "Cannot convert '%s' to float" s
        | Some other -> failwithf "Type mismatch: expected FloatValue, got %s" (caseName other)
        | None -> None

    /// Parse custom type from Required config using provided parser
    let custom<'T> (parser: string -> 'T option) (cv: ConfigValue option) : 'T =
        match cv with
        | Some v ->
            let s = valueToString v

            match parser s with
            | Some parsed -> parsed
            | None -> failwithf "Failed to parse custom type from '%s'" s
        | None -> failwith "Configuration value is None for required var"

    /// Parse custom type from Optional config using provided parser
    let customOption<'T> (parser: string -> 'T option) (cv: ConfigValue option) : 'T option =
        match cv with
        | Some v ->
            let s = valueToString v

            match parser s with
            | Some parsed -> Some parsed
            | None -> failwithf "Failed to parse custom type from '%s'" s
        | None -> None
