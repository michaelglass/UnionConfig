<!-- sync:intro:start -->
# UnionConfig

Type-safe configuration for F#, built around discriminated unions. The goal: define
your config vars as a DU, then read them from env vars, `.env` files, or AWS SSM
Parameter Store with typed parsing, validation, and secret masking.

Aims to stay one package with zero external dependencies.

```fsharp
type AppConfig =
    | DatabaseUrl
    | ApiKey
    | MaxRetries
    | DebugMode

let configDef =
    function
    | DatabaseUrl ->
        { Name = "DATABASE_URL"
          Provenance = Operator
          ValueType = StringType
          Requirement = Required
          IsSecret = false
          Default = NoDefault
          Doc =
            { Description = "PostgreSQL connection string"
              HowToFind = "Check your database provider dashboard"
              ManagementUrl = None } }
    | ApiKey ->
        { Name = "API_KEY"
          Provenance = Operator
          ValueType = StringType
          Requirement = Required
          IsSecret = true
          Default = NoDefault
          Doc =
            { Description = "External API key"
              HowToFind = "Generate at https://dashboard.example.com/keys"
              ManagementUrl = Some(Uri "https://dashboard.example.com/keys") } }
    | MaxRetries ->
        { Name = "MAX_RETRIES"
          Provenance = Operator
          ValueType = IntType
          Requirement = Optional
          IsSecret = false
          Default = RuntimeFallback "3"
          Doc =
            { Description = "Max retry attempts"
              HowToFind = "Set to desired retry count (default: 3)"
              ManagementUrl = None } }
    | DebugMode ->
        { Name = "DEBUG_MODE"
          Provenance = Operator
          ValueType = BoolType
          Requirement = Optional
          IsSecret = false
          Default = RuntimeFallback "false"
          Doc =
            { Description = "Enable debug logging"
              HowToFind = "Set to true or 1 to enable"
              ManagementUrl = None } }
```
<!-- sync:intro:end -->

The **provenance** — where a value comes from — is the root axis. It derives whether a
value is persisted, how it changes, and (when fetched) where it's read. The orthogonal
per-var axes (`ConfigRequirement`, `Default`, `IsSecret`, `ConfigValueType`) stay
independent. See [Provenance](#provenance) below.

> **Early alpha.** This library is young and substantially AI-written. Behavior and
> APIs may shift between versions, so pin a version and expect rough edges. Issues and
> PRs are very welcome.

## Installation

```bash
dotnet add package UnionConfig
```

## Reading Config

```fsharp
open UnionConfig.Types
open UnionConfig.Reader
```

<!-- sync:reading:start src=examples/ExampleApp/Program.fs -->
```fsharp
// Typed reads — fail if the var is unset (or, for *OrDefault, fall back)
let dbUrl = readString (configDef DatabaseUrl)
let retries = readInt (configDef MaxRetries)
let debug = readBool (configDef DebugMode)
let dbPort = readIntOrDefault (configDef DatabasePort) 5432

// read returns Result<ConfigValue option, string>
let logLevel =
    match read (configDef LogLevel) with
    | Ok(Some(StringValue level)) -> level
    | Ok _ -> "(not set)"
    | Error msg -> $"error: {msg}"
```
<!-- sync:reading:end -->

<!-- sync:extraction:start -->
## ConfigValue Extraction

Pipeline-friendly typed extraction:

```fsharp
let dbUrl = read (configDef DatabaseUrl) |> Result.map ConfigValue.stringOption
let retries = read (configDef MaxRetries) |> Result.map ConfigValue.intOption
let debug = read (configDef DebugMode) |> Result.map ConfigValue.boolOption
let timeout = read (configDef RequestTimeout) |> Result.map ConfigValue.floatOption

// Custom parsing
let parseLogLevel (s: string) =
    match s.ToLowerInvariant() with
    | "debug" | "info" | "warn" | "error" -> Some(s.ToUpperInvariant())
    | _ -> None

let level = read (configDef LogLevel) |> Result.map (ConfigValue.customOption parseLogLevel)
```
<!-- sync:extraction:end -->

<!-- sync:registry:start -->
## ConfigRegistry

Reflection-based DU case discovery -- no manual `allCases` arrays:

```fsharp
open UnionConfig.ConfigRegistry

// Flat discovery
let allDefs = allDefs configDef
let lookup = byName allDefs
let varNames = names allDefs

// Grouped discovery (nested DUs -> groups by wrapper case name)
let grouped = allDefsGrouped configDef
// Returns: (string * ConfigVarDef array) array
```
<!-- sync:registry:end -->

## Validation

<!-- sync:validation:start src=examples/ExampleApp/Program.fs -->
```fsharp
// validateRequired returns one message per required-but-unset var
let errors = validateRequired allDefs
```
<!-- sync:validation:end -->

<!-- sync:provenance-doc:start -->
## Provenance

`Provenance` — *where a value comes from* — is the root axis. It **derives** whether a
value is persisted, how an operator changes it, and (when fetched) where it's read.
That derivation lives in one place (`Provenance.behavior`), so writers, editors, and
seeders never re-decide it inconsistently.

```fsharp
type Retrieval<'FetchSource> =
    | Cached                       // source not re-readable → cache the captured value
    | Fetched of 'FetchSource      // source re-readable → fetch live, never cache

type Provenance<'FetchSource> =
    | Operator                     // hand-edited via `env edit`; cached
    | SystemGenerated              // a secret we mint; changes by rotation; cached
    | Provisioned of Retrieval<'FetchSource>   // external step; cached or fetched
    | Ambient                      // runtime/injector; read live, never persisted
```

| Provenance               | Persisted | Mutation      | Fetch source |
| ------------------------ | --------- | ------------- | ------------ |
| `Operator`               | yes       | `Edit`        | —            |
| `SystemGenerated`        | yes       | `Rotate`      | —            |
| `Provisioned Cached`     | yes       | `Reprovision` | —            |
| `Provisioned (Fetched s)`| **no**    | `Reprovision` | `Some s`     |
| `Ambient`                | **no**    | `NoMutation`  | —            |

`'FetchSource` is **consumer-defined** so UnionConfig stays platform-neutral: you name
your own concrete sources (stack outputs, pseudo-parameters), and the compiler forces
every `Fetched` var to name one — no hand-maintained fetch list can be forgotten.

```fsharp
// A re-readable infra value: fetched live from a stack output, never cached.
| CacheHost ->
    { Name = "CACHE_HOST"
      Provenance = Provisioned (Fetched (StackOutput "CacheEndpoint"))
      ValueType = StringType
      Requirement = Optional
      IsSecret = false
      Default = NoDefault
      Doc = { Description = "Redis cache hostname from the infra stack output"
              HowToFind = "Read live from the CacheEndpoint stack output"
              ManagementUrl = None } }
```

Persistence is **derived**, not a separate flag — filter with `Provenance.isPersisted`
before seeding SSM or writing the editable `.env` (skips `Fetched` and `Ambient` vars):

<!-- sync:provenance:start src=examples/ExampleApp/Program.fs -->
```fsharp
// Persistence is DERIVED from provenance, not a separate flag. Filter with
// Provenance.isPersisted before seeding SSM or writing the editable .env —
// Fetched (live) and Ambient (runtime-injected) vars are never persisted.
let persisted, notPersisted =
    allDefs |> List.partition (fun d -> Provenance.isPersisted d.Provenance)
```
<!-- sync:provenance:end -->
<!-- sync:provenance-doc:end -->

<!-- sync:defaults:start -->
## Static Defaults

`Default` is an orthogonal axis describing what a var's static default is used for —
independent of provenance:

- **`RuntimeFallback`** — `Reader.read` returns it when the env var is unset (a set env
  var always wins; it's parsed through `ValueType`, so a malformed default is an `Error`).
- **`SeedOnly`** — seeders (`.env` / store, e.g. `populateDefaults`) write it when the
  store has no entry for the var; *not* a runtime fallback.
- **`SeedAndFallback`** — both of the above. **`NoDefault`** — neither.

```fsharp
| AwsRegion ->
    { Name = "AWS_REGION"
      Provenance = Operator
      ValueType = StringType
      Requirement = Required
      IsSecret = false
      Default = SeedAndFallback "eu-central-1"
      Doc = { Description = "AWS region"
              HowToFind = "Defaults to eu-central-1 if not set"
              ManagementUrl = None } }
```
<!-- sync:defaults:end -->

<!-- sync:envfile:start -->
## .env File Operations

```fsharp
open UnionConfig.EnvFile

let config = readEnvFile ".env"

// Diff two configs
let changes = compareConfigs staging prod
displayChanges changes

// Secret masking (PASSWORD, SECRET, KEY, API_KEY, SIGNING, TOKEN)
let display = maskValue "API_KEY" "sk-1234"  // "sk-1***"

// Write sectioned .env file from grouped defs
let grouped = allDefsGrouped configDef
let sections = defaultSections grouped currentValues

// Generate a "MISSING CONFIG" banner for required entries without values
let allDefs = allDefs configDef
let headerLines = missingEntriesHeader allDefs currentValues

writeEnvFile ".env" headerLines sections
```

The generated `.env` file includes a header highlighting missing required entries:

```env
# ════════════════════════════════════════════════════════════════
# MISSING CONFIG — fill these in first
# ════════════════════════════════════════════════════════════════
#
# [Operator] API_KEY — External API key

# === Database ===
# PostgreSQL connection string
DATABASE_URL=
...
```
<!-- sync:envfile:end -->

## Verification

```fsharp
open UnionConfig.Verification
```

<!-- sync:verification:start src=examples/ExampleApp/Program.fs -->
```fsharp
let results =
    allDefs
    |> List.map (fun def ->
        // Provenance routes verification: live/injected vars are skipped (their
        // value isn't owned here); operator/generated vars must be present.
        let result =
            match def.Provenance with
            | Provisioned(Fetched _) -> VerifySkipped "fetched live from its source"
            | Provisioned Cached -> VerifySkipped "provisioned (cached value)"
            | Ambient -> VerifySkipped "injected at runtime"
            | Operator
            | SystemGenerated ->
                match read def with
                | Ok(Some _) -> VerifySuccess $"set (%A{def.ValueType})"
                | _ -> VerifyFailed "not set"

        (def.Name, result))
    |> Array.ofList

displayVerificationResults results
```
<!-- sync:verification:end -->

## AWS SSM Parameter Store

UnionConfig ships an SSM config store designed to work with any parameter store backend. You provide the operations; it handles path mapping, secret detection, and change application.

```fsharp
open UnionConfig.SsmConfigStore
```

<!-- sync:ssm:start src=examples/ExampleApp/Program.fs -->
```fsharp
// Provide your own SsmOperations (e.g. via AWSSDK.SSM). Here we use an
// in-memory store so the example runs without AWS.
let mutable paramStore = Map.empty<string, string>

let operations: SsmOperations =
    { GetParameter = fun path -> Map.tryFind path paramStore
      SetParameter =
        fun path value _isSecure ->
            paramStore <- Map.add path value paramStore
            Ok()
      DeleteParameter =
        fun path ->
            paramStore <- Map.remove path paramStore
            Ok()
      GetParametersByPath = fun prefix -> paramStore |> Map.filter (fun k _ -> k.StartsWith(prefix)) |> Map.toList }

// Map config var names to parameter-store paths, and flag which are secret.
let store: SsmConfigStore =
    { Operations = operations
      PathMapping =
        { ToPath = fun name -> $"/myapp/staging/%s{name}"
          FromPath = fun path -> path.Replace("/myapp/staging/", "")
          PathPrefix = "/myapp/staging/" }
      IsSecret = fun name -> name = "API_KEY" }

let varNames = allDefs |> List.map (fun d -> d.Name) |> Array.ofList

let setResult = setValue store "DATABASE_URL" "postgresql://localhost/myapp" // Result<unit, string>
let value = getValue store "DATABASE_URL" // string option
let all = loadAll store varNames // Map<string, string>
```
<!-- sync:ssm:end -->

<!-- sync:texteditor:start -->
## Interactive Editor

```fsharp
open UnionConfig.ConfigEditor

// Apply defaults without opening editor
let result = populateDefaults getValueFn setValueFn getDefaultsFn writeLocalFileFn

// Full workflow: load -> $EDITOR -> diff -> verify -> confirm -> apply
editConfig loadConfigFn setValueFn writeConfigFileFn verifyChangesFn
```
<!-- sync:texteditor:end -->

## Reference

See the [Example App](examples/ExampleApp/Program.fs) for a complete working example. Run with `mise run example`.

<!-- sync:types:start -->
### Types

```fsharp
type Retrieval<'FetchSource> = Cached | Fetched of source: 'FetchSource
type Provenance<'FetchSource> = Operator | SystemGenerated
                             | Provisioned of Retrieval<'FetchSource> | Ambient
type AllowedMutation = Edit | Rotate | Reprovision | NoMutation
type ProvenanceBehavior<'FetchSource> = {
    IsPersisted: bool; AllowedMutation: AllowedMutation; FetchSource: 'FetchSource option }
type Default = NoDefault | SeedOnly of string | RuntimeFallback of string | SeedAndFallback of string
type ConfigValueType = StringType | IntType | BoolType | FloatType
                     | CustomType of typeName: string * validate: (string -> string option)
type ConfigRequirement = Required | Optional
type ConfigVarDef<'FetchSource> = {
    Name: string; Provenance: Provenance<'FetchSource>; ValueType: ConfigValueType
    Requirement: ConfigRequirement; IsSecret: bool
    Default: Default; Doc: ConfigVarDoc }
type ConfigValue = StringValue of string | IntValue of int | BoolValue of bool | FloatValue of float
```
<!-- sync:types:end -->

<!-- sync:keyfunctions:start -->
### Key Functions

```fsharp
// Reader  (generic over the consumer-defined 'FetchSource; written `Def` here for brevity)
Reader.read              : ConfigVarDef<'F> -> Result<ConfigValue option, string>
Reader.readString        : ConfigVarDef<'F> -> string
Reader.readInt           : ConfigVarDef<'F> -> int
Reader.readBool          : ConfigVarDef<'F> -> bool
Reader.readIntOrDefault  : ConfigVarDef<'F> -> int -> int
Reader.readBoolOrDefault : ConfigVarDef<'F> -> bool -> bool
Reader.validateRequired  : ConfigVarDef<'F> seq -> string list

// Provenance (derivation — replaces ConfigVarKind.isPersistable)
Provenance.behavior         : Provenance<'F> -> ProvenanceBehavior<'F>
Provenance.isPersisted      : Provenance<'F> -> bool          // false for Fetched / Ambient
Provenance.fetchSource      : Provenance<'F> -> 'F option     // Some only for Fetched
Provenance.allowedMutation  : Provenance<'F> -> AllowedMutation

// Default (the static-default axis)
Default.runtimeFallback     : Default -> string option        // RuntimeFallback / SeedAndFallback
Default.seed                : Default -> string option         // SeedOnly / SeedAndFallback

// ConfigValue extraction
ConfigValue.string / stringOption  : ConfigValue option -> string / string option
ConfigValue.int / intOption        : ConfigValue option -> int / int option
ConfigValue.bool / boolOption      : ConfigValue option -> bool / bool option
ConfigValue.float / floatOption    : ConfigValue option -> float / float option
ConfigValue.custom / customOption  : (string -> 'T option) -> ConfigValue option -> 'T / 'T option

// ConfigRegistry  ('FetchSource inferred from toDef; call as `allDefs configDef`)
ConfigRegistry.allDefs         : ('T -> ConfigVarDef<'F>) -> ConfigVarDef<'F> array
ConfigRegistry.allDefsGrouped  : ('T -> ConfigVarDef<'F>) -> (string * ConfigVarDef<'F> array) array
ConfigRegistry.byName          : ConfigVarDef<'F> array -> Map<string, ConfigVarDef<'F>>
ConfigRegistry.names           : ConfigVarDef<'F> array -> string array

// EnvFile
EnvFile.readEnvFile           : string -> Map<string, string>
EnvFile.writeEnvFile          : string -> string list -> EnvFileSection array -> unit
EnvFile.defaultSections       : (string * ConfigVarDef<'F> array) array -> Map<string, string> -> EnvFileSection array
EnvFile.missingEntriesHeader  : ConfigVarDef<'F> array -> Map<string, string> -> string list
EnvFile.compareConfigs        : Map<string, string> -> Map<string, string> -> (string * string * string) array
EnvFile.maskValue             : string -> string -> string
EnvFile.displayChanges        : (string * string * string) array -> unit

// SsmConfigStore
SsmConfigStore.getValue      : SsmConfigStore -> string -> string option
SsmConfigStore.setValue      : SsmConfigStore -> string -> string -> Result<unit, string>
SsmConfigStore.deleteValue   : SsmConfigStore -> string -> Result<unit, string>
SsmConfigStore.loadAll       : SsmConfigStore -> string array -> Map<string, string>
SsmConfigStore.applyChanges  : SsmConfigStore -> (string * string * string) array -> (string * Result<unit, string> * bool) array

// ConfigEditor
ConfigEditor.populateDefaults : (string -> string option) -> ... -> PopulateResult
ConfigEditor.editConfig       : (unit -> Map<string, string>) -> ... -> unit

// Verification
Verification.displayVerificationResults : (string * VerificationResult) array -> unit

// Parsing
parseBool  : string -> bool option
parseValue : ConfigValueType -> string -> Result<ConfigValue, string>
```
<!-- sync:keyfunctions:end -->

<!-- sync:license:start -->
## License

MIT
<!-- sync:license:end -->
