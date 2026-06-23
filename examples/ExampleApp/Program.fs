/// Example application demonstrating the full UnionConfig public API.
/// Run with: dotnet run --project examples/ExampleApp
module ExampleApp.Program

open System
open System.IO
open UnionConfig.Types
open UnionConfig.Reader
open UnionConfig.EnvFile
open UnionConfig.Verification
open UnionConfig.SsmConfigStore
open UnionConfig.ConfigEditor

// =============================================================================
// 1. Define config variables as a discriminated union
// =============================================================================

/// Where this app's `Fetched` provisioned vars are read from, live, on each
/// assembly. Consumer-defined so UnionConfig stays platform-neutral — here, an
/// infra stack's outputs. The compiler forces every `Fetched` var to name one.
type FetchSource = StackOutput of name: string

/// All configuration variables for the example app.
/// Using a DU means the compiler enforces exhaustive handling.
type AppConfig =
    | DatabaseUrl
    | DatabasePort
    | ApiKey
    | MaxRetries
    | DebugMode
    | LogLevel
    | FeatureNewUi
    | RequestTimeout
    | SessionSecret
    | CacheHost
    | WebhookSecret
    | IamAuthToken

/// Map each DU case to its ConfigVarDef. The `Provenance` (root axis) is chosen per
/// var; persistence / mutation / fetch-source then follow via `Provenance.behavior`.
/// `FetchSource` is this app's concrete type, so `ConfigVarDef<FetchSource>`.
let configDef: AppConfig -> ConfigVarDef<FetchSource> =
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
    | DatabasePort ->
        { Name = "DATABASE_PORT"
          Provenance = Operator
          ValueType = IntType
          Requirement = Optional
          IsSecret = false
          // Operator-tunable, but seed a sensible default AND fall back to it at runtime.
          Default = SeedAndFallback "5432"

          Doc =
            { Description = "Database port number"
              HowToFind = "Usually 5432 for PostgreSQL"
              ManagementUrl = None } }
    | ApiKey ->
        { Name = "API_KEY"
          Provenance = Operator
          ValueType = StringType
          Requirement = Required
          IsSecret = true
          Default = NoDefault

          Doc =
            { Description = "External API key for third-party service"
              HowToFind = "Generate at https://dashboard.example.com/keys"
              ManagementUrl = Some(Uri "https://dashboard.example.com/keys") } }
    | MaxRetries ->
        { Name = "MAX_RETRIES"
          Provenance = Operator
          ValueType = IntType
          Requirement = Optional
          IsSecret = false
          // Runtime fallback only — not seeded into a store.
          Default = RuntimeFallback "3"

          Doc =
            { Description = "Maximum retry attempts for failed requests"
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
    | LogLevel ->
        { Name = "LOG_LEVEL"
          Provenance = Operator
          ValueType =
            CustomType(
                "LogLevel",
                fun s ->
                    match s.ToLowerInvariant() with
                    | "debug"
                    | "info"
                    | "warn"
                    | "error" -> None
                    | _ -> Some "must be debug, info, warn, or error"
            )
          Requirement = Optional
          IsSecret = false
          Default = NoDefault

          Doc =
            { Description = "Application log level"
              HowToFind = "One of: debug, info, warn, error"
              ManagementUrl = None } }
    | FeatureNewUi ->
        { Name = "FEATURE_NEW_UI"
          Provenance = Operator
          ValueType = BoolType
          Requirement = Optional
          IsSecret = false
          Default = RuntimeFallback "false"

          Doc =
            { Description = "Feature flag: enable new UI"
              HowToFind = "Set to true to enable the redesigned UI"
              ManagementUrl = None } }
    | RequestTimeout ->
        { Name = "REQUEST_TIMEOUT"
          Provenance = Operator
          ValueType = FloatType
          Requirement = Optional
          IsSecret = false
          Default = RuntimeFallback "30.0"

          Doc =
            { Description = "HTTP request timeout in seconds"
              HowToFind = "Set to desired timeout (default: 30.0)"
              ManagementUrl = None } }
    | SessionSecret ->
        { Name = "SESSION_SECRET"
          // A secret the system mints itself; changed by rotation, never hand-edited.
          Provenance = SystemGenerated
          ValueType = StringType
          Requirement = Optional
          IsSecret = true
          Default = NoDefault

          Doc =
            { Description = "Session signing secret minted by the app"
              HowToFind = "Generated at provisioning time; rotate to change"
              ManagementUrl = None } }
    | CacheHost ->
        { Name = "CACHE_HOST"
          // Re-readable infra value: fetched live from a stack output, never cached.
          Provenance = Provisioned(Fetched(StackOutput "CacheEndpoint"))
          ValueType = StringType
          Requirement = Optional
          IsSecret = false
          Default = NoDefault

          Doc =
            { Description = "Redis cache hostname from the infra stack output"
              HowToFind = "Read live from the CacheEndpoint stack output"
              ManagementUrl = None } }
    | WebhookSecret ->
        { Name = "WEBHOOK_SECRET"
          // Provisioned by a setup step whose source is NOT re-readable, so cache it.
          Provenance = Provisioned Cached
          ValueType = StringType
          Requirement = Optional
          IsSecret = true
          Default = NoDefault

          Doc =
            { Description = "Webhook signing secret captured during service setup"
              HowToFind = "Auto-provisioned during service setup; re-provision to change"
              ManagementUrl = None } }
    | IamAuthToken ->
        { Name = "IAM_AUTH_TOKEN"
          // Injected by the runtime each request; read live, never persisted.
          Provenance = Ambient
          ValueType = StringType
          Requirement = Required
          IsSecret = true
          Default = NoDefault

          Doc =
            { Description = "Short-lived IAM auth token injected by the runtime"
              HowToFind = "Set by the operator's shell or sidecar; not persisted to .env or SSM"
              ManagementUrl = None } }

/// All config cases for iteration.
let allConfigs =
    [ DatabaseUrl
      DatabasePort
      ApiKey
      MaxRetries
      DebugMode
      LogLevel
      FeatureNewUi
      RequestTimeout
      SessionSecret
      CacheHost
      WebhookSecret
      IamAuthToken ]

/// All ConfigVarDefs.
let allDefs = allConfigs |> List.map configDef

// =============================================================================
// 2. Demonstrate reading config from environment
// =============================================================================

let demoReadFromEnv () =
    printfn "== Reading Config from Environment =="
    printfn ""

    // Set env vars for the demo
    Environment.SetEnvironmentVariable("DATABASE_URL", "postgresql://localhost:5432/myapp")
    Environment.SetEnvironmentVariable("API_KEY", "sk-demo-key-abc123456")
    Environment.SetEnvironmentVariable("MAX_RETRIES", "5")
    Environment.SetEnvironmentVariable("DEBUG_MODE", "true")
    Environment.SetEnvironmentVariable("LOG_LEVEL", "info")
    Environment.SetEnvironmentVariable("FEATURE_NEW_UI", "1")
    Environment.SetEnvironmentVariable("REQUEST_TIMEOUT", "45.5")
    Environment.SetEnvironmentVariable("CACHE_HOST", "redis.internal.example.com")
    Environment.SetEnvironmentVariable("WEBHOOK_SECRET", "whsec_abc123")
    // External: set by the operator at runtime — UnionConfig still reads it like any env var
    Environment.SetEnvironmentVariable("IAM_AUTH_TOKEN", "iam-tok-runtime-only")

    // sync:reading:start
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
    // sync:reading:end

    printfn "  DATABASE_URL:      %s" dbUrl
    printfn "  MAX_RETRIES:       %d" retries
    printfn "  DEBUG_MODE:        %b" debug
    printfn "  DATABASE_PORT:     %d" dbPort
    printfn "  FEATURE_NEW_UI:    %b" (readBoolOrDefault (configDef FeatureNewUi) false)
    printfn "  LOG_LEVEL:         %s" logLevel

    match read (configDef RequestTimeout) with
    | Ok(Some(FloatValue timeout)) -> printfn "  REQUEST_TIMEOUT:   %.1f" timeout
    | _ -> printfn "  REQUEST_TIMEOUT:   (not set)"

    printfn "  CACHE_HOST:        %s" (readString (configDef CacheHost))

    printfn ""

// =============================================================================
// 3. Demonstrate ConfigValue extraction helpers
// =============================================================================

let demoConfigValueHelpers () =
    printfn "== ConfigValue Extraction Helpers =="
    printfn ""

    /// Unwrap a Result from read, throwing on Error.
    let unwrapRead result =
        match result with
        | Ok v -> v
        | Error(msg: string) -> failwithf "Configuration error: %s" msg

    // ConfigValue.string: extract string (required, fails if None)
    let dbUrl = read (configDef DatabaseUrl) |> unwrapRead |> ConfigValue.string
    printfn "  ConfigValue.string:       %s" dbUrl

    // ConfigValue.stringOption: extract string (optional, returns None if missing)
    let cacheHost = read (configDef CacheHost) |> unwrapRead |> ConfigValue.stringOption
    printfn "  ConfigValue.stringOption:  %A" cacheHost

    // ConfigValue.int: extract int (required)
    let retries = read (configDef MaxRetries) |> unwrapRead |> ConfigValue.int
    printfn "  ConfigValue.int:          %d" retries

    // ConfigValue.intOption: extract int (optional)
    let portOpt = read (configDef DatabasePort) |> unwrapRead |> ConfigValue.intOption
    printfn "  ConfigValue.intOption:    %A" portOpt

    // ConfigValue.bool: extract bool (required)
    let debug = read (configDef DebugMode) |> unwrapRead |> ConfigValue.bool
    printfn "  ConfigValue.bool:         %b" debug

    // ConfigValue.boolOption: extract bool (optional)
    let newUiOpt = read (configDef FeatureNewUi) |> unwrapRead |> ConfigValue.boolOption
    printfn "  ConfigValue.boolOption:   %A" newUiOpt

    // ConfigValue.float: extract float (required)
    let timeout = read (configDef RequestTimeout) |> unwrapRead |> ConfigValue.float
    printfn "  ConfigValue.float:        %.1f" timeout

    // ConfigValue.floatOption: extract float (optional)
    let timeoutOpt =
        read (configDef RequestTimeout) |> unwrapRead |> ConfigValue.floatOption

    printfn "  ConfigValue.floatOption:  %A" timeoutOpt

    // ConfigValue.custom: parse custom type (required)
    let parseLogLevel (s: string) =
        match s.ToLowerInvariant() with
        | "debug" -> Some "DEBUG"
        | "info" -> Some "INFO"
        | "warn" -> Some "WARN"
        | "error" -> Some "ERROR"
        | _ -> None

    let level =
        read (configDef LogLevel) |> unwrapRead |> ConfigValue.custom parseLogLevel

    printfn "  ConfigValue.custom:       %s" level

    // ConfigValue.customOption: parse custom type (optional)
    let levelOpt =
        read (configDef LogLevel)
        |> unwrapRead
        |> ConfigValue.customOption parseLogLevel

    printfn "  ConfigValue.customOption: %A" levelOpt

    printfn ""

// =============================================================================
// 4. Demonstrate direct parsing utilities
// =============================================================================

let demoParsing () =
    printfn "== Parsing Utilities =="
    printfn ""

    // parseBool: parse boolean strings
    let examples = [ "true"; "false"; "1"; "0"; "TRUE"; "yes" ]

    for s in examples do
        printfn "  parseBool \"%s\" = %A" s (parseBool s)

    printfn ""

    // parseValue: parse string to typed ConfigValue
    let parseExamples =
        [ (StringType, "hello")
          (IntType, "42")
          (BoolType, "true")
          (FloatType, "3.14")
          (IntType, "not-a-number") ]

    for (valueType, raw) in parseExamples do
        printfn "  parseValue %A \"%s\" = %A" valueType raw (parseValue valueType raw)

    printfn ""

// =============================================================================
// 5. Demonstrate validation
// =============================================================================

let demoValidation () =
    printfn "== Validating Required Config =="
    printfn ""

    // With all vars set, validation passes
    // sync:validation:start
    // validateRequired returns one message per required-but-unset var
    let errors = validateRequired allDefs
    // sync:validation:end
    printfn "  Errors with all vars set: %d" errors.Length

    // Unset a required var and validate again
    Environment.SetEnvironmentVariable("DATABASE_URL", null)
    let errors2 = validateRequired allDefs
    printfn "  Errors with DATABASE_URL missing: %d" errors2.Length

    for err in errors2 do
        printfn "    - %s" err

    // Restore for remaining demos
    Environment.SetEnvironmentVariable("DATABASE_URL", "postgresql://localhost:5432/myapp")
    printfn ""

// =============================================================================
// 6. Demonstrate .env file operations
// =============================================================================

let demoEnvFile () =
    printfn "== .env File Operations =="
    printfn ""

    // Write a sample .env file
    let envPath = Path.Combine(Path.GetTempPath(), "unionconfig-example.env")

    let content =
        [ "# Example .env file"
          "DATABASE_URL=postgresql://localhost:5432/myapp"
          "DATABASE_PORT=5432"
          "API_KEY=sk-demo-key-abc123456"
          "MAX_RETRIES=3"
          "DEBUG_MODE=false"
          "LOG_LEVEL=info"
          "REQUEST_TIMEOUT=30.0" ]
        |> String.concat "\n"

    File.WriteAllText(envPath, content)

    // readEnvFile: parse .env file into Map
    let config = readEnvFile envPath
    printfn "  Read %d vars from %s" config.Count envPath

    for kvp in config do
        let display = maskValue kvp.Key kvp.Value
        printfn "    %s = %s" kvp.Key display

    printfn ""

    // compareConfigs: detect differences between two config maps
    let staging = config

    let prod =
        staging
        |> Map.add "DATABASE_URL" "postgresql://prod-db.example.com:5432/myapp"
        |> Map.add "DEBUG_MODE" "false"
        |> Map.add "MAX_RETRIES" "5"
        |> Map.remove "LOG_LEVEL"

    let changes = compareConfigs staging prod
    printfn "  Comparing staging vs prod:"

    // displayChanges: show config differences with masking
    displayChanges changes

    // Clean up
    File.Delete(envPath)
    printfn ""

// =============================================================================
// 7. Demonstrate secret masking
// =============================================================================

let demoMasking () =
    printfn "== Secret Masking =="
    printfn ""

    // secretKeyIndicators: the patterns that trigger masking
    printfn "  Secret indicators: %s" (String.Join(", ", secretKeyIndicators))
    printfn ""

    // maskValue: mask sensitive values based on key name
    let examples =
        [ ("DATABASE_URL", "postgresql://localhost:5432/myapp")
          ("API_KEY", "sk-demo-key-abc123456")
          ("JWT_SECRET", "super-long-secret-value-here")
          ("PASSWORD", "p@ss")
          ("DB_HOST", "localhost")
          ("PORT", "5432") ]

    for (key, value) in examples do
        let masked = maskValue key value
        printfn "  %-15s  %s" key masked

    printfn ""

// =============================================================================
// 8. Demonstrate verification
// =============================================================================

let demoVerification () =
    printfn "== Verification Results =="
    printfn ""

    // Build VerificationResult values for each config var
    // sync:verification:start
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
    // sync:verification:end

    printfn ""

// =============================================================================
// 9. Demonstrate documentation metadata
// =============================================================================

let demoDocs () =
    printfn "== Config Variable Documentation =="
    printfn ""

    for config in allConfigs do
        let def = configDef config

        let provenanceStr =
            match def.Provenance with
            | Operator -> "operator"
            | SystemGenerated -> "generated"
            | Provisioned Cached -> "provisioned(cached)"
            | Provisioned(Fetched(StackOutput name)) -> $"fetched(%s{name})"
            | Ambient -> "ambient"

        let defaultStr =
            match def.Default with
            | NoDefault -> ""
            | SeedOnly v -> $" (seed: %s{v})"
            | RuntimeFallback v -> $" (fallback: %s{v})"
            | SeedAndFallback v -> $" (default: %s{v})"

        let reqStr =
            match def.Requirement with
            | Required -> "required"
            | Optional -> "optional"

        let secretStr = if def.IsSecret then " [SECRET]" else ""

        printfn "  %-20s %-10s %-20s%s%s" def.Name reqStr provenanceStr defaultStr secretStr
        printfn "    %s" def.Doc.Description

        match def.Doc.ManagementUrl with
        | Some url -> printfn "    URL: %O" url
        | None -> ()

    printfn ""

// =============================================================================
// 10. Demonstrate openInEditor (requires interactive terminal + $EDITOR)
// =============================================================================

let demoOpenInEditor () =
    printfn "== openInEditor =="
    printfn ""

    // openInEditor: opens a file in $EDITOR and waits for it to close
    // Requires an interactive terminal, so we just show how it's wired up
    let editorEnv =
        // fsharplint:disable-next-line Hints
        Environment.GetEnvironmentVariable("EDITOR") |> Option.ofObj

    match editorEnv with
    | None ->
        printfn "  Skipped: $EDITOR not set (set it to e.g. 'vim' or 'code --wait' to try)"
        printfn "  Usage: openInEditor \"vim\" \"/path/to/file.env\""
    | Some editor ->
        printfn "  $EDITOR is '%s'" editor

        if Environment.GetEnvironmentVariable("UNIONCONFIG_DEMO_INTERACTIVE") = "1" then
            let tmpPath = Path.Combine(Path.GetTempPath(), "unionconfig-editor-demo.env")
            File.WriteAllText(tmpPath, "# Edit me!\nFOO=bar\n")

            try
                openInEditor editor tmpPath
                printfn "  Editor closed. File contents:"
                let edited = File.ReadAllText(tmpPath)
                printfn "  %s" (edited.Replace("\n", "\n  "))
            finally
                try
                    File.Delete(tmpPath)
                with _ ->
                    ()
        else
            printfn "  Skipped: set UNIONCONFIG_DEMO_INTERACTIVE=1 to open the editor"
            printfn "  Usage: openInEditor \"%s\" \"/path/to/file.env\"" editor

    printfn ""

// =============================================================================
// 11. Demonstrate SSM Config Store (with in-memory operations)
// =============================================================================

let demoSsmConfigStore () =
    printfn "== SSM Config Store =="
    printfn ""

    // sync:ssm:start
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
    // sync:ssm:end

    printfn "  PathMapping.ToPath \"DATABASE_URL\" = %s" (store.PathMapping.ToPath "DATABASE_URL")

    printfn
        "  PathMapping.FromPath \"/myapp/staging/DATABASE_URL\" = %s"
        (store.PathMapping.FromPath "/myapp/staging/DATABASE_URL")

    printfn ""
    printfn "  setValue:              %A" setResult
    printfn "  getValue (after set):  %A" value
    printfn "  loadAll:               %d entries" all.Count

    // deleteValue: delete a config value (Result<unit, string>)
    let delResult = deleteValue store "DATABASE_URL"
    printfn "  deleteValue:           %A" delResult

    // applyChanges: apply a set of changes (sets and deletes).
    // Each result carries the store's error text on failure.
    let changes = [| ("MAX_RETRIES", "", "5"); ("LOG_LEVEL", "", "info") |]
    let results = applyChanges store changes
    printfn "  applyChanges:          %d results" results.Length

    for (key, result, wasDelete) in results do
        let op = if wasDelete then "delete" else "set"

        let outcome =
            match result with
            | Ok() -> "ok"
            | Error msg -> $"error: %s{msg}"

        printfn "    %s (%s): %s" key op outcome

    printfn ""

// =============================================================================
// 13. Demonstrate TextEditor ConfigEditor (requires interactive terminal)
// =============================================================================

let demoConfigEditor () =
    printfn "== ConfigEditor (TextEditor) =="
    printfn ""

    // populateDefaults: apply default values without opening an editor
    // Uses callback functions so it works with any backend (SSM, local files, etc.)

    // We use an in-memory store to demonstrate without AWS
    let mutable memStore = Map.empty<string, string>

    let getValueFn name = Map.tryFind name memStore

    let setValueFn name value =
        memStore <- Map.add name value memStore
        true

    let getDefaultsFn = defaultsFromDefs allDefs

    let writeLocalFileFn (_config: Map<string, string>) = ()

    let result = populateDefaults getValueFn setValueFn getDefaultsFn writeLocalFileFn

    match result with
    | NoChangesNeeded -> printfn "  populateDefaults: no changes needed"
    | Applied count -> printfn "  populateDefaults: applied %d defaults" count
    | Failed errors -> printfn "  populateDefaults: failed for %A" errors

    printfn "  In-memory store after defaults:"

    for kvp in memStore do
        printfn "    %s = %s" kvp.Key kvp.Value

    printfn ""

    // editConfig: full interactive editor workflow
    // Requires $EDITOR and an interactive terminal, so we describe the wiring
    let editorEnv =
        // fsharplint:disable-next-line Hints
        Environment.GetEnvironmentVariable("EDITOR") |> Option.ofObj

    match editorEnv with
    | None -> printfn "  editConfig: skipped ($EDITOR not set)"
    | Some editor ->
        if Environment.GetEnvironmentVariable("UNIONCONFIG_DEMO_INTERACTIVE") = "1" then
            printfn "  editConfig: launching editor workflow with '%s'..." editor

            let loadConfigFn () = memStore

            let writeConfigFileFn (path: string) (config: Map<string, string>) =
                let lines = config |> Map.toArray |> Array.map (fun (k, v) -> $"%s{k}=%s{v}")

                File.WriteAllLines(path, lines)

            let verifyChangesFn
                (_changes: (string * string * string) array)
                (_config: Map<string, string>)
                : (string * VerificationResult) array =
                // In a real app, you'd verify API keys work, DB connects, etc.
                _changes |> Array.map (fun (key, _, _) -> (key, VerifySuccess "accepted"))

            editConfig loadConfigFn setValueFn writeConfigFileFn verifyChangesFn
        else
            printfn "  editConfig: skipped (set UNIONCONFIG_DEMO_INTERACTIVE=1 to try)"
            printfn "  Usage: editConfig loadConfig setValue writeConfigFile verifyChanges"

    printfn ""

// =============================================================================
// 14. Demonstrate UnionConfig.ConfigRegistry.allDefs (flat DU)
// =============================================================================

let demoConfigRegistryFlat () =
    printfn "== UnionConfig.ConfigRegistry.allDefs (flat DU) =="
    printfn ""

    // allDefs<AppConfig>: discovers all DU cases via reflection and maps to ConfigVarDef
    // No more manual list maintenance!
    let defs = UnionConfig.ConfigRegistry.allDefs configDef
    printfn "  Discovered %d config vars via reflection:" defs.Length

    for def in defs do
        printfn "    %s" def.Name

    printfn ""

    // byName: build a lookup map from the discovered defs
    let lookup = UnionConfig.ConfigRegistry.byName defs
    printfn "  byName lookup for API_KEY: IsSecret=%b" lookup.["API_KEY"].IsSecret

    // names: extract just the var names
    let varNames = UnionConfig.ConfigRegistry.names defs
    printfn "  names: %s" (String.Join(", ", varNames))

    printfn ""

// =============================================================================
// 15. Demonstrate UnionConfig.ConfigRegistry.allDefsGrouped (nested DU)
// =============================================================================

/// Inner DU for database-related config vars.
type DatabaseConfig =
    | DbHost
    | DbPort

/// Inner DU for API-related config vars.
type ApiConfig =
    | ApiEndpoint
    | ApiTimeout

/// Nested DU grouping config vars by category.
/// UnionConfig.ConfigRegistry.allDefsGrouped uses wrapper case names as group names.
type GroupedAppConfig =
    | Database of DatabaseConfig
    | Api of ApiConfig

/// Map grouped config DU to ConfigVarDef. These vars are all operator-authored, so
/// no fetch source is needed — `ConfigVarDef<unit>`.
let groupedConfigDef: GroupedAppConfig -> ConfigVarDef<unit> =
    function
    | Database DbHost ->
        { Name = "DB_HOST"
          Provenance = Operator
          ValueType = StringType
          Requirement = Required
          IsSecret = false
          Default = NoDefault

          Doc =
            { Description = "Database hostname"
              HowToFind = "Check your database provider"
              ManagementUrl = None } }
    | Database DbPort ->
        { Name = "DB_PORT"
          Provenance = Operator
          ValueType = IntType
          Requirement = Optional
          IsSecret = false
          Default = SeedAndFallback "5432"

          Doc =
            { Description = "Database port number"
              HowToFind = "Usually 5432 for PostgreSQL"
              ManagementUrl = None } }
    | Api ApiEndpoint ->
        { Name = "API_ENDPOINT"
          Provenance = Operator
          ValueType = StringType
          Requirement = Required
          IsSecret = false
          Default = NoDefault

          Doc =
            { Description = "Base URL for the API"
              HowToFind = "Check your API provider dashboard"
              ManagementUrl = None } }
    | Api ApiTimeout ->
        { Name = "API_TIMEOUT"
          Provenance = Operator
          ValueType = IntType
          Requirement = Optional
          IsSecret = false
          Default = RuntimeFallback "30"

          Doc =
            { Description = "API request timeout in seconds"
              HowToFind = "Set to desired timeout (default: 30)"
              ManagementUrl = None } }

let demoConfigRegistryGrouped () =
    printfn "== UnionConfig.ConfigRegistry.allDefsGrouped (nested DU) =="
    printfn ""

    // allDefsGrouped: discovers cases and groups by wrapper case name
    let grouped = UnionConfig.ConfigRegistry.allDefsGrouped groupedConfigDef
    printfn "  Found %d groups:" grouped.Length

    for groupName, (defs: ConfigVarDef<unit> array) in grouped do
        printfn "    Group '%s': %d vars" groupName defs.Length

        for d in defs do
            printfn "      - %s (%s)" d.Name d.Doc.Description

    printfn ""

// =============================================================================
// 16. Demonstrate writeEnvFile with defaultSections
// =============================================================================

let demoWriteEnvFile () =
    printfn "== writeEnvFile + defaultSections =="
    printfn ""

    // Use allDefsGrouped to get grouped definitions
    let grouped = UnionConfig.ConfigRegistry.allDefsGrouped groupedConfigDef

    // Simulate current config values
    let currentValues =
        Map.ofList
            [ ("DB_HOST", "localhost")
              ("DB_PORT", "5432")
              ("API_ENDPOINT", "https://api.example.com")
              ("API_TIMEOUT", "60") ]

    // defaultSections: build EnvFileSection array from grouped defs + values
    // Uses group names as section headers and Doc.Description as comments
    let sections = defaultSections grouped currentValues

    // missingEntriesHeader: generate a "MISSING CONFIG" banner for required entries without values
    let allDefs = UnionConfig.ConfigRegistry.allDefs groupedConfigDef
    let headerLines = missingEntriesHeader allDefs currentValues

    let envPath = Path.Combine(Path.GetTempPath(), "unionconfig-sectioned.env")
    writeEnvFile envPath headerLines sections

    printfn "  Wrote sectioned .env file to %s" envPath
    printfn "  Contents:"

    let contents = File.ReadAllText(envPath)

    for line in contents.Split('\n') do
        printfn "    %s" line

    // Clean up
    File.Delete(envPath)
    printfn ""

// =============================================================================
// 17. Demonstrate Reader.read returning Result
// =============================================================================

let demoReadResult () =
    printfn "== Reader.read returning Result =="
    printfn ""

    // read now returns Result<ConfigValue option, string>
    // Ok (Some value) = present and valid
    // Ok None = optional and missing
    // Error msg = required and missing, or invalid value

    // Success case: valid value
    Environment.SetEnvironmentVariable("MAX_RETRIES", "5")

    match read (configDef MaxRetries) with
    | Ok(Some(IntValue n)) -> printfn "  MAX_RETRIES: Ok (Some %d)" n
    | Ok None -> printfn "  MAX_RETRIES: Ok None"
    | Ok(Some _) -> printfn "  MAX_RETRIES: unexpected type"
    | Error msg -> printfn "  MAX_RETRIES: Error \"%s\"" msg

    // Error case: invalid int value
    Environment.SetEnvironmentVariable("MAX_RETRIES", "not-a-number")

    match read (configDef MaxRetries) with
    | Ok _ -> printfn "  MAX_RETRIES (invalid): unexpectedly Ok"
    | Error msg -> printfn "  MAX_RETRIES (invalid): Error \"%s\"" msg

    // Ok None case: optional and missing
    Environment.SetEnvironmentVariable("CACHE_HOST", null)

    match read (configDef CacheHost) with
    | Ok None -> printfn "  CACHE_HOST (unset): Ok None"
    | Ok(Some _) -> printfn "  CACHE_HOST (unset): unexpectedly present"
    | Error msg -> printfn "  CACHE_HOST (unset): Error \"%s\"" msg

    // Error case: required and missing
    Environment.SetEnvironmentVariable("DATABASE_URL", null)

    match read (configDef DatabaseUrl) with
    | Ok _ -> printfn "  DATABASE_URL (unset): unexpectedly Ok"
    | Error msg -> printfn "  DATABASE_URL (unset, required): Error \"%s\"" msg

    // Restore for other demos
    Environment.SetEnvironmentVariable("DATABASE_URL", "postgresql://localhost:5432/myapp")
    Environment.SetEnvironmentVariable("MAX_RETRIES", "5")
    Environment.SetEnvironmentVariable("CACHE_HOST", "redis.internal.example.com")

    printfn ""

// =============================================================================
// 18. Demonstrate Provenance-derived persistence (replaces External + isPersistable)
// =============================================================================

let demoProvenanceRouting () =
    printfn "== Provenance-derived persistence =="
    printfn ""

    // sync:provenance:start
    // Persistence is DERIVED from provenance, not a separate flag. Filter with
    // Provenance.isPersisted before seeding SSM or writing the editable .env —
    // Fetched (live) and Ambient (runtime-injected) vars are never persisted.
    let persisted, notPersisted =
        allDefs |> List.partition (fun d -> Provenance.isPersisted d.Provenance)
    // sync:provenance:end

    printfn "  Persisted (SSM / .env candidates): %d" persisted.Length

    for d in persisted do
        let b = Provenance.behavior d.Provenance
        printfn "    %s (%A, mutate=%A)" d.Name d.Provenance b.AllowedMutation

    printfn ""
    printfn "  Not persisted (fetched live or injected): %d" notPersisted.Length

    for d in notPersisted do
        printfn "    %s (%A) - %s" d.Name d.Provenance d.Doc.Description

    printfn ""

    // Reading an Ambient var works exactly like any other provenance.
    match read (configDef IamAuthToken) with
    | Ok(Some(StringValue tok)) -> printfn "  IAM_AUTH_TOKEN read at runtime: %s" (maskValue "IAM_AUTH_TOKEN" tok)
    | Ok(Some _) -> printfn "  IAM_AUTH_TOKEN: unexpected type"
    | Ok None -> printfn "  IAM_AUTH_TOKEN: not set"
    | Error msg -> printfn "  IAM_AUTH_TOKEN: Error \"%s\"" msg

    printfn ""

// =============================================================================
// 19. Demonstrate the Default axis (runtime fallback)
// =============================================================================

let demoDefaultValue () =
    printfn "== Default axis (runtime fallback) =="
    printfn ""

    let awsRegion =
        { Name = "AWS_REGION_DEMO"
          Provenance = Operator
          ValueType = StringType
          Requirement = Required
          IsSecret = false
          // RuntimeFallback: Reader.read returns it when the env var is unset.
          Default = RuntimeFallback "eu-central-1"
          Doc =
            { Description = "AWS region with static fallback"
              HowToFind = "Defaults to eu-central-1 when unset"
              ManagementUrl = None } }

    Environment.SetEnvironmentVariable("AWS_REGION_DEMO", null)

    match read awsRegion with
    | Ok(Some(StringValue r)) -> printfn "  unset      -> Ok (Some \"%s\")  (from Default fallback)" r
    | other -> printfn "  unset      -> unexpected %A" other

    Environment.SetEnvironmentVariable("AWS_REGION_DEMO", "us-west-2")

    match read awsRegion with
    | Ok(Some(StringValue r)) -> printfn "  set        -> Ok (Some \"%s\")  (env wins)" r
    | other -> printfn "  set        -> unexpected %A" other

    let badIntDef =
        { Name = "RETRY_COUNT_DEMO"
          Provenance = Operator
          ValueType = IntType
          Requirement = Required
          IsSecret = false
          Default = RuntimeFallback "not-an-int"
          Doc =
            { Description = "Retry count with malformed default"
              HowToFind = "Demonstrates Error on bad default"
              ManagementUrl = None } }

    Environment.SetEnvironmentVariable("RETRY_COUNT_DEMO", null)

    match read badIntDef with
    | Error msg -> printfn "  bad default -> Error \"%s\"" msg
    | Ok v -> printfn "  bad default -> unexpectedly Ok %A" v

    Environment.SetEnvironmentVariable("AWS_REGION_DEMO", null)
    printfn ""

// =============================================================================
// Run all demos
// =============================================================================

[<EntryPoint>]
let main _argv =
    printfn "UnionConfig Example App"
    printfn "======================"
    printfn ""

    demoReadFromEnv ()
    demoConfigValueHelpers ()
    demoParsing ()
    demoValidation ()
    demoEnvFile ()
    demoMasking ()
    demoVerification ()
    demoDocs ()
    demoOpenInEditor ()
    demoSsmConfigStore ()
    demoConfigEditor ()
    demoConfigRegistryFlat ()
    demoConfigRegistryGrouped ()
    demoWriteEnvFile ()
    demoReadResult ()
    demoProvenanceRouting ()
    demoDefaultValue ()

    printfn "Done!"
    0
