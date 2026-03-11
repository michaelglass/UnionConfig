/// Generic config store backed by AWS SSM Parameter Store.
/// Maps config var names to SSM paths and provides CRUD operations.
module UnionConfig.Ssm.SsmConfigStore

open System
open UnionConfig.Ssm.SsmClient

/// Maps config var names to SSM parameter paths
[<NoComparison; NoEquality>]
type SsmPathMapping =
    { /// Build SSM path for a var name
      ToPath: string -> string
      /// Extract var name from an SSM path
      FromPath: string -> string
      /// Path prefix for listing all vars under this mapping
      PathPrefix: string }

/// A config store backed by AWS SSM
[<NoComparison; NoEquality>]
type SsmConfigStore =
    { /// SSM client configuration (region, auth)
      Config: SsmClientConfig
      /// How var names map to SSM paths
      PathMapping: SsmPathMapping
      /// Determine if a var should use SecureString storage
      IsSecret: string -> bool }

/// Get a single config value from SSM
let getValue (store: SsmConfigStore) (name: string) : string option =
    getParameter store.Config (store.PathMapping.ToPath name)

/// Set a single config value in SSM (auto-determines SecureString from IsSecret)
let setValue (store: SsmConfigStore) (name: string) (value: string) : bool =
    let isSecure = store.IsSecret name

    match setParameter store.Config (store.PathMapping.ToPath name) value isSecure with
    | Ok() -> true
    | Error _ -> false

/// Delete a config value from SSM. Returns true if deleted or already absent.
let deleteValue (store: SsmConfigStore) (name: string) : bool =
    match deleteParameter store.Config (store.PathMapping.ToPath name) with
    | Ok() -> true
    | Error msg ->
        if msg.Contains("ParameterNotFound") then
            true
        else
            false

/// Load all config values from SSM for the given var names.
/// Returns a Map with all names as keys (empty string for missing values).
let loadAll (store: SsmConfigStore) (varNames: string array) : Map<string, string> =
    varNames
    |> Array.map (fun name ->
        let value = getValue store name |> Option.defaultValue ""
        (name, value))
    |> Map.ofArray

/// Apply a set of changes to SSM.
/// Each change is (key, oldValue, newValue).
/// Deletions occur when newValue is empty and oldValue was not.
/// Returns (key, success, wasDelete) for each change.
let applyChanges
    (store: SsmConfigStore)
    (changes: (string * string * string) array)
    : (string * bool * bool) array =
    changes
    |> Array.map (fun (key, oldValue, newValue) ->
        let isDelete =
            String.IsNullOrEmpty(newValue) && not (String.IsNullOrEmpty(oldValue))

        let success =
            if isDelete then
                deleteValue store key
            else
                setValue store key newValue

        (key, success, isDelete))
