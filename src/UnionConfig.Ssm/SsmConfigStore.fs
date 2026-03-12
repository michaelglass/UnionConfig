/// Generic config store backed by a parameter store.
/// Maps config var names to parameter paths and provides CRUD operations.
module UnionConfig.Ssm.SsmConfigStore

open System

/// Operations for interacting with a parameter store.
/// Consumers provide an implementation (e.g., backed by AWS SSM).
[<NoComparison; NoEquality>]
type SsmOperations =
    {
        /// Get a single parameter value. Returns None if not found.
        GetParameter: string -> string option
        /// Set a parameter value. isSecure indicates SecureString storage.
        SetParameter: string -> string -> bool -> Result<unit, string>
        /// Delete a parameter. Returns Ok if deleted or absent.
        DeleteParameter: string -> Result<unit, string>
        /// Get all parameters under a path prefix. Returns (path, value) pairs.
        GetParametersByPath: string -> (string * string) list
    }

/// Maps config var names to parameter paths
[<NoComparison; NoEquality>]
type SsmPathMapping =
    {
        /// Build parameter path for a var name
        ToPath: string -> string
        /// Extract var name from a parameter path
        FromPath: string -> string
        /// Path prefix for listing all vars under this mapping
        PathPrefix: string
    }

/// A config store backed by a parameter store
[<NoComparison; NoEquality>]
type SsmConfigStore =
    {
        /// Parameter store operations
        Operations: SsmOperations
        /// How var names map to parameter paths
        PathMapping: SsmPathMapping
        /// Determine if a var should use SecureString storage
        IsSecret: string -> bool
    }

/// Get a single config value
let getValue (store: SsmConfigStore) (name: string) : string option =
    store.Operations.GetParameter(store.PathMapping.ToPath name)

/// Set a single config value (auto-determines SecureString from IsSecret)
let setValue (store: SsmConfigStore) (name: string) (value: string) : bool =
    let isSecure = store.IsSecret name

    match store.Operations.SetParameter(store.PathMapping.ToPath name) value isSecure with
    | Ok() -> true
    | Error _ -> false

/// Delete a config value. Returns true on success, false on error.
let deleteValue (store: SsmConfigStore) (name: string) : bool =
    store.Operations.DeleteParameter(store.PathMapping.ToPath name)
    |> Result.isOk

/// Load all config values for the given var names.
/// Returns a Map with all names as keys (empty string for missing values).
let loadAll (store: SsmConfigStore) (varNames: string array) : Map<string, string> =
    varNames
    |> Array.map (fun name ->
        let value = getValue store name |> Option.defaultValue ""
        (name, value))
    |> Map.ofArray

/// Apply a set of changes.
/// Each change is (key, oldValue, newValue).
/// Deletions occur when newValue is empty and oldValue was not.
/// Returns (key, success, wasDelete) for each change.
let applyChanges (store: SsmConfigStore) (changes: (string * string * string) array) : (string * bool * bool) array =
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
