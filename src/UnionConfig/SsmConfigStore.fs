/// Generic config store backed by a parameter store.
/// Maps config var names to parameter paths and provides CRUD operations.
module UnionConfig.SsmConfigStore

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

/// Set a single config value (auto-determines SecureString from IsSecret).
/// Returns Ok on success, Error with the store's message on failure.
let setValue (store: SsmConfigStore) (name: string) (value: string) : Result<unit, string> =
    let isSecure = store.IsSecret name
    store.Operations.SetParameter (store.PathMapping.ToPath name) value isSecure

/// Delete a config value. Returns Ok on success (including when absent),
/// Error with the store's message on failure.
let deleteValue (store: SsmConfigStore) (name: string) : Result<unit, string> =
    store.Operations.DeleteParameter(store.PathMapping.ToPath name)

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
/// Returns (key, result, wasDelete) for each change, where result carries the
/// store's error text on failure.
let applyChanges
    (store: SsmConfigStore)
    (changes: (string * string * string) array)
    : (string * Result<unit, string> * bool) array =
    changes
    |> Array.map (fun (key, oldValue, newValue) ->
        let isDelete =
            String.IsNullOrEmpty(newValue) && not (String.IsNullOrEmpty(oldValue))

        let result =
            if isDelete then
                deleteValue store key
            else
                setValue store key newValue

        (key, result, isDelete))
