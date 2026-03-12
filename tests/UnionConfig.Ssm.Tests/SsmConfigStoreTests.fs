module UnionConfig.Ssm.Tests.SsmConfigStoreTests

open System
open Xunit
open Swensen.Unquote
open UnionConfig.Ssm.SsmConfigStore

/// Mutable in-memory parameter store for testing
type InMemoryStore() =
    let mutable store = Map.empty<string, (string * bool)>
    let mutable failOnPut = false
    let mutable failOnDelete = false

    member _.AddParameter(name, value) =
        store <- Map.add name (value, false) store

    member _.GetStore() = store
    member _.SetFailOnPut(fail) = failOnPut <- fail
    member _.SetFailOnDelete(fail) = failOnDelete <- fail

    member _.ToOperations() : SsmOperations =
        { GetParameter =
            fun path ->
                match Map.tryFind path store with
                | Some(value, _) when not (String.IsNullOrWhiteSpace(value)) -> Some value
                | _ -> None
          SetParameter =
            fun path value isSecure ->
                if failOnPut then
                    Error "Simulated PutParameter failure"
                else
                    store <- Map.add path (value, isSecure) store
                    Ok()
          DeleteParameter =
            fun path ->
                if failOnDelete then
                    Error "Simulated DeleteParameter failure"
                else
                    match Map.tryFind path store with
                    | Some _ ->
                        store <- Map.remove path store
                        Ok()
                    | None -> Error "ParameterNotFound: not found"
          GetParametersByPath =
            fun prefix ->
                store
                |> Map.filter (fun k _ -> k.StartsWith(prefix, StringComparison.Ordinal))
                |> Map.toList
                |> List.map (fun (k, (v, _)) -> (k, v)) }

let private testMapping prefix =
    { ToPath = fun name -> $"%s{prefix}/%s{name}"
      FromPath = fun path -> path.Split('/') |> Array.last
      PathPrefix = prefix }

let private createTestStore (mem: InMemoryStore) =
    { Operations = mem.ToOperations()
      PathMapping = testMapping "/myapp/staging"
      IsSecret = fun name -> name.Contains("SECRET") || name.Contains("KEY") }

// ============================================================================
// SsmConfigStore Tests
// ============================================================================

module GetValueTests =
    [<Fact>]
    let ``getValue returns value via path mapping`` () =
        let mem = InMemoryStore()
        mem.AddParameter("/myapp/staging/DB_HOST", "localhost")
        let store = createTestStore mem
        test <@ getValue store "DB_HOST" = Some "localhost" @>

    [<Fact>]
    let ``getValue returns None for missing parameter`` () =
        let mem = InMemoryStore()
        let store = createTestStore mem
        test <@ getValue store "MISSING" = None @>

module SetValueTests =
    [<Fact>]
    let ``setValue sets parameter via path mapping`` () =
        let mem = InMemoryStore()
        let store = createTestStore mem
        let result = setValue store "DB_HOST" "localhost"
        test <@ result = true @>
        test <@ getValue store "DB_HOST" = Some "localhost" @>

    [<Fact>]
    let ``setValue uses SecureString for secret vars`` () =
        let mem = InMemoryStore()
        let store = createTestStore mem
        setValue store "API_SECRET" "s3cret" |> ignore

        let internalStore = mem.GetStore()

        test
            <@
                match Map.tryFind "/myapp/staging/API_SECRET" internalStore with
                | Some(_, isSecure) -> isSecure = true
                | None -> false
            @>

    [<Fact>]
    let ``setValue uses String for non-secret vars`` () =
        let mem = InMemoryStore()
        let store = createTestStore mem
        setValue store "DB_HOST" "localhost" |> ignore

        let internalStore = mem.GetStore()

        test
            <@
                match Map.tryFind "/myapp/staging/DB_HOST" internalStore with
                | Some(_, isSecure) -> isSecure = false
                | None -> false
            @>

    [<Fact>]
    let ``setValue returns false when operation fails`` () =
        let mem = InMemoryStore()
        mem.SetFailOnPut(true)
        let store = createTestStore mem
        test <@ setValue store "DB_HOST" "localhost" = false @>

module DeleteValueTests =
    [<Fact>]
    let ``deleteValue removes parameter`` () =
        let mem = InMemoryStore()
        mem.AddParameter("/myapp/staging/DB_HOST", "localhost")
        let store = createTestStore mem
        test <@ deleteValue store "DB_HOST" = true @>
        test <@ getValue store "DB_HOST" = None @>

    [<Fact>]
    let ``deleteValue returns false for nonexistent parameter`` () =
        let mem = InMemoryStore()
        let store = createTestStore mem
        test <@ deleteValue store "NONEXISTENT" = false @>

    [<Fact>]
    let ``deleteValue returns false when operation fails`` () =
        let mem = InMemoryStore()
        mem.AddParameter("/myapp/staging/VAR", "value")
        mem.SetFailOnDelete(true)
        let store = createTestStore mem
        test <@ deleteValue store "VAR" = false @>

module LoadAllTests =
    [<Fact>]
    let ``loadAll returns values for all requested var names`` () =
        let mem = InMemoryStore()
        mem.AddParameter("/myapp/staging/DB_HOST", "localhost")
        mem.AddParameter("/myapp/staging/DB_PORT", "5432")
        let store = createTestStore mem
        let result = loadAll store [| "DB_HOST"; "DB_PORT"; "MISSING" |]
        test <@ Map.find "DB_HOST" result = "localhost" @>
        test <@ Map.find "DB_PORT" result = "5432" @>
        test <@ Map.find "MISSING" result = "" @>

    [<Fact>]
    let ``loadAll returns empty strings for all missing vars`` () =
        let mem = InMemoryStore()
        let store = createTestStore mem
        let result = loadAll store [| "A"; "B" |]
        test <@ Map.find "A" result = "" @>
        test <@ Map.find "B" result = "" @>

module ApplyChangesTests =
    [<Fact>]
    let ``applyChanges sets new values`` () =
        let mem = InMemoryStore()
        let store = createTestStore mem
        let changes = [| ("DB_HOST", "", "localhost") |]
        let results = applyChanges store changes
        test <@ results.[0] = ("DB_HOST", true, false) @>
        test <@ getValue store "DB_HOST" = Some "localhost" @>

    [<Fact>]
    let ``applyChanges deletes when newValue is empty and oldValue is not`` () =
        let mem = InMemoryStore()
        mem.AddParameter("/myapp/staging/DB_HOST", "localhost")
        let store = createTestStore mem
        let changes = [| ("DB_HOST", "localhost", "") |]
        let results = applyChanges store changes
        test <@ results.[0] = ("DB_HOST", true, true) @>

    [<Fact>]
    let ``applyChanges does not delete when both values are empty`` () =
        let mem = InMemoryStore()
        let store = createTestStore mem
        let changes = [| ("DB_HOST", "", "") |]
        let results = applyChanges store changes
        let (_, _, isDelete) = results.[0]
        test <@ isDelete = false @>

    [<Fact>]
    let ``applyChanges handles multiple changes`` () =
        let mem = InMemoryStore()
        mem.AddParameter("/myapp/staging/OLD_VAR", "old")
        let store = createTestStore mem

        let changes =
            [| ("NEW_VAR", "", "new-value")
               ("OLD_VAR", "old", "")
               ("UPDATED", "before", "after") |]

        let results = applyChanges store changes
        test <@ results.Length = 3 @>
        test <@ let (_, _, d) = results.[0] in d = false @>
        test <@ let (_, _, d) = results.[1] in d = true @>
        test <@ let (_, _, d) = results.[2] in d = false @>

module PathMappingTests =
    [<Fact>]
    let ``ToPath builds correct path`` () =
        let mapping = testMapping "/myapp/staging"
        test <@ mapping.ToPath "DBHOST" = "/myapp/staging/DBHOST" @>

    [<Fact>]
    let ``FromPath extracts var name from path`` () =
        let mapping = testMapping "/myapp/staging"
        test <@ mapping.FromPath "/myapp/staging/DBHOST" = "DBHOST" @>

    [<Fact>]
    let ``ToPath and FromPath are inverse`` () =
        let mapping = testMapping "/myapp/prod"
        let name = "API_KEY"
        test <@ mapping.FromPath(mapping.ToPath name) = name @>

module IsSecretTests =
    [<Fact>]
    let ``IsSecret identifies secret vars`` () =
        let mem = InMemoryStore()
        let store = createTestStore mem
        test <@ store.IsSecret "MY_SECRET" = true @>
        test <@ store.IsSecret "API_KEY" = true @>
        test <@ store.IsSecret "DB_HOST" = false @>
