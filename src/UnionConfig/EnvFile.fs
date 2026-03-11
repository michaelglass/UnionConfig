/// .env file operations: reading, comparing, masking, editing.
module UnionConfig.EnvFile

open System
open System.Diagnostics
open System.IO

/// Secret key indicators for masking sensitive values in output
let secretKeyIndicators =
    [| "PASSWORD"; "SECRET"; "KEY"; "API_KEY"; "SIGNING"; "TOKEN" |]

/// Read a .env-style configuration file into a Map
/// Ignores comments (lines starting with #) and empty lines
let readEnvFile (path: string) : Map<string, string> =
    if not (File.Exists path) then
        Map.empty
    else
        File.ReadAllLines(path)
        |> Array.choose (fun line ->
            let trimmed = line.Trim()

            if
                String.IsNullOrEmpty(trimmed)
                || trimmed.StartsWith("#", StringComparison.Ordinal)
            then
                None
            else
                match trimmed.IndexOf('=') with
                | -1 -> None
                | idx ->
                    let key = trimmed.Substring(0, idx).Trim()
                    let value = trimmed.Substring(idx + 1).Trim()
                    Some(key, value))
        |> Map.ofArray

/// Compare two configs and return array of (key, oldValue, newValue) for changed entries
/// Detects: new values, changed values, and deletions (old value present, new value empty/missing)
let compareConfigs (before: Map<string, string>) (after: Map<string, string>) : (string * string * string) array =
    // Get all keys from both maps
    let allKeys =
        Set.union (before |> Map.keys |> Set.ofSeq) (after |> Map.keys |> Set.ofSeq)

    allKeys
    |> Set.toArray
    |> Array.choose (fun key ->
        let oldValue = Map.tryFind key before |> Option.defaultValue ""
        let newValue = Map.tryFind key after |> Option.defaultValue ""

        if newValue <> oldValue then
            Some(key, oldValue, newValue)
        else
            None)

/// Mask sensitive values for display (shows first 4 chars, rest as asterisks)
let maskValue (key: string) (value: string) : string =
    let isSecret =
        secretKeyIndicators
        |> Array.exists (fun s -> key.ToUpperInvariant().Contains(s))

    if isSecret && value.Length > 0 then
        let visibleChars = min 4 value.Length

        value.Substring(0, visibleChars)
        + String.replicate (value.Length - visibleChars) "*"
    else
        value

/// Open a file in the given editor command
let openInEditor (editor: string) (path: string) =
    let parts = editor.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    let editorExe = parts.[0]

    let editorArgs =
        if parts.Length > 1 then
            String.Join(" ", parts.[1..]) + " "
        else
            ""

    printfn $"Opening %s{path} in %s{editor}..."
    printfn "Edit the values, save, and close the editor to continue."
    printfn ""

    let psi = ProcessStartInfo(editorExe, $"%s{editorArgs}\"%s{path}\"")
    psi.UseShellExecute <- false
    use proc = Process.Start(psi)
    proc.WaitForExit()

/// Display config changes in a user-friendly format
let displayChanges (changes: (string * string * string) array) =
    printfn ""
    printfn "Changes detected:"
    printfn "-----------------"

    for (key, oldValue, newValue) in changes do
        let oldDisplay =
            if String.IsNullOrEmpty(oldValue) then
                "(unset)"
            else
                maskValue key oldValue

        let newDisplay =
            if String.IsNullOrEmpty(newValue) then
                "(unset)"
            else
                maskValue key newValue

        printfn $"  %s{key}: %s{oldDisplay} -> %s{newDisplay}"
