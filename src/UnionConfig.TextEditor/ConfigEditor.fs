namespace UnionConfig.TextEditor

open System
open System.IO
open UnionConfig.Verification
open UnionConfig.EnvFile

/// Shared config editing operations parameterized by backend
module ConfigEditor =
    let private logInfo (msg: string) = printfn "  %s" msg
    let private logPass (msg: string) = printfn "  ✓ %s" msg
    let private logFail (msg: string) = printfn "  ✗ %s" msg
    let private section (title: string) = printfn "── %s ──" title

    /// Result of populating defaults
    type PopulateResult =
        | NoChangesNeeded
        | Applied of count: int
        | Failed of errors: string list

    /// Populate defaults directly (no editor)
    /// Parameters:
    /// - getValue: function to get current value for a name
    /// - setValue: function to set a value, returns true on success
    /// - getDefaults: function that returns (name, defaultValue) pairs for vars that need defaults
    /// - writeLocalFile: optional function to write updated config to local file
    let populateDefaults
        (getValue: string -> string option)
        (setValue: string -> string -> bool)
        (getDefaults: Map<string, string> -> (string * string) array)
        (writeLocalFile: Map<string, string> -> unit)
        : PopulateResult =

        // Load current config
        let currentConfig =
            getDefaults Map.empty
            |> Array.map fst
            |> Array.distinct
            |> Array.append (getDefaults Map.empty |> Array.map fst)
            |> Array.distinct
            |> Array.map (fun name ->
                let value = getValue name |> Option.defaultValue ""
                (name, value))
            |> Map.ofArray

        // Find which defaults need to be applied
        let defaultsToApply = getDefaults currentConfig

        if Array.isEmpty defaultsToApply then
            logInfo "All defaults already set"
            NoChangesNeeded
        else
            section "Applying defaults"

            let results =
                defaultsToApply
                |> Array.map (fun (name, defaultVal) ->
                    logInfo $"Setting %s{name} = %s{defaultVal}"
                    let success = setValue name defaultVal
                    (name, defaultVal, success))

            printfn ""

            let errors =
                results
                |> Array.choose (fun (name, _, success) ->
                    if success then
                        logPass name
                        None
                    else
                        logFail $"%s{name} (failed)"
                        Some name)
                |> Array.toList

            // Update local file with applied defaults
            let updatedConfig =
                defaultsToApply
                |> Array.fold (fun cfg (name, value) -> Map.add name value cfg) currentConfig

            writeLocalFile updatedConfig

            if List.isEmpty errors then
                Applied(Array.length defaultsToApply)
            else
                Failed errors

    /// Internal: Edit config with injectable I/O operations for testability
    let internal editConfigWith
        (runEditor: string -> unit)
        (getConfirmation: unit -> string)
        (loadConfig: unit -> Map<string, string>)
        (setValue: string -> string -> bool)
        (writeConfigFile: string -> Map<string, string> -> unit)
        (verifyChanges: (string * string * string) array -> Map<string, string> -> (string * VerificationResult) array)
        : unit =

        let config = loadConfig ()
        let tempFile = Path.GetTempFileName() + ".env"

        try
            writeConfigFile tempFile config
            printfn $"Wrote config to: %s{tempFile}"
            printfn ""

            runEditor tempFile

            let afterConfig = readEnvFile tempFile
            let changes = compareConfigs config afterConfig

            if Array.isEmpty changes then
                printfn ""
                printfn "No changes detected."
            else
                displayChanges changes

                printfn ""
                section "Verifying API keys"

                let updatedConfig =
                    changes
                    |> Array.fold (fun acc (key, _, newValue) -> Map.add key newValue acc) config

                let verificationResults = verifyChanges changes updatedConfig
                displayVerificationResults verificationResults

                let failures =
                    verificationResults
                    |> Array.filter (fun (_, r) ->
                        match r with
                        | VerifyFailed _ -> true
                        | VerifySuccess _
                        | VerifySkipped _ -> false)

                if not (Array.isEmpty failures) then
                    printfn ""
                    printfn "Verification failed. Please fix the errors above and try again."
                    printfn "Changes not applied."
                else
                    printfn ""
                    printf "Apply these changes? [y/N] "
                    let response = getConfirmation ()

                    if response.ToLowerInvariant() = "y" then
                        printfn ""

                        let results =
                            changes
                            |> Array.map (fun (key, _, newValue) ->
                                let success = setValue key newValue
                                (key, success))

                        printfn ""

                        for (key, success) in results do
                            if success then logPass key else logFail $"%s{key} (failed)"

                        printfn ""
                        printfn "Done!"
                    else
                        printfn "Changes discarded."
        finally
            File.Delete(tempFile)

    /// Edit config via editor workflow
    /// Parameters:
    /// - loadConfig: function to load all config
    /// - setValue: function to set a single value
    /// - writeConfigFile: function to write config to a file (path -> config -> unit)
    /// - verifyChanges: function to verify changed values (returns verification results)
    let editConfig
        (loadConfig: unit -> Map<string, string>)
        (setValue: string -> string -> bool)
        (writeConfigFile: string -> Map<string, string> -> unit)
        (verifyChanges: (string * string * string) array -> Map<string, string> -> (string * VerificationResult) array)
        : unit =

        let runEditor tempFile =
            let editorEnv =
                // fsharplint:disable-next-line Hints
                Environment.GetEnvironmentVariable("EDITOR")
                |> Option.ofObj
                |> Option.defaultValue "vi"

            openInEditor editorEnv tempFile

        let getConfirmation () = Console.ReadLine()

        editConfigWith runEditor getConfirmation loadConfig setValue writeConfigFile verifyChanges
