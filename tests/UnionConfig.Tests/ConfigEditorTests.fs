module UnionConfig.Tests.ConfigEditorTests

open System.IO
open Xunit
open Swensen.Unquote
open UnionConfig.ConfigEditor
open UnionConfig.Verification

module PopulateDefaultsTests =
    [<Fact>]
    let ``returns NoChangesNeeded when all defaults already set`` () =
        let getValue name =
            match name with
            | "DB_HOST" -> Some "localhost"
            | _ -> None

        let setValue _ _ = true

        let getDefaults (_config: Map<string, string>) =
            // No defaults need to be applied
            [||]

        let mutable writeCalled = false
        let writeLocalFile _ = writeCalled <- true

        let result = populateDefaults getValue setValue getDefaults writeLocalFile
        test <@ result = NoChangesNeeded @>

    [<Fact>]
    let ``returns Applied when defaults are set successfully`` () =
        let mutable store = Map.empty<string, string>

        let getValue name = Map.tryFind name store

        let setValue name value =
            store <- Map.add name value store
            true

        let getDefaults (config: Map<string, string>) =
            [| ("DB_HOST", "localhost"); ("DB_PORT", "5432") |]
            |> Array.filter (fun (name, _) ->
                match Map.tryFind name config with
                | Some v when v <> "" -> false
                | _ -> true)

        let mutable writtenConfig = Map.empty
        let writeLocalFile config = writtenConfig <- config

        let result = populateDefaults getValue setValue getDefaults writeLocalFile

        match result with
        | Applied count -> test <@ count = 2 @>
        | other -> failwithf "Expected Applied, got %A" other

        test <@ Map.find "DB_HOST" writtenConfig = "localhost" @>
        test <@ Map.find "DB_PORT" writtenConfig = "5432" @>

    [<Fact>]
    let ``returns Failed when some setValue calls fail`` () =
        let getValue _ = None

        let setValue name _ =
            // Fail for one var
            name <> "FAIL_VAR"

        let getDefaults _ =
            [| ("OK_VAR", "val1"); ("FAIL_VAR", "val2") |]

        let writeLocalFile _ = ()

        let result = populateDefaults getValue setValue getDefaults writeLocalFile

        match result with
        | Failed errors ->
            test <@ errors.Length = 1 @>
            test <@ errors |> List.contains "FAIL_VAR" @>
        | other -> failwithf "Expected Failed, got %A" other

module EditConfigWithTests =
    [<Fact>]
    let ``no changes detected when editor does not modify file`` () =
        let sw = new StringWriter()
        let original = System.Console.Out
        System.Console.SetOut(sw)

        try
            let config = Map.ofList [ "DB_HOST", "localhost" ]
            let loadConfig () = config

            let writeConfigFile (path: string) (cfg: Map<string, string>) =
                let lines = cfg |> Map.toArray |> Array.map (fun (k, v) -> $"%s{k}=%s{v}")
                File.WriteAllLines(path, lines)

            // Editor does nothing - file stays the same
            let runEditor _ = ()
            let getConfirmation () = "n"
            let setValue _ _ = true
            let verifyChanges _ _ = [||]

            editConfigWith runEditor getConfirmation loadConfig setValue writeConfigFile verifyChanges

            let output = sw.ToString()
            test <@ output.Contains("No changes detected.") @>
        finally
            System.Console.SetOut(original)

    [<Fact>]
    let ``applies changes when user confirms`` () =
        let sw = new StringWriter()
        let original = System.Console.Out
        System.Console.SetOut(sw)

        try
            let config = Map.ofList [ "DB_HOST", "localhost" ]
            let mutable applied = Map.empty<string, string>
            let loadConfig () = config

            let writeConfigFile (path: string) (cfg: Map<string, string>) =
                let lines = cfg |> Map.toArray |> Array.map (fun (k, v) -> $"%s{k}=%s{v}")
                File.WriteAllLines(path, lines)

            // Editor modifies the file
            let runEditor (path: string) =
                File.WriteAllText(path, "DB_HOST=new-host\nDB_PORT=5432")

            let getConfirmation () = "y"

            let setValue name value =
                applied <- Map.add name value applied
                true

            let verifyChanges _ _ = [| ("DB_HOST", VerifySuccess "ok") |]

            editConfigWith runEditor getConfirmation loadConfig setValue writeConfigFile verifyChanges

            let output = sw.ToString()
            test <@ output.Contains("Done!") @>
            test <@ Map.find "DB_HOST" applied = "new-host" @>
            test <@ Map.find "DB_PORT" applied = "5432" @>
        finally
            System.Console.SetOut(original)

    [<Fact>]
    let ``discards changes when user declines`` () =
        let sw = new StringWriter()
        let original = System.Console.Out
        System.Console.SetOut(sw)

        try
            let config = Map.ofList [ "DB_HOST", "localhost" ]
            let mutable applied = false
            let loadConfig () = config

            let writeConfigFile (path: string) (cfg: Map<string, string>) =
                let lines = cfg |> Map.toArray |> Array.map (fun (k, v) -> $"%s{k}=%s{v}")
                File.WriteAllLines(path, lines)

            let runEditor (path: string) =
                File.WriteAllText(path, "DB_HOST=new-host")

            let getConfirmation () = "n"

            let setValue _ _ =
                applied <- true
                true

            let verifyChanges _ _ = [||]

            editConfigWith runEditor getConfirmation loadConfig setValue writeConfigFile verifyChanges

            let output = sw.ToString()
            test <@ output.Contains("Changes discarded.") @>
            test <@ applied = false @>
        finally
            System.Console.SetOut(original)

    [<Fact>]
    let ``does not apply changes when verification fails`` () =
        let sw = new StringWriter()
        let original = System.Console.Out
        System.Console.SetOut(sw)

        try
            let config = Map.ofList [ "DB_HOST", "localhost" ]
            let mutable applied = false
            let loadConfig () = config

            let writeConfigFile (path: string) (cfg: Map<string, string>) =
                let lines = cfg |> Map.toArray |> Array.map (fun (k, v) -> $"%s{k}=%s{v}")
                File.WriteAllLines(path, lines)

            let runEditor (path: string) =
                File.WriteAllText(path, "DB_HOST=bad-value")

            let getConfirmation () = "y"

            let setValue _ _ =
                applied <- true
                true

            let verifyChanges _ _ =
                [| ("DB_HOST", VerifyFailed "invalid host") |]

            editConfigWith runEditor getConfirmation loadConfig setValue writeConfigFile verifyChanges

            let output = sw.ToString()
            test <@ output.Contains("Verification failed") @>
            test <@ output.Contains("Changes not applied") @>
            test <@ applied = false @>
        finally
            System.Console.SetOut(original)

    [<Fact>]
    let ``handles setValue failures gracefully`` () =
        let sw = new StringWriter()
        let original = System.Console.Out
        System.Console.SetOut(sw)

        try
            let config = Map.ofList [ "DB_HOST", "localhost" ]
            let loadConfig () = config

            let writeConfigFile (path: string) (cfg: Map<string, string>) =
                let lines = cfg |> Map.toArray |> Array.map (fun (k, v) -> $"%s{k}=%s{v}")
                File.WriteAllLines(path, lines)

            let runEditor (path: string) =
                File.WriteAllText(path, "DB_HOST=new-host")

            let getConfirmation () = "y"
            let setValue _ _ = false // All set operations fail
            let verifyChanges _ _ = [||]

            editConfigWith runEditor getConfirmation loadConfig setValue writeConfigFile verifyChanges

            let output = sw.ToString()
            // Should still complete and show "Done!" even with failures
            test <@ output.Contains("Done!") @>
        finally
            System.Console.SetOut(original)

    [<Fact>]
    let ``applies changes with mixed verification results (success and skipped)`` () =
        let sw = new StringWriter()
        let original = System.Console.Out
        System.Console.SetOut(sw)

        try
            let config = Map.ofList [ "DB_HOST", "localhost" ]
            let mutable applied = Map.empty<string, string>
            let loadConfig () = config

            let writeConfigFile (path: string) (cfg: Map<string, string>) =
                let lines = cfg |> Map.toArray |> Array.map (fun (k, v) -> $"%s{k}=%s{v}")
                File.WriteAllLines(path, lines)

            let runEditor (path: string) =
                File.WriteAllText(path, "DB_HOST=new-host\nAPI_KEY=secret")

            let getConfirmation () = "y"

            let setValue name value =
                applied <- Map.add name value applied
                true

            let verifyChanges _ _ =
                [| ("DB_HOST", VerifySuccess "ok")
                   ("API_KEY", VerifySkipped "managed by infra") |]

            editConfigWith runEditor getConfirmation loadConfig setValue writeConfigFile verifyChanges

            let output = sw.ToString()
            test <@ output.Contains("Done!") @>
            test <@ Map.find "DB_HOST" applied = "new-host" @>
        finally
            System.Console.SetOut(original)

module EditConfigPublicApiTests =
    [<Fact>]
    let ``editConfig no changes path`` () =
        // No-op editor -> no changes detected
        let originalIn = System.Console.In
        System.Console.SetIn(new StringReader("n"))

        try
            System.Environment.SetEnvironmentVariable("EDITOR", "true")

            let config = Map.ofList [ "DB_HOST", "localhost" ]
            let loadConfig () = config

            let writeConfigFile (path: string) (cfg: Map<string, string>) =
                let lines = cfg |> Map.toArray |> Array.map (fun (k, v) -> $"%s{k}=%s{v}")
                File.WriteAllLines(path, lines)

            let setValue _ _ = true
            let verifyChanges _ _ = [||]

            editConfig loadConfig setValue writeConfigFile verifyChanges
        finally
            System.Environment.SetEnvironmentVariable("EDITOR", null)
            System.Console.SetIn(originalIn)

    [<Fact>]
    let ``editConfig with changes uses Console.ReadLine for confirmation`` () =
        // Create a temp script that acts as the editor and appends a new key
        let scriptPath = Path.GetTempFileName()
        File.WriteAllText(scriptPath, "#!/bin/sh\necho 'NEW_VAR=new_val' >> \"$1\"\n")

        let chmod =
            System.Diagnostics.Process.Start(
                System.Diagnostics.ProcessStartInfo("chmod", $"+x \"{scriptPath}\"", UseShellExecute = false)
            )

        chmod.WaitForExit()

        let originalIn = System.Console.In
        System.Console.SetIn(new StringReader("n"))

        try
            System.Environment.SetEnvironmentVariable("EDITOR", scriptPath)

            let config = Map.ofList [ "DB_HOST", "localhost" ]
            let loadConfig () = config

            let writeConfigFile (path: string) (cfg: Map<string, string>) =
                let lines = cfg |> Map.toArray |> Array.map (fun (k, v) -> $"%s{k}=%s{v}")
                File.WriteAllLines(path, lines)

            let setValue _ _ = true
            let verifyChanges _ _ = [||]

            editConfig loadConfig setValue writeConfigFile verifyChanges
        finally
            System.Environment.SetEnvironmentVariable("EDITOR", null)
            System.Console.SetIn(originalIn)

            try
                File.Delete(scriptPath)
            with _ ->
                ()
