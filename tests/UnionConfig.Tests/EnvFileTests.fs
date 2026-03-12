module UnionConfig.Tests.EnvFileTests

open System
open System.IO
open Xunit
open Swensen.Unquote
open UnionConfig.EnvFile
open UnionConfig.Types

module ReadEnvFileTests =
    [<Fact>]
    let ``readEnvFile returns empty for missing file`` () =
        test <@ readEnvFile "/nonexistent/path.env" = Map.empty @>

    [<Fact>]
    let ``readEnvFile parses key=value pairs`` () =
        let path = Path.GetTempFileName()

        try
            File.WriteAllText(path, "FOO=bar\nBAZ=qux")
            let result = readEnvFile path
            test <@ result = Map.ofList [ "FOO", "bar"; "BAZ", "qux" ] @>
        finally
            File.Delete(path)

    [<Fact>]
    let ``readEnvFile skips comments and blank lines`` () =
        let path = Path.GetTempFileName()

        try
            File.WriteAllText(path, "# comment\n\nFOO=bar\n# another comment\nBAZ=qux")
            let result = readEnvFile path
            test <@ result = Map.ofList [ "FOO", "bar"; "BAZ", "qux" ] @>
        finally
            File.Delete(path)

    [<Fact>]
    let ``readEnvFile handles values with equals signs`` () =
        let path = Path.GetTempFileName()

        try
            File.WriteAllText(path, "URL=https://example.com?a=1&b=2")
            let result = readEnvFile path
            test <@ Map.find "URL" result = "https://example.com?a=1&b=2" @>
        finally
            File.Delete(path)

    [<Fact>]
    let ``readEnvFile skips lines without equals sign`` () =
        let path = Path.GetTempFileName()

        try
            File.WriteAllText(path, "VALID=value\nno-equals-here\nALSO_VALID=ok")
            let result = readEnvFile path
            test <@ result.Count = 2 @>
            test <@ Map.find "VALID" result = "value" @>
            test <@ Map.find "ALSO_VALID" result = "ok" @>
        finally
            File.Delete(path)

    [<Fact>]
    let ``readEnvFile handles empty value after equals`` () =
        let path = Path.GetTempFileName()

        try
            File.WriteAllText(path, "EMPTY_VAL=")
            let result = readEnvFile path
            test <@ Map.find "EMPTY_VAL" result = "" @>
        finally
            File.Delete(path)

    [<Fact>]
    let ``readEnvFile trims keys and values`` () =
        let path = Path.GetTempFileName()

        try
            File.WriteAllText(path, "  KEY  =  value  ")
            let result = readEnvFile path
            test <@ Map.find "KEY" result = "value" @>
        finally
            File.Delete(path)

module CompareConfigsTests =
    [<Fact>]
    let ``compareConfigs detects new values`` () =
        let before = Map.empty
        let after = Map.ofList [ "FOO", "bar" ]
        let changes = compareConfigs before after
        test <@ changes = [| ("FOO", "", "bar") |] @>

    [<Fact>]
    let ``compareConfigs detects changed values`` () =
        let before = Map.ofList [ "FOO", "old" ]
        let after = Map.ofList [ "FOO", "new" ]
        let changes = compareConfigs before after
        test <@ changes = [| ("FOO", "old", "new") |] @>

    [<Fact>]
    let ``compareConfigs detects deletions`` () =
        let before = Map.ofList [ "FOO", "bar" ]
        let after = Map.empty
        let changes = compareConfigs before after
        test <@ changes = [| ("FOO", "bar", "") |] @>

    [<Fact>]
    let ``compareConfigs returns empty for identical maps`` () =
        let config = Map.ofList [ "FOO", "bar" ]
        test <@ compareConfigs config config |> Array.isEmpty @>

    [<Fact>]
    let ``compareConfigs handles multiple changes`` () =
        let before = Map.ofList [ "A", "1"; "B", "2"; "C", "3" ]
        let after = Map.ofList [ "A", "1"; "B", "changed"; "D", "new" ]
        let changes = compareConfigs before after
        // B changed, C deleted, D added (A unchanged)
        test <@ changes.Length = 3 @>

module MaskValueTests =
    [<Fact>]
    let ``maskValue masks secret keys`` () =
        let masked = maskValue "API_KEY" "sk-12345"
        // "sk-12345" = 8 chars, first 4 visible, 4 masked
        test <@ masked = "sk-1****" @>

    [<Fact>]
    let ``maskValue does not mask non-secret keys`` () =
        let masked = maskValue "PORT" "3000"
        test <@ masked = "3000" @>

    [<Fact>]
    let ``maskValue handles short secret values`` () =
        let masked = maskValue "PASSWORD" "ab"
        test <@ masked = "ab" @>

    [<Fact>]
    let ``maskValue handles empty values`` () =
        let masked = maskValue "API_KEY" ""
        test <@ masked = "" @>

    [<Fact>]
    let ``maskValue is case insensitive for key matching`` () =
        let masked = maskValue "my_api_key" "secret123"
        test <@ masked = "secr*****" @>

    [<Fact>]
    let ``maskValue masks PASSWORD keys`` () =
        let masked = maskValue "DB_PASSWORD" "mypassword"
        test <@ masked = "mypa******" @>

    [<Fact>]
    let ``maskValue masks SECRET keys`` () =
        let masked = maskValue "APP_SECRET" "supersecret"
        test <@ masked = "supe*******" @>

    [<Fact>]
    let ``maskValue masks TOKEN keys`` () =
        let masked = maskValue "AUTH_TOKEN" "tok-abc123"
        test <@ masked = "tok-******" @>

    [<Fact>]
    let ``maskValue masks SIGNING keys`` () =
        let masked = maskValue "SIGNING_KEY" "sign-xyz"
        test <@ masked = "sign****" @>

    [<Fact>]
    let ``maskValue exactly 4 char secret shows all chars`` () =
        let masked = maskValue "API_KEY" "abcd"
        // 4 chars, min(4, 4) = 4 visible, 0 masked
        test <@ masked = "abcd" @>

module DisplayChangesTests =
    [<Fact>]
    let ``displayChanges handles changes with secret masking`` () =
        // Exercises masking path for secret keys
        displayChanges [| ("API_KEY", "old-secret-key", "new-secret-key") |]

    [<Fact>]
    let ``displayChanges handles new values (empty old)`` () =
        // Exercises the "(unset)" path for empty oldValue
        displayChanges [| ("PORT", "", "3000") |]

    [<Fact>]
    let ``displayChanges handles deletion (empty new)`` () =
        // Exercises the "(unset)" path for empty newValue
        displayChanges [| ("PORT", "3000", "") |]

    [<Fact>]
    let ``displayChanges handles non-secret keys`` () =
        // Exercises the non-masked path
        displayChanges [| ("DB_HOST", "old-host", "new-host") |]

module SecretKeyIndicatorsTests =
    [<Fact>]
    let ``secretKeyIndicators contains expected patterns`` () =
        test <@ secretKeyIndicators |> Array.contains "PASSWORD" @>
        test <@ secretKeyIndicators |> Array.contains "SECRET" @>
        test <@ secretKeyIndicators |> Array.contains "KEY" @>
        test <@ secretKeyIndicators |> Array.contains "API_KEY" @>
        test <@ secretKeyIndicators |> Array.contains "TOKEN" @>
        test <@ secretKeyIndicators |> Array.contains "SIGNING" @>

module OpenInEditorTests =
    [<Fact>]
    let ``openInEditor runs editor without error`` () =
        // 'true' is a no-op command on macOS/Linux
        let path = Path.GetTempFileName()

        try
            openInEditor "true" path
        finally
            File.Delete(path)

    [<Fact>]
    let ``openInEditor handles editor command with args`` () =
        let path = Path.GetTempFileName()

        try
            openInEditor "true --wait" path
        finally
            File.Delete(path)

module WriteEnvFileTests =
    [<Fact>]
    let ``writeEnvFile writes empty file for no sections`` () =
        let path = Path.GetTempFileName()

        try
            writeEnvFile path [||]
            let content = File.ReadAllText(path)
            test <@ content = "" @>
        finally
            File.Delete(path)

    [<Fact>]
    let ``writeEnvFile writes entries with header`` () =
        let path = Path.GetTempFileName()

        try
            let sections =
                [| { Header = "Database"
                     Entries =
                       [| { Name = "DB_HOST"
                            Value = "localhost"
                            Comment = Some "The database host" }
                          { Name = "DB_PORT"
                            Value = "5432"
                            Comment = None } |] } |]

            writeEnvFile path sections
            let lines = File.ReadAllLines(path)
            test <@ lines.[0] = "# === Database ===" @>
            test <@ lines.[1] = "# The database host" @>
            test <@ lines.[2] = "DB_HOST=localhost" @>
            test <@ lines.[3] = "DB_PORT=5432" @>
            test <@ lines.Length = 4 @>
        finally
            File.Delete(path)

    [<Fact>]
    let ``writeEnvFile writes multiple sections`` () =
        let path = Path.GetTempFileName()

        try
            let sections =
                [| { Header = "Database"
                     Entries =
                       [| { Name = "DB_HOST"
                            Value = "localhost"
                            Comment = None } |] }
                   { Header = "Auth"
                     Entries =
                       [| { Name = "API_KEY"
                            Value = "abc"
                            Comment = None } |] } |]

            writeEnvFile path sections
            let lines = File.ReadAllLines(path)
            test <@ lines.[0] = "# === Database ===" @>
            test <@ lines.[1] = "DB_HOST=localhost" @>
            test <@ lines.[2] = "" @>
            test <@ lines.[3] = "# === Auth ===" @>
            test <@ lines.[4] = "API_KEY=abc" @>
            test <@ lines.Length = 5 @>
        finally
            File.Delete(path)

module DefaultSectionsTests =
    let mkDef name description =
        { Name = name
          Kind = Manual
          ValueType = StringType
          Requirement = Required
          IsSecret = false
          Doc =
            { Description = description
              HowToFind = ""
              ManagementUrl = None } }

    [<Fact>]
    let ``defaultSections creates sections from grouped defs`` () =
        let grouped = [| ("Database", [| mkDef "DB_HOST" "The database host" |]) |]
        let values = Map.ofList [ "DB_HOST", "localhost" ]
        let sections = defaultSections grouped values
        test <@ sections.Length = 1 @>
        test <@ sections.[0].Header = "Database" @>
        test <@ sections.[0].Entries.[0].Name = "DB_HOST" @>
        test <@ sections.[0].Entries.[0].Value = "localhost" @>

    [<Fact>]
    let ``defaultSections renames empty group to Other`` () =
        let grouped = [| ("", [| mkDef "MISC_VAR" "" |]) |]
        let values = Map.empty
        let sections = defaultSections grouped values
        test <@ sections.[0].Header = "Other" @>

    [<Fact>]
    let ``defaultSections uses empty string for missing values`` () =
        let grouped = [| ("App", [| mkDef "MISSING_VAR" "" |]) |]
        let values = Map.empty
        let sections = defaultSections grouped values
        test <@ sections.[0].Entries.[0].Value = "" @>

    [<Fact>]
    let ``defaultSections uses Description as comment`` () =
        let grouped = [| ("App", [| mkDef "PORT" "The application port" |]) |]
        let values = Map.ofList [ "PORT", "3000" ]
        let sections = defaultSections grouped values
        test <@ sections.[0].Entries.[0].Comment = Some "The application port" @>
