module UnionConfig.Tests.EnvFileTests

open System
open System.IO
open Xunit
open Swensen.Unquote
open UnionConfig.EnvFile

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
        test <@ compareConfigs config config = [||] @>

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
