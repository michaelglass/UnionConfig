module UnionConfig.Tests.ReaderTests

open System
open Xunit
open Swensen.Unquote
open UnionConfig.Types
open UnionConfig.Reader

let private makeDef name requirement valueType =
    { Name = name
      Kind = Manual
      ValueType = valueType
      Requirement = requirement
      IsSecret = false
      Group = None
      Doc =
        { Description = "test"
          HowToFind = "test"
          ManagementUrl = None } }

module ReadTests =
    [<Fact>]
    let ``read returns Ok Some for present required var`` () =
        let def = makeDef "TEST_READ_REQ" Required StringType
        Environment.SetEnvironmentVariable("TEST_READ_REQ", "hello")

        try
            match read def with
            | Ok(Some(StringValue s)) -> test <@ s = "hello" @>
            | other -> failwithf "Expected Ok(Some(StringValue)), got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_REQ", null)

    [<Fact>]
    let ``read returns Error for missing required var`` () =
        let def = makeDef "TEST_READ_MISSING" Required StringType
        Environment.SetEnvironmentVariable("TEST_READ_MISSING", null)
        test <@ Result.isError (read def) @>

    [<Fact>]
    let ``read returns Error for empty required var`` () =
        let def = makeDef "TEST_READ_EMPTY" Required StringType
        Environment.SetEnvironmentVariable("TEST_READ_EMPTY", "  ")

        try
            test <@ Result.isError (read def) @>
        finally
            Environment.SetEnvironmentVariable("TEST_READ_EMPTY", null)

    [<Fact>]
    let ``read returns Ok None for missing optional var`` () =
        let def = makeDef "TEST_READ_OPT" Optional StringType
        Environment.SetEnvironmentVariable("TEST_READ_OPT", null)

        match read def with
        | Ok None -> ()
        | other -> failwithf "Expected Ok None, got %A" other

    [<Fact>]
    let ``read optional var returns Ok None when empty`` () =
        let def = makeDef "TEST_READ_OPT_EMPTY" Optional StringType
        Environment.SetEnvironmentVariable("TEST_READ_OPT_EMPTY", "")

        try
            match read def with
            | Ok None -> ()
            | other -> failwithf "Expected Ok None, got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_OPT_EMPTY", null)

    [<Fact>]
    let ``read parses int value`` () =
        let def = makeDef "TEST_READ_INT" Required IntType
        Environment.SetEnvironmentVariable("TEST_READ_INT", "42")

        try
            match read def with
            | Ok(Some(IntValue i)) -> test <@ i = 42 @>
            | other -> failwithf "Expected Ok(Some(IntValue)), got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_INT", null)

    [<Fact>]
    let ``read returns Error for invalid int`` () =
        let def = makeDef "TEST_READ_BAD_INT" Required IntType
        Environment.SetEnvironmentVariable("TEST_READ_BAD_INT", "abc")

        try
            test <@ Result.isError (read def) @>
        finally
            Environment.SetEnvironmentVariable("TEST_READ_BAD_INT", null)

    [<Fact>]
    let ``read parses bool value`` () =
        let def = makeDef "TEST_READ_BOOL" Required BoolType
        Environment.SetEnvironmentVariable("TEST_READ_BOOL", "true")

        try
            match read def with
            | Ok(Some(BoolValue b)) -> test <@ b = true @>
            | other -> failwithf "Expected Ok(Some(BoolValue)), got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_BOOL", null)

    [<Fact>]
    let ``read parses float value`` () =
        let def = makeDef "TEST_READ_FLOAT" Required FloatType
        Environment.SetEnvironmentVariable("TEST_READ_FLOAT", "3.14")

        try
            match read def with
            | Ok(Some(FloatValue f)) -> test <@ f = 3.14 @>
            | other -> failwithf "Expected Ok(Some(FloatValue)), got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_FLOAT", null)

    [<Fact>]
    let ``read parses custom type value`` () =
        let validator (s: string) =
            if s.StartsWith("sk-") then
                None
            else
                Some "must start with sk-"

        let def = makeDef "TEST_READ_CUSTOM" Required (CustomType("ApiKey", validator))
        Environment.SetEnvironmentVariable("TEST_READ_CUSTOM", "sk-abc123")

        try
            match read def with
            | Ok(Some(StringValue s)) -> test <@ s = "sk-abc123" @>
            | other -> failwithf "Expected Ok(Some(StringValue)), got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_CUSTOM", null)

    [<Fact>]
    let ``read returns Error for invalid custom type`` () =
        let validator (s: string) =
            if s.StartsWith("sk-") then
                None
            else
                Some "must start with sk-"

        let def = makeDef "TEST_READ_BAD_CUSTOM" Required (CustomType("ApiKey", validator))
        Environment.SetEnvironmentVariable("TEST_READ_BAD_CUSTOM", "bad-key")

        try
            test <@ Result.isError (read def) @>
        finally
            Environment.SetEnvironmentVariable("TEST_READ_BAD_CUSTOM", null)

    [<Fact>]
    let ``read returns Error for invalid value type on optional`` () =
        let def = makeDef "TEST_READ_OPT_BAD" Optional IntType
        Environment.SetEnvironmentVariable("TEST_READ_OPT_BAD", "not-a-number")

        try
            test <@ Result.isError (read def) @>
        finally
            Environment.SetEnvironmentVariable("TEST_READ_OPT_BAD", null)

    [<Fact>]
    let ``read optional var returns Ok Some when set`` () =
        let def = makeDef "TEST_READ_OPT_SET" Optional StringType
        Environment.SetEnvironmentVariable("TEST_READ_OPT_SET", "present")

        try
            match read def with
            | Ok(Some(StringValue s)) -> test <@ s = "present" @>
            | other -> failwithf "Expected Ok(Some(StringValue)), got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_OPT_SET", null)

    [<Fact>]
    let ``read optional var returns Ok None when whitespace only`` () =
        let def = makeDef "TEST_READ_OPT_WS" Optional StringType
        Environment.SetEnvironmentVariable("TEST_READ_OPT_WS", "   ")

        try
            match read def with
            | Ok None -> ()
            | other -> failwithf "Expected Ok None, got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_OPT_WS", null)

    [<Fact>]
    let ``read optional int returns parsed value`` () =
        let def = makeDef "TEST_READ_OPT_INT" Optional IntType
        Environment.SetEnvironmentVariable("TEST_READ_OPT_INT", "99")

        try
            match read def with
            | Ok(Some(IntValue i)) -> test <@ i = 99 @>
            | other -> failwithf "Expected Ok(Some(IntValue)), got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_OPT_INT", null)

    [<Fact>]
    let ``read error message includes var name for missing required`` () =
        let def = makeDef "TEST_ERR_MSG" Required StringType
        Environment.SetEnvironmentVariable("TEST_ERR_MSG", null)

        match read def with
        | Error msg -> test <@ msg.Contains("TEST_ERR_MSG") @>
        | Ok _ -> failwith "Expected Error"

    [<Fact>]
    let ``read error message includes var name for empty required`` () =
        let def = makeDef "TEST_ERR_MSG_EMPTY" Required StringType
        Environment.SetEnvironmentVariable("TEST_ERR_MSG_EMPTY", "  ")

        try
            match read def with
            | Error msg ->
                test <@ msg.Contains("TEST_ERR_MSG_EMPTY") @>
                test <@ msg.Contains("empty") @>
            | Ok _ -> failwith "Expected Error"
        finally
            Environment.SetEnvironmentVariable("TEST_ERR_MSG_EMPTY", null)

module ConvenienceFunctionTests =
    [<Fact>]
    let ``readString returns value when set`` () =
        let def = makeDef "TEST_RSTR" Optional StringType
        Environment.SetEnvironmentVariable("TEST_RSTR", "hello")

        try
            test <@ readString def = "hello" @>
        finally
            Environment.SetEnvironmentVariable("TEST_RSTR", null)

    [<Fact>]
    let ``readString returns empty string when not set`` () =
        let def = makeDef "TEST_RSTR_MISS" Optional StringType
        Environment.SetEnvironmentVariable("TEST_RSTR_MISS", null)
        test <@ readString def = "" @>

    [<Fact>]
    let ``readInt returns parsed int`` () =
        let def = makeDef "TEST_RINT" Required IntType
        Environment.SetEnvironmentVariable("TEST_RINT", "42")

        try
            test <@ readInt def = 42 @>
        finally
            Environment.SetEnvironmentVariable("TEST_RINT", null)

    [<Fact>]
    let ``readInt throws for missing required`` () =
        let def = makeDef "TEST_RINT_MISS" Required IntType
        Environment.SetEnvironmentVariable("TEST_RINT_MISS", null)
        raises<exn> <@ readInt def @>

    [<Fact>]
    let ``readBool returns parsed bool`` () =
        let def = makeDef "TEST_RBOOL" Required BoolType
        Environment.SetEnvironmentVariable("TEST_RBOOL", "true")

        try
            test <@ readBool def = true @>
        finally
            Environment.SetEnvironmentVariable("TEST_RBOOL", null)

    [<Fact>]
    let ``readIntOrDefault returns value when set`` () =
        let def = makeDef "TEST_RIOD" Optional IntType
        Environment.SetEnvironmentVariable("TEST_RIOD", "99")

        try
            test <@ readIntOrDefault def 0 = 99 @>
        finally
            Environment.SetEnvironmentVariable("TEST_RIOD", null)

    [<Fact>]
    let ``readIntOrDefault returns default when not set`` () =
        let def = makeDef "TEST_RIOD_MISS" Optional IntType
        Environment.SetEnvironmentVariable("TEST_RIOD_MISS", null)
        test <@ readIntOrDefault def 42 = 42 @>

    [<Fact>]
    let ``readBoolOrDefault returns value when set`` () =
        let def = makeDef "TEST_RBOD" Optional BoolType
        Environment.SetEnvironmentVariable("TEST_RBOD", "false")

        try
            test <@ readBoolOrDefault def true = false @>
        finally
            Environment.SetEnvironmentVariable("TEST_RBOD", null)

    [<Fact>]
    let ``readBoolOrDefault returns default when not set`` () =
        let def = makeDef "TEST_RBOD_MISS" Optional BoolType
        Environment.SetEnvironmentVariable("TEST_RBOD_MISS", null)
        test <@ readBoolOrDefault def true = true @>

module ValidateRequiredTests =
    [<Fact>]
    let ``validateRequired returns empty for all set`` () =
        let def = makeDef "TEST_VALID_SET" Required StringType
        Environment.SetEnvironmentVariable("TEST_VALID_SET", "value")

        try
            test <@ validateRequired [ def ] |> List.isEmpty @>
        finally
            Environment.SetEnvironmentVariable("TEST_VALID_SET", null)

    [<Fact>]
    let ``validateRequired reports missing vars`` () =
        let def = makeDef "TEST_VALID_MISSING" Required StringType
        Environment.SetEnvironmentVariable("TEST_VALID_MISSING", null)
        let errors = validateRequired [ def ]
        test <@ errors.Length = 1 @>

    [<Fact>]
    let ``validateRequired skips optional vars`` () =
        let def = makeDef "TEST_VALID_OPT" Optional StringType
        Environment.SetEnvironmentVariable("TEST_VALID_OPT", null)
        test <@ validateRequired [ def ] |> List.isEmpty @>

    [<Fact>]
    let ``validateRequired reports empty required vars`` () =
        let def = makeDef "TEST_VALID_EMPTY" Required StringType
        Environment.SetEnvironmentVariable("TEST_VALID_EMPTY", "   ")

        try
            let errors = validateRequired [ def ]
            test <@ errors.Length = 1 @>
            test <@ errors.[0].Contains("empty") @>
        finally
            Environment.SetEnvironmentVariable("TEST_VALID_EMPTY", null)

    [<Fact>]
    let ``validateRequired reports invalid value type`` () =
        let def = makeDef "TEST_VALID_BAD_INT" Required IntType
        Environment.SetEnvironmentVariable("TEST_VALID_BAD_INT", "not-a-number")

        try
            let errors = validateRequired [ def ]
            test <@ errors.Length = 1 @>
        finally
            Environment.SetEnvironmentVariable("TEST_VALID_BAD_INT", null)

    [<Fact>]
    let ``validateRequired handles multiple defs`` () =
        let def1 = makeDef "TEST_MULTI_1" Required StringType
        let def2 = makeDef "TEST_MULTI_2" Required StringType
        let def3 = makeDef "TEST_MULTI_3" Optional StringType
        Environment.SetEnvironmentVariable("TEST_MULTI_1", "ok")
        Environment.SetEnvironmentVariable("TEST_MULTI_2", null)
        Environment.SetEnvironmentVariable("TEST_MULTI_3", null)

        try
            let errors = validateRequired [ def1; def2; def3 ]
            // Only def2 should fail (def1 is set, def3 is optional)
            test <@ errors.Length = 1 @>
            test <@ errors.[0].Contains("TEST_MULTI_2") @>
        finally
            Environment.SetEnvironmentVariable("TEST_MULTI_1", null)
            Environment.SetEnvironmentVariable("TEST_MULTI_2", null)
