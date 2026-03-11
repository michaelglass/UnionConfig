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
      Doc =
        { Description = "test"
          HowToFind = "test"
          ManagementUrl = None } }

module ReadTests =
    [<Fact>]
    let ``read required var returns value when set`` () =
        let def = makeDef "TEST_READ_REQ" Required StringType
        Environment.SetEnvironmentVariable("TEST_READ_REQ", "hello")

        try
            match read def with
            | Some(StringValue s) -> test <@ s = "hello" @>
            | other -> failwithf "Expected Some(StringValue), got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_REQ", null)

    [<Fact>]
    let ``read required var fails when missing`` () =
        let def = makeDef "TEST_READ_MISSING" Required StringType
        Environment.SetEnvironmentVariable("TEST_READ_MISSING", null)
        raises<exn> <@ read def @>

    [<Fact>]
    let ``read required var fails when empty`` () =
        let def = makeDef "TEST_READ_EMPTY" Required StringType
        Environment.SetEnvironmentVariable("TEST_READ_EMPTY", "  ")

        try
            raises<exn> <@ read def @>
        finally
            Environment.SetEnvironmentVariable("TEST_READ_EMPTY", null)

    [<Fact>]
    let ``read optional var returns None when missing`` () =
        let def = makeDef "TEST_READ_OPT" Optional StringType
        Environment.SetEnvironmentVariable("TEST_READ_OPT", null)
        test <@ Option.isNone (read def) @>

    [<Fact>]
    let ``read optional var returns None when empty`` () =
        let def = makeDef "TEST_READ_OPT_EMPTY" Optional StringType
        Environment.SetEnvironmentVariable("TEST_READ_OPT_EMPTY", "")

        try
            test <@ Option.isNone (read def) @>
        finally
            Environment.SetEnvironmentVariable("TEST_READ_OPT_EMPTY", null)

    [<Fact>]
    let ``read parses int value`` () =
        let def = makeDef "TEST_READ_INT" Required IntType
        Environment.SetEnvironmentVariable("TEST_READ_INT", "42")

        try
            match read def with
            | Some(IntValue i) -> test <@ i = 42 @>
            | other -> failwithf "Expected Some(IntValue), got %A" other
        finally
            Environment.SetEnvironmentVariable("TEST_READ_INT", null)

    [<Fact>]
    let ``read fails on invalid int`` () =
        let def = makeDef "TEST_READ_BAD_INT" Required IntType
        Environment.SetEnvironmentVariable("TEST_READ_BAD_INT", "abc")

        try
            raises<exn> <@ read def @>
        finally
            Environment.SetEnvironmentVariable("TEST_READ_BAD_INT", null)

module ValidateRequiredTests =
    [<Fact>]
    let ``validateRequired returns empty for all set`` () =
        let def = makeDef "TEST_VALID_SET" Required StringType
        Environment.SetEnvironmentVariable("TEST_VALID_SET", "value")

        try
            test <@ validateRequired [ def ] = [] @>
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
        test <@ validateRequired [ def ] = [] @>
