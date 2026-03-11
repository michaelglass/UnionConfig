module UnionConfig.Tests.TypesTests

open Xunit
open Swensen.Unquote
open UnionConfig.Types

module ParseValueTests =
    [<Fact>]
    let ``parseValue StringType returns StringValue`` () =
        match parseValue StringType "hello" with
        | Ok(StringValue s) -> test <@ s = "hello" @>
        | other -> failwithf "Expected Ok(StringValue), got %A" other

    [<Fact>]
    let ``parseValue IntType parses valid int`` () =
        match parseValue IntType "42" with
        | Ok(IntValue i) -> test <@ i = 42 @>
        | other -> failwithf "Expected Ok(IntValue), got %A" other

    [<Fact>]
    let ``parseValue IntType rejects non-int`` () =
        test <@ Result.isError (parseValue IntType "abc") @>

    [<Fact>]
    let ``parseValue BoolType parses true variants`` () =
        match parseValue BoolType "true" with
        | Ok(BoolValue b) -> test <@ b = true @>
        | other -> failwithf "Expected Ok(BoolValue), got %A" other

        match parseValue BoolType "1" with
        | Ok(BoolValue b) -> test <@ b = true @>
        | other -> failwithf "Expected Ok(BoolValue), got %A" other

    [<Fact>]
    let ``parseValue BoolType parses false variants`` () =
        match parseValue BoolType "false" with
        | Ok(BoolValue b) -> test <@ b = false @>
        | other -> failwithf "Expected Ok(BoolValue), got %A" other

        match parseValue BoolType "0" with
        | Ok(BoolValue b) -> test <@ b = false @>
        | other -> failwithf "Expected Ok(BoolValue), got %A" other

    [<Fact>]
    let ``parseValue BoolType rejects invalid`` () =
        test <@ Result.isError (parseValue BoolType "yes") @>

    [<Fact>]
    let ``parseValue FloatType parses valid float`` () =
        match parseValue FloatType "3.14" with
        | Ok(FloatValue f) -> test <@ f = 3.14 @>
        | other -> failwithf "Expected Ok(FloatValue), got %A" other

    [<Fact>]
    let ``parseValue FloatType rejects non-float`` () =
        test <@ Result.isError (parseValue FloatType "not-a-float") @>

    [<Fact>]
    let ``parseValue CustomType calls validator`` () =
        let validator (s: string) =
            if s.StartsWith("sk-") then None
            else Some "must start with sk-"

        match parseValue (CustomType("ApiKey", validator)) "sk-123" with
        | Ok(StringValue s) -> test <@ s = "sk-123" @>
        | other -> failwithf "Expected Ok(StringValue), got %A" other

        test <@ Result.isError (parseValue (CustomType("ApiKey", validator)) "bad") @>

module ConfigValueTests =
    [<Fact>]
    let ``string extracts from StringValue`` () =
        test <@ ConfigValue.string (Some(StringValue "hello")) = "hello" @>

    [<Fact>]
    let ``string converts IntValue to string`` () =
        test <@ ConfigValue.string (Some(IntValue 42)) = "42" @>

    [<Fact>]
    let ``string fails on None`` () =
        raises<exn> <@ ConfigValue.string None @>

    [<Fact>]
    let ``stringOption returns None for None`` () =
        test <@ ConfigValue.stringOption None = None @>

    [<Fact>]
    let ``int extracts from IntValue`` () =
        test <@ ConfigValue.int (Some(IntValue 42)) = 42 @>

    [<Fact>]
    let ``int parses from StringValue`` () =
        test <@ ConfigValue.int (Some(StringValue "99")) = 99 @>

    [<Fact>]
    let ``bool extracts from BoolValue`` () =
        test <@ ConfigValue.bool (Some(BoolValue true)) = true @>

    [<Fact>]
    let ``float extracts from FloatValue`` () =
        test <@ ConfigValue.float (Some(FloatValue 3.14)) = 3.14 @>

    [<Fact>]
    let ``intOption returns None for None`` () =
        test <@ ConfigValue.intOption None = None @>

    [<Fact>]
    let ``boolOption returns None for None`` () =
        test <@ ConfigValue.boolOption None = None @>

    [<Fact>]
    let ``floatOption returns None for None`` () =
        test <@ ConfigValue.floatOption None = None @>
