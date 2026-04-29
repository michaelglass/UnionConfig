module UnionConfig.Tests.TypesTests

open Xunit
open Swensen.Unquote
open UnionConfig.Types

module ParseBoolTests =
    [<Fact>]
    let ``parseBool parses true`` () = test <@ parseBool "true" = Some true @>

    [<Fact>]
    let ``parseBool parses TRUE (case insensitive)`` () = test <@ parseBool "TRUE" = Some true @>

    [<Fact>]
    let ``parseBool parses 1`` () = test <@ parseBool "1" = Some true @>

    [<Fact>]
    let ``parseBool parses false`` () =
        test <@ parseBool "false" = Some false @>

    [<Fact>]
    let ``parseBool parses FALSE (case insensitive)`` () =
        test <@ parseBool "FALSE" = Some false @>

    [<Fact>]
    let ``parseBool parses 0`` () = test <@ parseBool "0" = Some false @>

    [<Fact>]
    let ``parseBool returns None for invalid`` () =
        test <@ parseBool "yes" = None @>
        test <@ parseBool "no" = None @>
        test <@ parseBool "" = None @>

    [<Fact>]
    let ``parseBool trims whitespace`` () =
        test <@ parseBool "  true  " = Some true @>

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
            if s.StartsWith("sk-") then
                None
            else
                Some "must start with sk-"

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
    let ``string converts BoolValue to string`` () =
        test <@ ConfigValue.string (Some(BoolValue true)) = "true" @>
        test <@ ConfigValue.string (Some(BoolValue false)) = "false" @>

    [<Fact>]
    let ``string converts FloatValue to string`` () =
        test <@ ConfigValue.string (Some(FloatValue 3.14)) = "3.14" @>

    [<Fact>]
    let ``string fails on None`` () =
        raises<exn> <@ ConfigValue.string None @>

    [<Fact>]
    let ``stringOption returns None for None`` () =
        test <@ ConfigValue.stringOption None = None @>

    [<Fact>]
    let ``stringOption returns Some for StringValue`` () =
        test <@ ConfigValue.stringOption (Some(StringValue "hi")) = Some "hi" @>

    [<Fact>]
    let ``stringOption converts IntValue`` () =
        test <@ ConfigValue.stringOption (Some(IntValue 42)) = Some "42" @>

    [<Fact>]
    let ``stringOption converts BoolValue`` () =
        test <@ ConfigValue.stringOption (Some(BoolValue true)) = Some "true" @>

    [<Fact>]
    let ``stringOption converts FloatValue`` () =
        test <@ ConfigValue.stringOption (Some(FloatValue 1.5)) = Some "1.5" @>

    [<Fact>]
    let ``int extracts from IntValue`` () =
        test <@ ConfigValue.int (Some(IntValue 42)) = 42 @>

    [<Fact>]
    let ``int parses from StringValue`` () =
        test <@ ConfigValue.int (Some(StringValue "99")) = 99 @>

    [<Fact>]
    let ``int fails on non-parseable StringValue`` () =
        raises<exn> <@ ConfigValue.int (Some(StringValue "abc")) @>

    [<Fact>]
    let ``int fails on type mismatch`` () =
        raises<exn> <@ ConfigValue.int (Some(BoolValue true)) @>

    [<Fact>]
    let ``int fails on None`` () = raises<exn> <@ ConfigValue.int None @>

    [<Fact>]
    let ``intOption returns Some for IntValue`` () =
        test <@ ConfigValue.intOption (Some(IntValue 42)) = Some 42 @>

    [<Fact>]
    let ``intOption parses from StringValue`` () =
        test <@ ConfigValue.intOption (Some(StringValue "99")) = Some 99 @>

    [<Fact>]
    let ``intOption fails on non-parseable StringValue`` () =
        raises<exn> <@ ConfigValue.intOption (Some(StringValue "abc")) @>

    [<Fact>]
    let ``intOption fails on type mismatch`` () =
        raises<exn> <@ ConfigValue.intOption (Some(BoolValue true)) @>

    [<Fact>]
    let ``intOption returns None for None`` () =
        test <@ ConfigValue.intOption None = None @>

    [<Fact>]
    let ``bool extracts from BoolValue`` () =
        test <@ ConfigValue.bool (Some(BoolValue true)) = true @>

    [<Fact>]
    let ``bool fails on type mismatch`` () =
        raises<exn> <@ ConfigValue.bool (Some(StringValue "true")) @>

    [<Fact>]
    let ``bool fails on None`` () = raises<exn> <@ ConfigValue.bool None @>

    [<Fact>]
    let ``boolOption returns Some for BoolValue`` () =
        test <@ ConfigValue.boolOption (Some(BoolValue false)) = Some false @>

    [<Fact>]
    let ``boolOption fails on type mismatch`` () =
        raises<exn> <@ ConfigValue.boolOption (Some(StringValue "true")) @>

    [<Fact>]
    let ``boolOption returns None for None`` () =
        test <@ ConfigValue.boolOption None = None @>

    [<Fact>]
    let ``float extracts from FloatValue`` () =
        test <@ ConfigValue.float (Some(FloatValue 3.14)) = 3.14 @>

    [<Fact>]
    let ``float parses from StringValue`` () =
        test <@ ConfigValue.float (Some(StringValue "2.5")) = 2.5 @>

    [<Fact>]
    let ``float fails on non-parseable StringValue`` () =
        raises<exn> <@ ConfigValue.float (Some(StringValue "abc")) @>

    [<Fact>]
    let ``float fails on type mismatch`` () =
        raises<exn> <@ ConfigValue.float (Some(BoolValue true)) @>

    [<Fact>]
    let ``float fails on None`` () =
        raises<exn> <@ ConfigValue.float None @>

    [<Fact>]
    let ``floatOption returns Some for FloatValue`` () =
        test <@ ConfigValue.floatOption (Some(FloatValue 1.5)) = Some 1.5 @>

    [<Fact>]
    let ``floatOption parses from StringValue`` () =
        test <@ ConfigValue.floatOption (Some(StringValue "2.5")) = Some 2.5 @>

    [<Fact>]
    let ``floatOption fails on non-parseable StringValue`` () =
        raises<exn> <@ ConfigValue.floatOption (Some(StringValue "abc")) @>

    [<Fact>]
    let ``floatOption fails on type mismatch`` () =
        raises<exn> <@ ConfigValue.floatOption (Some(BoolValue true)) @>

    [<Fact>]
    let ``floatOption returns None for None`` () =
        test <@ ConfigValue.floatOption None = None @>

    [<Fact>]
    let ``custom parses with provided parser`` () =
        let parser (s: string) =
            if s.StartsWith("sk-") then Some s else None

        let result = ConfigValue.custom<string> parser (Some(StringValue "sk-abc"))
        test <@ result = "sk-abc" @>

    [<Fact>]
    let ``custom fails when parser returns None`` () =
        let parser (_: string) : string option = None

        Assert.Throws<exn>(fun () -> ConfigValue.custom<string> parser (Some(StringValue "bad")) |> ignore)
        |> ignore

    [<Fact>]
    let ``custom fails on None`` () =
        let parser (s: string) = Some s

        Assert.Throws<exn>(fun () -> ConfigValue.custom<string> parser None |> ignore)
        |> ignore

    [<Fact>]
    let ``customOption parses with provided parser`` () =
        let parser (s: string) =
            if s.StartsWith("sk-") then Some s else None

        let result = ConfigValue.customOption<string> parser (Some(StringValue "sk-abc"))
        test <@ result = Some "sk-abc" @>

    [<Fact>]
    let ``customOption fails when parser returns None`` () =
        let parser (_: string) : string option = None

        Assert.Throws<exn>(fun () -> ConfigValue.customOption<string> parser (Some(StringValue "bad")) |> ignore)
        |> ignore

    [<Fact>]
    let ``customOption returns None for None`` () =
        let parser (s: string) = Some s
        let result = ConfigValue.customOption<string> parser None
        test <@ result = None @>

module ConfigVarKindTests =
    [<Fact>]
    let ``isPersistable: External returns false`` () =
        test <@ ConfigVarKind.isPersistable External = false @>

    [<Fact>]
    let ``isPersistable: all other kinds return true`` () =
        test <@ ConfigVarKind.isPersistable Manual = true @>
        test <@ ConfigVarKind.isPersistable (AutoGenerated None) = true @>
        test <@ ConfigVarKind.isPersistable (AutoGenerated(Some "x")) = true @>
        test <@ ConfigVarKind.isPersistable Infrastructure = true @>
        test <@ ConfigVarKind.isPersistable AutoProvisioned = true @>
