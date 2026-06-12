module UnionConfig.Tests.ConfigRegistryTests

open Xunit
open Swensen.Unquote
open UnionConfig.Types
open UnionConfig.ConfigRegistry

// Test DUs

type SimpleConfig =
    | DB_HOST
    | DB_PORT
    | API_KEY

type DatabaseVars =
    | DBHOST
    | DBPORT

type EmailVars =
    | SMTP_HOST
    | SMTP_PORT

type NestedConfig =
    | Database of DatabaseVars
    | Email of EmailVars

type UnsupportedConfig =
    | ValidCase
    | BadCase of string

let private simpleToDef (v: SimpleConfig) : ConfigVarDef =
    let name =
        match v with
        | DB_HOST -> "DB_HOST"
        | DB_PORT -> "DB_PORT"
        | API_KEY -> "API_KEY"

    { Name = name
      Kind = Manual
      ValueType = StringType
      Requirement = Required
      IsSecret = (v = API_KEY)
      DefaultValue = None

      Doc =
        { Description = $"Simple config var %s{name}"
          HowToFind = "env"
          ManagementUrl = None } }

let private nestedToDef (v: NestedConfig) : ConfigVarDef =
    let name, isSecret =
        match v with
        | Database DBHOST -> "DBHOST", false
        | Database DBPORT -> "DBPORT", false
        | Email SMTP_HOST -> "SMTP_HOST", false
        | Email SMTP_PORT -> "SMTP_PORT", false

    { Name = name
      Kind = Manual
      ValueType = StringType
      Requirement = Required
      IsSecret = isSecret
      DefaultValue = None

      Doc =
        { Description = $"Nested config var %s{name}"
          HowToFind = "env"
          ManagementUrl = None } }

let private dummyToDef (_: UnsupportedConfig) : ConfigVarDef =
    { Name = "DUMMY"
      Kind = Manual
      ValueType = StringType
      Requirement = Required
      IsSecret = false
      DefaultValue = None

      Doc =
        { Description = "dummy"
          HowToFind = "env"
          ManagementUrl = None } }

module AllDefsTests =
    [<Fact>]
    let ``allDefs discovers all cases of flat DU`` () =
        let defs = allDefs simpleToDef
        test <@ defs.Length = 3 @>
        let defNames = defs |> Array.map (fun d -> d.Name)
        test <@ defNames = [| "DB_HOST"; "DB_PORT"; "API_KEY" |] @>

    [<Fact>]
    let ``allDefs discovers all leaf cases of nested DU`` () =
        let defs = allDefs nestedToDef
        test <@ defs.Length = 4 @>
        let defNames = defs |> Array.map (fun d -> d.Name)
        test <@ defNames = [| "DBHOST"; "DBPORT"; "SMTP_HOST"; "SMTP_PORT" |] @>

    [<Fact>]
    let ``allDefs preserves def fields`` () =
        let defs = allDefs simpleToDef
        let apiKeyDef = defs |> Array.find (fun d -> d.Name = "API_KEY")
        test <@ apiKeyDef.IsSecret = true @>
        test <@ apiKeyDef.Requirement = Required @>
        test <@ apiKeyDef.Kind = Manual @>

    [<Fact>]
    let ``allDefs throws for DU cases with unsupported payload`` () = raises<exn> <@ allDefs dummyToDef @>

module AllDefsGroupedTests =
    [<Fact>]
    let ``allDefsGrouped groups nested DU by wrapper case name`` () =
        let grouped = allDefsGrouped nestedToDef
        test <@ grouped.Length = 2 @>
        let groupNames = grouped |> Array.map fst
        test <@ groupNames = [| "Database"; "Email" |] @>

    [<Fact>]
    let ``allDefsGrouped returns correct defs per group`` () =
        let grouped = allDefsGrouped nestedToDef
        let dbGroup = grouped |> Array.find (fun (name, _) -> name = "Database")
        let dbNames = snd dbGroup |> Array.map (fun d -> d.Name)
        test <@ dbNames = [| "DBHOST"; "DBPORT" |] @>

        let emailGroup = grouped |> Array.find (fun (name, _) -> name = "Email")
        let emailNames = snd emailGroup |> Array.map (fun d -> d.Name)
        test <@ emailNames = [| "SMTP_HOST"; "SMTP_PORT" |] @>

    [<Fact>]
    let ``allDefsGrouped uses empty string for flat DU cases`` () =
        let grouped = allDefsGrouped simpleToDef
        test <@ grouped.Length = 1 @>
        let groupName, defs = grouped.[0]
        test <@ groupName = "" @>
        test <@ defs.Length = 3 @>

    [<Fact>]
    let ``allDefsGrouped preserves all def fields`` () =
        let grouped = allDefsGrouped nestedToDef
        let emailGroup = grouped |> Array.find (fun (name, _) -> name = "Email")
        let smtpHostDef = snd emailGroup |> Array.find (fun d -> d.Name = "SMTP_HOST")
        test <@ smtpHostDef.Kind = Manual @>
        test <@ smtpHostDef.Requirement = Required @>
        test <@ smtpHostDef.IsSecret = false @>

module ByNameTests =
    [<Fact>]
    let ``byName creates lookup map`` () =
        let defs = allDefs simpleToDef
        let lookup = byName defs
        test <@ lookup.Count = 3 @>
        test <@ lookup.["DB_HOST"].Name = "DB_HOST" @>
        test <@ lookup.["API_KEY"].IsSecret = true @>

module NamesTests =
    [<Fact>]
    let ``names returns all var names`` () =
        let defs = allDefs simpleToDef
        let result = names defs
        test <@ result = [| "DB_HOST"; "DB_PORT"; "API_KEY" |] @>

    [<Fact>]
    let ``names returns all var names from nested DU`` () =
        let defs = allDefs nestedToDef
        let result = names defs
        test <@ result = [| "DBHOST"; "DBPORT"; "SMTP_HOST"; "SMTP_PORT" |] @>

module ErrorCasesTests =
    // DU where inner case has fields — triggers the "has fields" failwithf
    type InnerWithPayload =
        | Good
        | Bad of int

    type NestedWithPayloadInner = Wrapper of InnerWithPayload

    let private dummyDef (_: NestedWithPayloadInner) : ConfigVarDef =
        { Name = "DUMMY"
          Kind = Manual
          ValueType = StringType
          Requirement = Required
          IsSecret = false
          DefaultValue = None
          Doc =
            { Description = "dummy"
              HowToFind = ""
              ManagementUrl = None } }

    [<Fact>]
    let ``allDefs throws for nested DU with inner cases that have fields`` () =
        let ex = Assert.Throws<exn>(fun () -> allDefs dummyDef |> ignore)
        test <@ ex.Message.Contains("has fields") @>

    // Multi-field case — triggers the "unsupported payload" failwithf
    type MultiFieldConfig = TwoFields of string * int

    let private multiFieldDef (_: MultiFieldConfig) : ConfigVarDef =
        { Name = "DUMMY"
          Kind = Manual
          ValueType = StringType
          Requirement = Required
          IsSecret = false
          DefaultValue = None
          Doc =
            { Description = "dummy"
              HowToFind = ""
              ManagementUrl = None } }

    [<Fact>]
    let ``allDefs throws for DU cases with multiple fields`` () =
        let ex = Assert.Throws<exn>(fun () -> allDefs multiFieldDef |> ignore)
        test <@ ex.Message.Contains("unsupported payload") @>

    [<Fact>]
    let ``allDefs throws for non-union type`` () =
        let dummyStringDef (_: string) : ConfigVarDef =
            { Name = "DUMMY"
              Kind = Manual
              ValueType = StringType
              Requirement = Required
              IsSecret = false
              DefaultValue = None
              Doc =
                { Description = "dummy"
                  HowToFind = ""
                  ManagementUrl = None } }

        let ex = Assert.Throws<exn>(fun () -> allDefs dummyStringDef |> ignore)
        test <@ ex.Message.Contains("not a discriminated union") @>

    // 3-level nesting: Top -> Middle (single DU field) -> Leaf (fieldless).
    // enumerateAll only descends two levels, so the third level must be rejected
    // with a clear depth-limit message rather than silently skipped or mislabelled.
    type Leaf3 =
        | LeafA
        | LeafB

    type Middle3 = Middle of Leaf3

    type Top3 = Top of Middle3

    let private top3Def (_: Top3) : ConfigVarDef =
        { Name = "DUMMY"
          Kind = Manual
          ValueType = StringType
          Requirement = Required
          IsSecret = false
          DefaultValue = None
          Doc =
            { Description = "dummy"
              HowToFind = ""
              ManagementUrl = None } }

    [<Fact>]
    let ``allDefs throws a clear depth-limit error for 3-level nested DU`` () =
        let ex = Assert.Throws<exn>(fun () -> allDefs top3Def |> ignore)
        test <@ ex.Message.Contains("nesting") @>

    // Inner case carrying multiple fields — distinct from both the single-DU-field
    // depth-limit shape and the single-non-DU-field "has fields" shape.
    type MultiFieldInner =
        | OkLeaf
        | BadMulti of string * int

    type WrapMultiField = WrapMulti of MultiFieldInner

    let private wrapMultiDef (_: WrapMultiField) : ConfigVarDef =
        { Name = "DUMMY"
          Kind = Manual
          ValueType = StringType
          Requirement = Required
          IsSecret = false
          DefaultValue = None
          Doc =
            { Description = "dummy"
              HowToFind = ""
              ManagementUrl = None } }

    [<Fact>]
    let ``allDefs throws has-fields error for inner case with multiple fields`` () =
        let ex = Assert.Throws<exn>(fun () -> allDefs wrapMultiDef |> ignore)
        test <@ ex.Message.Contains("has fields") @>
