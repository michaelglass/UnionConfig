module UnionConfig.Tests.ConfigRegistryTests

open System
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

      Doc =
        { Description = $"Nested config var %s{name}"
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
