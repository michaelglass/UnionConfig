/// Reflection-based discovery of discriminated union config cases.
/// Replaces manual allCases arrays with automatic DU case enumeration.
module UnionConfig.ConfigRegistry

open Microsoft.FSharp.Reflection
open UnionConfig.Types

/// Enumerate all leaf values of a DU type via reflection.
/// For cases with no fields: creates the case value directly via FSharpValue.MakeUnion.
/// For cases with a single field that is itself a DU: recursively enumerates inner cases and wraps.
/// Cases with non-DU payload or multiple fields: skip.
let private enumerateAll<'T> () : 'T array =
    let ty = typeof<'T>

    if not (FSharpType.IsUnion(ty)) then
        failwithf "Type '%s' is not a discriminated union" ty.Name

    let cases = FSharpType.GetUnionCases(ty)

    [| for case in cases do
           let fields = case.GetFields()

           if fields.Length = 0 then
               yield FSharpValue.MakeUnion(case, [||]) :?> 'T
           elif fields.Length = 1 && FSharpType.IsUnion(fields.[0].PropertyType) then
               let innerType = fields.[0].PropertyType
               let innerCases = FSharpType.GetUnionCases(innerType)

               for innerCase in innerCases do
                   let innerFields = innerCase.GetFields()

                   if innerFields.Length = 0 then
                       let innerValue = FSharpValue.MakeUnion(innerCase, [||])
                       yield FSharpValue.MakeUnion(case, [| innerValue |]) :?> 'T |]

/// Get the wrapper case name for a value (Some for nested, None for flat).
let private getWrapperCaseName<'T> (value: 'T) : string option =
    let ty = typeof<'T>
    let caseInfo, fieldValues = FSharpValue.GetUnionFields(value :> obj, ty)
    let fields = caseInfo.GetFields()

    if
        fields.Length = 1
        && FSharpType.IsUnion(fields.[0].PropertyType)
        && fieldValues.Length = 1
    then
        Some caseInfo.Name
    else
        None

/// Discover all cases of a DU, map through toDef function. Returns flat array.
let allDefs<'T> (toDef: 'T -> ConfigVarDef) : ConfigVarDef array = enumerateAll<'T> () |> Array.map toDef

/// Discover all cases and return (groupName, defs) pairs.
/// Group name = wrapper case name for nested DUs, "" for flat cases.
let allDefsGrouped<'T> (toDef: 'T -> ConfigVarDef) : (string * ConfigVarDef array) array =
    let allValues = enumerateAll<'T> ()

    allValues
    |> Array.groupBy (fun v ->
        match getWrapperCaseName v with
        | Some name -> name
        | None -> "")
    |> Array.map (fun (groupName, values) -> (groupName, values |> Array.map toDef))

/// Build a name-to-def lookup map.
let byName (defs: ConfigVarDef array) : Map<string, ConfigVarDef> =
    defs |> Array.map (fun d -> d.Name, d) |> Map.ofArray

/// Extract all var names from defs.
let names (defs: ConfigVarDef array) : string array = defs |> Array.map (fun d -> d.Name)
