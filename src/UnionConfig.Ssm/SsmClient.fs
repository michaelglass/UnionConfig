/// Generic AWS SSM Parameter Store client operations.
/// Provides get/set/delete/list without hardcoded paths or auth strategies.
module UnionConfig.Ssm.SsmClient

open System
open Amazon
open Amazon.SimpleSystemsManagement
open Amazon.SimpleSystemsManagement.Model

/// Configuration for the SSM client
[<NoComparison; NoEquality>]
type SsmClientConfig =
    { /// AWS region for SSM operations
      Region: RegionEndpoint
      /// Called before each SSM operation (e.g., to ensure AWS SSO auth)
      EnsureAuth: unit -> unit }

    /// Default config: us-east-1, no auth hook
    static member Default =
        { Region = RegionEndpoint.USEast1
          EnsureAuth = ignore }

/// Get a single SSM parameter value (with decryption)
let getParameter (config: SsmClientConfig) (path: string) : string option =
    config.EnsureAuth()

    try
        use client = new AmazonSimpleSystemsManagementClient(config.Region)
        let request = GetParameterRequest(Name = path, WithDecryption = true)
        let response = client.GetParameterAsync(request).Result

        if not (String.IsNullOrWhiteSpace(response.Parameter.Value)) then
            Some response.Parameter.Value
        else
            None
    with
    | :? AggregateException as ae when (ae.InnerException :? ParameterNotFoundException) -> None
    | _ -> None

/// Get all SSM parameters under a path prefix (with pagination and decryption)
let getParametersByPath (config: SsmClientConfig) (pathPrefix: string) : (string * string) list =
    config.EnsureAuth()

    try
        use client = new AmazonSimpleSystemsManagementClient(config.Region)

        let rec getAllParameters (nextToken: string option) (acc: (string * string) list) =
            let request =
                GetParametersByPathRequest(Path = pathPrefix, WithDecryption = true, Recursive = true)

            match nextToken with
            | Some token -> request.NextToken <- token
            | None -> ()

            let response = client.GetParametersByPathAsync(request).Result

            let parameters =
                if isNull response.Parameters then
                    []
                else
                    response.Parameters |> Seq.map (fun p -> (p.Name, p.Value)) |> Seq.toList

            let allParams = acc @ parameters

            if String.IsNullOrEmpty(response.NextToken) then
                allParams
            else
                getAllParameters (Some response.NextToken) allParams

        getAllParameters None []
    with _ ->
        []

/// Set an SSM parameter value (creates or updates)
let setParameter
    (config: SsmClientConfig)
    (path: string)
    (value: string)
    (isSecure: bool)
    : Result<unit, string> =
    config.EnsureAuth()

    try
        use client = new AmazonSimpleSystemsManagementClient(config.Region)

        let paramType =
            if isSecure then
                ParameterType.SecureString
            else
                ParameterType.String

        let request =
            PutParameterRequest(Name = path, Value = value, Type = paramType, Overwrite = true)

        client.PutParameterAsync(request).Result |> ignore
        Ok()
    with ex ->
        Error ex.Message

/// Delete an SSM parameter
let deleteParameter (config: SsmClientConfig) (path: string) : Result<unit, string> =
    config.EnsureAuth()

    try
        use client = new AmazonSimpleSystemsManagementClient(config.Region)
        let request = DeleteParameterRequest(Name = path)
        client.DeleteParameterAsync(request).Result |> ignore
        Ok()
    with ex ->
        Error ex.Message
