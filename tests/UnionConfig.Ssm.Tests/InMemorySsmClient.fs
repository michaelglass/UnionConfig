/// In-memory test double for AmazonSimpleSystemsManagementClient.
/// Overrides virtual methods to store parameters in a mutable map.
module UnionConfig.Ssm.Tests.InMemorySsmClient

#nowarn "3536"

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open Amazon
open Amazon.Runtime
open Amazon.SimpleSystemsManagement
open Amazon.SimpleSystemsManagement.Model

/// Test double that stores SSM parameters in memory.
/// Subclasses AmazonSimpleSystemsManagementClient and overrides its virtual methods.
type InMemorySsmClient() =
    inherit
        AmazonSimpleSystemsManagementClient(
            AnonymousAWSCredentials(),
            AmazonSimpleSystemsManagementConfig(RegionEndpoint = RegionEndpoint.USEast1)
        )

    let mutable store = Map.empty<string, (string * bool)> // name -> (value, isSecure)
    let mutable failOnPut = false
    let mutable failOnDelete = false
    let mutable pageSize = 0 // 0 = no pagination

    /// Add a parameter to the in-memory store
    member _.AddParameter(name: string, value: string) =
        store <- Map.add name (value, false) store

    /// Add a secure parameter to the in-memory store
    member _.AddSecureParameter(name: string, value: string) =
        store <- Map.add name (value, true) store

    /// Get the current store contents
    member _.GetStore() = store

    /// Clear all parameters
    member _.Clear() = store <- Map.empty

    /// Configure the client to fail on PutParameter calls
    member _.SetFailOnPut(fail: bool) = failOnPut <- fail

    /// Configure the client to fail on DeleteParameter calls
    member _.SetFailOnDelete(fail: bool) = failOnDelete <- fail

    /// Set page size for GetParametersByPath pagination (0 = no pagination)
    member _.SetPageSize(size: int) = pageSize <- size

    override _.GetParameterAsync(request: GetParameterRequest, _ct: CancellationToken) =
        match Map.tryFind request.Name store with
        | Some(value, _) ->
            let param = Parameter()
            param.Name <- request.Name
            param.Value <- value
            let response = GetParameterResponse()
            response.Parameter <- param
            Task.FromResult(response)
        | None ->
            let ex = ParameterNotFoundException($"Parameter {request.Name} not found")
            raise (AggregateException(ex))

    override _.GetParametersByPathAsync(request: GetParametersByPathRequest, _ct: CancellationToken) =
        let prefix = request.Path

        let allMatching =
            store
            |> Map.filter (fun k _ -> k.StartsWith(prefix, StringComparison.Ordinal))
            |> Map.toList
            |> List.sortBy fst

        // Support pagination via NextToken
        let startIndex =
            match request.NextToken with
            | null
            | "" -> 0
            | token -> int token

        let pageParams, nextToken =
            if pageSize > 0 && startIndex + pageSize < allMatching.Length then
                let page = allMatching |> List.skip startIndex |> List.take pageSize
                (page, string (startIndex + pageSize))
            else
                let page = allMatching |> List.skip startIndex
                (page, null)

        let parameters =
            pageParams
            |> List.map (fun (k, (v, _)) ->
                let p = Parameter()
                p.Name <- k
                p.Value <- v
                p)

        let response = GetParametersByPathResponse()
        response.Parameters <- List<_>(parameters)
        response.NextToken <- nextToken
        Task.FromResult(response)

    override _.PutParameterAsync(request: PutParameterRequest, _ct: CancellationToken) =
        if failOnPut then
            raise (Exception("Simulated PutParameter failure"))

        let isSecure = request.Type = ParameterType.SecureString
        store <- Map.add request.Name (request.Value, isSecure) store
        Task.FromResult(PutParameterResponse())

    override _.DeleteParameterAsync(request: DeleteParameterRequest, _ct: CancellationToken) =
        if failOnDelete then
            raise (Exception("Simulated DeleteParameter failure"))

        match Map.tryFind request.Name store with
        | Some _ ->
            store <- Map.remove request.Name store
            Task.FromResult(DeleteParameterResponse())
        | None ->
            let ex = ParameterNotFoundException($"Parameter {request.Name} not found")
            raise (AggregateException(ex))

/// Test double that returns null Parameters list from GetParametersByPath.
type NullParametersSsmClient() =
    inherit
        AmazonSimpleSystemsManagementClient(
            AnonymousAWSCredentials(),
            AmazonSimpleSystemsManagementConfig(RegionEndpoint = RegionEndpoint.USEast1)
        )

    override _.GetParametersByPathAsync(_request: GetParametersByPathRequest, _ct: CancellationToken) =
        let response = GetParametersByPathResponse()
        response.Parameters <- null
        Task.FromResult(response)
