/// HTTP fixture recording and replay infrastructure.
/// Records live HTTP request/response pairs as JSON fixtures,
/// then replays them via a custom HttpMessageHandler for deterministic tests.
module UnionConfig.Ssm.Tests.HttpFixtures

open System
open System.IO
open System.Net
open System.Net.Http
open System.Text.Json
open System.Threading
open System.Threading.Tasks

/// Recorded HTTP fixture
type HttpFixture =
    { Method: string
      RequestUri: string
      RequestBody: string option
      StatusCode: int
      ResponseBody: string
      ResponseHeaders: (string * string) list
      RecordedAt: DateTime }

/// Directory for storing fixtures
let fixturesDir =
    let dir =
        Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "Fixtures", "Recorded")

    Directory.CreateDirectory(dir) |> ignore
    dir

/// Generate a fixture filename from request details
let fixtureFilename (method: string) (uri: string) =
    let sanitized =
        try
            uri
            |> fun s -> Uri(s).PathAndQuery
            |> fun s -> s.Replace("/", "_").Replace("?", "_").Replace("&", "_")
            |> fun s -> if s.Length > 50 then s.Substring(0, 50) else s
        with _ ->
            "unknown"

    $"%s{method}_%s{sanitized}.json"

/// Save a fixture to disk
let saveFixture (fixture: HttpFixture) =
    let filename = fixtureFilename fixture.Method fixture.RequestUri
    let path = Path.Combine(fixturesDir, filename)

    let json =
        JsonSerializer.Serialize(fixture, JsonSerializerOptions(WriteIndented = true))

    File.WriteAllText(path, json)
    path

/// Load a fixture from disk
let loadFixture (method: string) (uri: string) : HttpFixture option =
    let filename = fixtureFilename method uri
    let path = Path.Combine(fixturesDir, filename)

    if File.Exists(path) then
        let json = File.ReadAllText(path)
        Some(JsonSerializer.Deserialize<HttpFixture>(json))
    else
        None

/// Load all fixtures from a directory
let loadAllFixtures (dir: string) : HttpFixture list =
    if Directory.Exists(dir) then
        Directory.GetFiles(dir, "*.json")
        |> Array.map (fun path ->
            let json = File.ReadAllText(path)
            JsonSerializer.Deserialize<HttpFixture>(json))
        |> Array.toList
    else
        []

/// HTTP message handler that replays fixtures
type FixtureReplayHandler(fixtures: HttpFixture list) =
    inherit HttpMessageHandler()

    let fixtureMap =
        fixtures |> List.map (fun f -> (f.Method, f.RequestUri), f) |> Map.ofList

    override _.SendAsync(request: HttpRequestMessage, _: CancellationToken) =
        let key = (request.Method.Method, request.RequestUri.ToString())

        match Map.tryFind key fixtureMap with
        | Some fixture ->
            let response = new HttpResponseMessage(enum<HttpStatusCode> fixture.StatusCode)

            response.Content <- new StringContent(fixture.ResponseBody)

            for (name, value) in fixture.ResponseHeaders do
                response.Headers.TryAddWithoutValidation(name, value) |> ignore

            Task.FromResult(response)
        | None ->
            failwithf
                "No fixture found for %s %s. Available fixtures: %A"
                request.Method.Method
                (request.RequestUri.ToString())
                (fixtureMap |> Map.keys |> Seq.toList)

/// Create an HttpClient that replays fixtures
let createReplayClient (fixtures: HttpFixture list) =
    new HttpClient(new FixtureReplayHandler(fixtures))

/// Record a fixture from a live HTTP request
let recordFixture (client: HttpClient) (request: HttpRequestMessage) : Async<HttpFixture> =
    async {
        let! requestBody =
            if isNull request.Content then
                async { return None }
            else
                async {
                    let! body = request.Content.ReadAsStringAsync() |> Async.AwaitTask
                    return Some body
                }

        let! response = client.SendAsync(request) |> Async.AwaitTask
        let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask

        let headers =
            response.Headers
            |> Seq.collect (fun h -> h.Value |> Seq.map (fun v -> h.Key, v))
            |> Seq.toList

        return
            { Method = request.Method.Method
              RequestUri = request.RequestUri.ToString()
              RequestBody = requestBody
              StatusCode = int response.StatusCode
              ResponseBody = responseBody
              ResponseHeaders = headers
              RecordedAt = DateTime.UtcNow }
    }
