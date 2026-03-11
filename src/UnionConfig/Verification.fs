/// Generic verification infrastructure for config values.
module UnionConfig.Verification

/// Result of verifying a config value
type VerificationResult =
    | VerifySuccess of message: string
    | VerifyFailed of message: string
    | VerifySkipped of reason: string

/// Display verification results with colored output (plain ANSI codes, no CommandTree dependency)
let displayVerificationResults (results: (string * VerificationResult) array) =
    let green = "\x1b[32m"
    let red = "\x1b[31m"
    let yellow = "\x1b[33m"
    let reset = "\x1b[0m"

    let successes =
        results
        |> Array.filter (fun (_, r) ->
            match r with
            | VerifySuccess _ -> true
            | VerifyFailed _
            | VerifySkipped _ -> false)

    let failures =
        results
        |> Array.filter (fun (_, r) ->
            match r with
            | VerifyFailed _ -> true
            | VerifySuccess _
            | VerifySkipped _ -> false)

    let skipped =
        results
        |> Array.filter (fun (_, r) ->
            match r with
            | VerifySkipped _ -> true
            | VerifySuccess _
            | VerifyFailed _ -> false)

    if Array.isEmpty results then
        printfn "No verifiable config vars found."
    else
        for (name, result) in results do
            match result with
            | VerifySuccess msg -> printfn $"  %s{green}✓%s{reset} %s{name}: %s{msg}"
            | VerifyFailed msg -> printfn $"  %s{red}✗%s{reset} %s{name}: %s{msg}"
            | VerifySkipped reason -> printfn $"  %s{yellow}!%s{reset} %s{name}: Skipped - %s{reason}"

        printfn ""
        printfn $"Results: %d{successes.Length} passed, %d{failures.Length} failed, %d{skipped.Length} skipped"
