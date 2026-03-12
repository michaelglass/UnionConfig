module UnionConfig.Tests.VerificationTests

open Xunit
open UnionConfig.Verification

module DisplayVerificationResultsTests =
    [<Fact>]
    let ``handles empty results without error`` () =
        // Just verifies the function executes without throwing
        displayVerificationResults [||]

    [<Fact>]
    let ``handles success results without error`` () =
        displayVerificationResults [| ("MY_VAR", VerifySuccess "looks good") |]

    [<Fact>]
    let ``handles failed results without error`` () =
        displayVerificationResults [| ("BAD_VAR", VerifyFailed "invalid value") |]

    [<Fact>]
    let ``handles skipped results without error`` () =
        displayVerificationResults [| ("SKIP_VAR", VerifySkipped "not applicable") |]

    [<Fact>]
    let ``handles mixed results without error`` () =
        displayVerificationResults
            [| ("OK_VAR", VerifySuccess "valid")
               ("BAD_VAR", VerifyFailed "broken")
               ("SKIP_VAR", VerifySkipped "N/A") |]
