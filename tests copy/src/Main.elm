module Main exposing (main)

import Tests


main : Program () () msg
main =
    let
        program =
            Platform.worker
                { init = \() -> ( (), Cmd.none )
                , update = \_ () -> ( (), Cmd.none )
                , subscriptions = \() -> Sub.none
                }
    in
    runAllTests program


runAllTests : a -> a
runAllTests a =
    let
        runSeedTest =
            Runner.String.runWithOptions 1 SeedTests.fixedSeed

        _ =
            [ [ Runner.String.run Tests.all ]
            , List.map runSeedTest SeedTests.tests
            , List.map (runSeedTest >> removeAutoFail) SeedTests.noAutoFail
            ]
                |> List.concat
                |> List.foldl combineSummaries emptySummary
                |> Runner.Log.logOutput
    in
    a
