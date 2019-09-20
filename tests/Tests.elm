module Tests exposing (all)

import Test exposing (..)
import Test.Runner
import Test.Runner.Failure exposing (Reason(..))
import Tests.Base16
import Tests.Base32
import Tests.Base64


all : Test
all =
    Test.concat
        [ Tests.Base16.suite
        , Tests.Base32.suite
        , Tests.Base64.suite
        ]
