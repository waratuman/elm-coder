module Tests.Base64 exposing (bytesOf, cases, decodeTests, encodeTests, suite)

import Base64 exposing (decode, encode)
import Char
import Expect exposing (Expectation)
import Test exposing (..)


bytesOf : String -> List Int
bytesOf s =
    String.toList s |> List.map Char.toCode


cases : List ( String, String )
cases =
    [ ( "", "" )
    , ( "f", "Zg==" )
    , ( "fo", "Zm8=" )
    , ( "foo", "Zm9v" )
    , ( "foob", "Zm9vYg==" )
    , ( "fooba", "Zm9vYmE=" )
    , ( "foobar", "Zm9vYmFy" )
    ]


suite : Test
suite =
    List.concat
        [ encodeTests
        , decodeTests
        ]
        |> describe "Base64"


encodeTests : List Test
encodeTests =
    cases
        |> List.map
            (\( data, encoded ) ->
                test
                    ("encode \"" ++ data ++ "\"")
                    (\_ ->
                        Expect.equal
                            (encode (bytesOf data))
                            (Ok encoded)
                    )
            )


decodeTests : List Test
decodeTests =
    cases
        |> List.map
            (\( data, encoded ) ->
                test
                    ("decode \"" ++ encoded ++ "\"")
                <|
                    \_ ->
                        Expect.equal
                            (decode encoded)
                            (Ok (bytesOf data))
            )
