module Tests.Base16 exposing (bytesOf, cases, decodeTests, encodeTests, suite)

import Base16 exposing (decode, encode)
import Char
import Expect exposing (Expectation)
import Test exposing (..)


bytesOf : String -> List Int
bytesOf s =
    String.toList s |> List.map Char.toCode


cases : List ( String, String )
cases =
    [ ( "", "" )
    , ( "f", "66" )
    , ( "fo", "666F" )
    , ( "foo", "666F6F" )
    , ( "foob", "666F6F62" )
    , ( "fooba", "666F6F6261" )
    , ( "foobar", "666F6F626172" )
    ]


suite : Test
suite =
    List.concat
        [ encodeTests
        , decodeTests
        ]
        |> describe "Base16"


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
