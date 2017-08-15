module Tests.Base64 exposing (..)

import Base64 exposing (encode, decode)
import Expect exposing (Expectation)
import Test exposing (..)
import Char


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
                <|
                    \_ ->
                        Expect.equal
                            (encode (bytesOf data))
                            encoded
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
