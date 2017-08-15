module Tests.Base32 exposing (..)

import Base32 exposing (encode, decode)
import Expect exposing (Expectation)
import Test exposing (..)
import Char


bytesOf : String -> List Int
bytesOf s =
    String.toList s |> List.map Char.toCode


cases : List ( String, String )
cases =
    [ ( "", "" )
    , ( "f", "MY======" )
    , ( "fo", "MZXQ====" )
    , ( "foo", "MZXW6===" )
    , ( "foob", "MZXW6YQ=" )
    , ( "fooba", "MZXW6YTB" )
    , ( "foobar", "MZXW6YTBOI======" )
    ]


suite : Test
suite =
    List.concat
        [ encodeTests
        , decodeTests
        ]
        |> describe "Base32"


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
                            (Ok encoded)
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
