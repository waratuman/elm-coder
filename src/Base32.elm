module Base32 exposing (encode, decode, scheme)

{-| Library for base32 encoding and decoding according to RFC 4648.

@docs encode, decode, scheme

-}

import Coder exposing (Scheme)


{-| The decoding / encoding scheme for base 32.
-}
scheme : Scheme
scheme =
    { octets = 5
    , chars = 8
    , padChar = "="
    , intToChar = intToChar
    , charToInt = charToInt
    }


{-| Decode a base32 string into a list of bytes.
-}
decode : String -> Result String (List Int)
decode =
    Coder.decode scheme


{-| Encode a list of bytes into a base32 string.
-}
encode : List Int -> Result String String
encode =
    Coder.encode scheme


charToInt : Char -> Result String Int
charToInt char =
    case char of
        'A' ->
            Ok 0

        'B' ->
            Ok 1

        'C' ->
            Ok 2

        'D' ->
            Ok 3

        'E' ->
            Ok 4

        'F' ->
            Ok 5

        'G' ->
            Ok 6

        'H' ->
            Ok 7

        'I' ->
            Ok 8

        'J' ->
            Ok 9

        'K' ->
            Ok 10

        'L' ->
            Ok 11

        'M' ->
            Ok 12

        'N' ->
            Ok 13

        'O' ->
            Ok 14

        'P' ->
            Ok 15

        'Q' ->
            Ok 16

        'R' ->
            Ok 17

        'S' ->
            Ok 18

        'T' ->
            Ok 19

        'U' ->
            Ok 20

        'V' ->
            Ok 21

        'W' ->
            Ok 22

        'X' ->
            Ok 23

        'Y' ->
            Ok 24

        'Z' ->
            Ok 25

        '2' ->
            Ok 26

        '3' ->
            Ok 27

        '4' ->
            Ok 28

        '5' ->
            Ok 29

        '6' ->
            Ok 30

        '7' ->
            Ok 31

        '=' ->
            Ok 0

        _ ->
            Err "Invalid character"


intToChar : Int -> Result String Char
intToChar int =
    case int of
        0 ->
            Ok 'A'

        1 ->
            Ok 'B'

        2 ->
            Ok 'C'

        3 ->
            Ok 'D'

        4 ->
            Ok 'E'

        5 ->
            Ok 'F'

        6 ->
            Ok 'G'

        7 ->
            Ok 'H'

        8 ->
            Ok 'I'

        9 ->
            Ok 'J'

        10 ->
            Ok 'K'

        11 ->
            Ok 'L'

        12 ->
            Ok 'M'

        13 ->
            Ok 'N'

        14 ->
            Ok 'O'

        15 ->
            Ok 'P'

        16 ->
            Ok 'Q'

        17 ->
            Ok 'R'

        18 ->
            Ok 'S'

        19 ->
            Ok 'T'

        20 ->
            Ok 'U'

        21 ->
            Ok 'V'

        22 ->
            Ok 'W'

        23 ->
            Ok 'X'

        24 ->
            Ok 'Y'

        25 ->
            Ok 'Z'

        26 ->
            Ok '2'

        27 ->
            Ok '3'

        28 ->
            Ok '4'

        29 ->
            Ok '5'

        30 ->
            Ok '6'

        31 ->
            Ok '7'

        x ->
            Err
                ("Invalid byte value \""
                    ++ String.fromInt x
                    ++ "\" for base32"
                )
