module Base16 exposing (encode, decode)

{-| Library for base32 encoding and decoding according to RFC 4648.

@docs encode, decode

-}

import Coder


{-| Decode a base32 string into a list of bytes.
-}
decode : String -> Result String (List Int)
decode =
    Coder.decode ( 1, 2, "=", intToChar, charToInt )


{-| Encode a list of bytes into a base32 string.
-}
encode : List Int -> Result String String
encode =
    Coder.encode ( 1, 2, "=", intToChar, charToInt )


charToInt : Char -> Result String Int
charToInt char =
    case char of
        '0' ->
            Ok 0

        '1' ->
            Ok 1

        '2' ->
            Ok 2

        '3' ->
            Ok 3

        '4' ->
            Ok 4

        '5' ->
            Ok 5

        '6' ->
            Ok 6

        '7' ->
            Ok 7

        '8' ->
            Ok 8

        '9' ->
            Ok 9

        'A' ->
            Ok 10

        'B' ->
            Ok 11

        'C' ->
            Ok 12

        'D' ->
            Ok 13

        'E' ->
            Ok 14

        'F' ->
            Ok 15

        _ ->
            Err "Invalid character"


intToChar : Int -> Result String Char
intToChar int =
    case int of
        0 ->
            Ok '0'

        1 ->
            Ok '1'

        2 ->
            Ok '2'

        3 ->
            Ok '3'

        4 ->
            Ok '4'

        5 ->
            Ok '5'

        6 ->
            Ok '6'

        7 ->
            Ok '7'

        8 ->
            Ok '8'

        9 ->
            Ok '9'

        10 ->
            Ok 'A'

        11 ->
            Ok 'B'

        12 ->
            Ok 'C'

        13 ->
            Ok 'D'

        14 ->
            Ok 'E'

        15 ->
            Ok 'F'

        _ ->
            Err "Invalid byte value for base32"
