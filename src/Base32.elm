module Base32 exposing (encode, decode)

{-| Library for base32 encoding and decoding according to RFC 4648.

@docs encode, decode

-}

import Coder


{-| Decode a base32 string into a list of bytes.
-}
decode : String -> Result String (List Int)
decode =
    Coder.decode ( 5, 8, "=", intToChar, charToInt )


{-| Encode a list of bytes into a base32 string.
-}
encode : List Int -> String
encode =
    Coder.encode ( 5, 8, "=", intToChar, charToInt )


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


intToChar : Int -> Char
intToChar int =
    case int of
        0 ->
            'A'

        1 ->
            'B'

        2 ->
            'C'

        3 ->
            'D'

        4 ->
            'E'

        5 ->
            'F'

        6 ->
            'G'

        7 ->
            'H'

        8 ->
            'I'

        9 ->
            'J'

        10 ->
            'K'

        11 ->
            'L'

        12 ->
            'M'

        13 ->
            'N'

        14 ->
            'O'

        15 ->
            'P'

        16 ->
            'Q'

        17 ->
            'R'

        18 ->
            'S'

        19 ->
            'T'

        20 ->
            'U'

        21 ->
            'V'

        22 ->
            'W'

        23 ->
            'X'

        24 ->
            'Y'

        25 ->
            'Z'

        26 ->
            '2'

        27 ->
            '3'

        28 ->
            '4'

        29 ->
            '5'

        30 ->
            '6'

        31 ->
            '7'

        x ->
            Debug.crash
                ("Invalid byte value \""
                    ++ (toString x)
                    ++ "\" for base32"
                )
                '💣'
