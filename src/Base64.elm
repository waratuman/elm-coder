module Base64 exposing (encode, decode)

{-| Library for base64 encoding and decoding according to RFC 4648.

@docs encode, decode

-}

import Coder


{-| Decode a base64 string into a list of bytes.
-}
decode : String -> Result String (List Int)
decode =
    Coder.decode ( 3, 4, "=", intToChar, charToInt )


{-| Encode a list of bytes into a base64 string.
-}
encode : List Int -> String
encode =
    Coder.encode ( 3, 4, "=", intToChar, charToInt )


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

        'a' ->
            Ok 26

        'b' ->
            Ok 27

        'c' ->
            Ok 28

        'd' ->
            Ok 29

        'e' ->
            Ok 30

        'f' ->
            Ok 31

        'g' ->
            Ok 32

        'h' ->
            Ok 33

        'i' ->
            Ok 34

        'j' ->
            Ok 35

        'k' ->
            Ok 36

        'l' ->
            Ok 37

        'm' ->
            Ok 38

        'n' ->
            Ok 39

        'o' ->
            Ok 40

        'p' ->
            Ok 41

        'q' ->
            Ok 42

        'r' ->
            Ok 43

        's' ->
            Ok 44

        't' ->
            Ok 45

        'u' ->
            Ok 46

        'v' ->
            Ok 47

        'w' ->
            Ok 48

        'x' ->
            Ok 49

        'y' ->
            Ok 50

        'z' ->
            Ok 51

        '0' ->
            Ok 52

        '1' ->
            Ok 53

        '2' ->
            Ok 54

        '3' ->
            Ok 55

        '4' ->
            Ok 56

        '5' ->
            Ok 57

        '6' ->
            Ok 58

        '7' ->
            Ok 59

        '8' ->
            Ok 60

        '9' ->
            Ok 61

        '+' ->
            Ok 62

        '/' ->
            Ok 63

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
            'a'

        27 ->
            'b'

        28 ->
            'c'

        29 ->
            'd'

        30 ->
            'e'

        31 ->
            'f'

        32 ->
            'g'

        33 ->
            'h'

        34 ->
            'i'

        35 ->
            'j'

        36 ->
            'k'

        37 ->
            'l'

        38 ->
            'm'

        39 ->
            'n'

        40 ->
            'o'

        41 ->
            'p'

        42 ->
            'q'

        43 ->
            'r'

        44 ->
            's'

        45 ->
            't'

        46 ->
            'u'

        47 ->
            'v'

        48 ->
            'w'

        49 ->
            'x'

        50 ->
            'y'

        51 ->
            'z'

        52 ->
            '0'

        53 ->
            '1'

        54 ->
            '2'

        55 ->
            '3'

        56 ->
            '4'

        57 ->
            '5'

        58 ->
            '6'

        59 ->
            '7'

        60 ->
            '8'

        61 ->
            '9'

        62 ->
            '+'

        63 ->
            '/'

        x ->
            Debug.crash
                ("Invalid byte value \""
                    ++ (toString x)
                    ++ "\" for base64"
                )
                '💣'
