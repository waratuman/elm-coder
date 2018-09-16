module Base64 exposing (encode, decode, scheme)

{-| Library for base64 encoding and decoding according to RFC 4648.

@docs encode, decode, scheme

-}

import Coder exposing (Scheme)


{-| The decoding / encoding scheme for base 64.
-}
scheme : Scheme
scheme =
    { octets = 3
    , chars = 4
    , padChar = "="
    , intToChar = intToChar
    , charToInt = charToInt
    }


{-| Decode a base64 string into a list of bytes.
-}
decode : String -> Result String (List Int)
decode =
    Coder.decode scheme


{-| Encode a list of bytes into a base64 string.
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
            Ok 'a'

        27 ->
            Ok 'b'

        28 ->
            Ok 'c'

        29 ->
            Ok 'd'

        30 ->
            Ok 'e'

        31 ->
            Ok 'f'

        32 ->
            Ok 'g'

        33 ->
            Ok 'h'

        34 ->
            Ok 'i'

        35 ->
            Ok 'j'

        36 ->
            Ok 'k'

        37 ->
            Ok 'l'

        38 ->
            Ok 'm'

        39 ->
            Ok 'n'

        40 ->
            Ok 'o'

        41 ->
            Ok 'p'

        42 ->
            Ok 'q'

        43 ->
            Ok 'r'

        44 ->
            Ok 's'

        45 ->
            Ok 't'

        46 ->
            Ok 'u'

        47 ->
            Ok 'v'

        48 ->
            Ok 'w'

        49 ->
            Ok 'x'

        50 ->
            Ok 'y'

        51 ->
            Ok 'z'

        52 ->
            Ok '0'

        53 ->
            Ok '1'

        54 ->
            Ok '2'

        55 ->
            Ok '3'

        56 ->
            Ok '4'

        57 ->
            Ok '5'

        58 ->
            Ok '6'

        59 ->
            Ok '7'

        60 ->
            Ok '8'

        61 ->
            Ok '9'

        62 ->
            Ok '+'

        63 ->
            Ok '/'

        x ->
            Err
                ("Invalid byte value \""
                    ++ String.fromInt x
                    ++ "\" for base64"
                )
