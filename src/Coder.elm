module Coder exposing
    ( Scheme
    , decode
    , decodeChunk
    , encode
    , encodeChunk
    )

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)
import List exposing (head, map, tail)
import List.Extra exposing (getAt, groupsOf)
import Maybe
import Result exposing (andThen)
import Result.Extra as Result exposing (combine)
import Tuple


{-| The decoding / encoding scheme.
-}
type alias Scheme =
    { octets : Int
    , chars : Int
    , padChar : String
    , intToChar : Int -> Result String Char
    , charToInt : Char -> Result String Int
    }


{-| Decode the given string according to the give scheme. The result is a list
of integers ranging from 0 - 254
-}
decode : Scheme -> String -> Result String (List Int)
decode ({ octets, chars, padChar, intToChar, charToInt } as scheme) string =
    String.toList string
        |> groupsOf chars
        |> map (decodeChunk scheme)
        |> combine
        |> andThen
            (\x ->
                let
                    output =
                        List.concat x

                    p =
                        (String.indexes padChar string |> List.length |> toFloat)
                            * toFloat octets
                            / toFloat chars
                            |> ceiling

                    s =
                        List.length output - p
                in
                output
                    |> List.take s
                    |> Ok
            )


decodeChunk : Scheme -> List Char -> Result String (List Int)
decodeChunk ({ octets, chars, padChar, intToChar, charToInt } as scheme) chunk =
    let
        bitsPerChar =
            toFloat (octets * 8) / toFloat chars |> floor
    in
    chunk
        |> List.map charToInt
        |> combine
        |> andThen
            (\x ->
                List.Extra.indexedFoldl
                    (\charIndex char ( currentByte, bytes ) ->
                        let
                            octetIndex =
                                toFloat charIndex * (toFloat bitsPerChar / 8) |> floor

                            octetStart =
                                octetIndex * 8

                            octetEnd =
                                octetStart + 8

                            charStart =
                                charIndex * bitsPerChar

                            charEnd =
                                charStart + bitsPerChar

                            shift =
                                octetEnd - charEnd

                            byte =
                                or currentByte
                                    (if shift < 0 then
                                        shiftRightZfBy -shift char

                                     else
                                        shiftLeftBy shift char
                                    )

                            nextByte =
                                if charEnd == octetEnd then
                                    Just 0

                                else if charEnd > octetEnd then
                                    Just <|
                                        and 0xFF
                                            (if shift < 0 then
                                                shiftLeftBy (8 + shift) char

                                             else
                                                shiftRightZfBy (8 - shift) char
                                            )

                                else
                                    Nothing
                        in
                        case nextByte of
                            Just b ->
                                ( b, bytes ++ [ byte ] )

                            Nothing ->
                                ( byte, bytes )
                    )
                    ( 0, [] )
                    x
                    |> Tuple.second
                    |> Ok
            )


{-| Ecode the given list of integers according to the give scheme. The result is
a string
-}
encode : Scheme -> List Int -> Result String String
encode ({ octets, chars, padChar, intToChar, charToInt } as scheme) bytes =
    let
        n =
            toFloat (List.length bytes * chars) / toFloat octets |> ceiling

        p =
            if modBy chars n > 0 then
                chars - modBy chars n

            else
                0

        data =
            bytes
                ++ List.repeat
                    (if modBy octets (List.length bytes) > 0 then
                        octets - modBy octets (List.length bytes)

                     else
                        0
                    )
                    0
    in
    groupsOf octets data
        |> map (encodeChunk scheme)
        |> combine
        |> Result.map
            (String.join ""
                >> String.slice 0 n
                >> (\x -> String.append x (String.repeat p padChar))
            )


{-| Ecode the given list of integers according to the give scheme. The result is
a string.

Note that the list of integers must all be within the range 0 - 254! If any of
the integers are greater than 254 the program will never terminate.

-}
unsafeEncode : Scheme -> List Int -> String
unsafeEncode ({ octets, chars, padChar, intToChar, charToInt } as scheme) bytes =
    let
        n =
            toFloat (List.length bytes * chars) / toFloat octets |> ceiling

        p =
            if modBy chars n > 0 then
                chars - modBy chars n

            else
                0

        data =
            bytes
                ++ List.repeat
                    (if modBy octets (List.length bytes) > 0 then
                        octets - modBy octets (List.length bytes)

                     else
                        0
                    )
                    0
    in
    groupsOf octets data
        |> map (encodeChunk scheme)
        |> combine
        |> Result.map
            (String.join ""
                >> String.slice 0 n
                >> (\x -> String.append x (String.repeat p padChar))
            )
        -- If this ever gets called with a number over 254, it will never
        -- terminate! If that happens, use the safe method `encode` or debug
        -- further by uncommenting this:
        --
        -- Debug.todo ("Tried to encode " ++ (Debug.toString bytes))
        |> Result.withDefault (unsafeEncode scheme bytes)


encodeChunk : Scheme -> List Int -> Result String String
encodeChunk ({ octets, chars, padChar, intToChar, charToInt } as scheme) chunk =
    let
        bits =
            toFloat (octets * 8) / toFloat chars |> floor
    in
    List.range 0 (chars - 1)
        |> List.map
            (\i ->
                let
                    start =
                        i * bits

                    end =
                        start + bits

                    a =
                        toFloat start / 8 |> floor

                    b =
                        toFloat (end - 1) / 8 |> floor

                    aShift =
                        if modBy 8 start < (8 - bits) then
                            8 - modBy 8 end

                        else
                            (8 - bits) - modBy 8 start

                    bShift =
                        if a == b then
                            aShift

                        else
                            8 - modBy 8 end

                    byte =
                        Maybe.map2
                            (\x y ->
                                or
                                    (if aShift < 0 then
                                        shiftLeftBy -aShift x

                                     else
                                        shiftRightZfBy aShift x
                                    )
                                    (shiftRightZfBy bShift y)
                                    |> and (2 ^ bits - 1)
                            )
                            (getAt a chunk)
                            (getAt b chunk)
                            |> Maybe.withDefault 0
                in
                intToChar byte
            )
        |> combine
        |> Result.map String.fromList
