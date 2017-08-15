module Coder exposing (..)

{-| The scheme ( octets, chars, padChar, intToChar, charToInt )
-}

import Bitwise exposing (shiftRightZfBy, shiftLeftBy, and, or)
import List exposing (map, head, tail)
import List.Extra exposing (groupsOf, getAt)
import Result exposing (andThen)
import Result.Extra exposing (combine)
import Maybe
import Tuple


type alias Scheme =
    ( Int, Int, String, Int -> Result String Char, Char -> Result String Int )


decode : Scheme -> String -> Result String (List Int)
decode ( octets, chars, padChar, intToChar, charToInt ) string =
    String.toList string
        |> groupsOf chars
        |> map (decodeChunk ( octets, chars, padChar, intToChar, charToInt ))
        |> combine
        |> andThen
            (\x ->
                let
                    output =
                        List.concat x

                    p =
                        (String.indexes padChar string |> List.length |> toFloat)
                            * (toFloat octets)
                            / (toFloat chars)
                            |> ceiling

                    s =
                        List.length output - p
                in
                    output
                        |> List.take s
                        |> Ok
            )


decodeChunk : Scheme -> List Char -> Result String (List Int)
decodeChunk ( octets, chars, padChar, intToChar, charToInt ) chunk =
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
                                    toFloat charIndex * ((toFloat bitsPerChar) / 8) |> floor

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


encode : Scheme -> List Int -> Result String String
encode ( octets, chars, padChar, intToChar, charToInt ) bytes =
    let
        n =
            toFloat (List.length bytes * chars) / toFloat octets |> ceiling

        p =
            if n % chars > 0 then
                chars - (n % chars)
            else
                0

        data =
            bytes
                ++ (List.repeat
                        (if (List.length bytes) % octets > 0 then
                            octets - ((List.length bytes) % octets)
                         else
                            0
                        )
                        0
                   )
    in
        (groupsOf octets data)
            |> map (encodeChunk ( octets, chars, padChar, intToChar, charToInt ))
            |> combine
            |> andThen
                (\x ->
                    -- String.join "" x |> Ok
                    (String.join "" x |> String.slice 0 n)
                        ++ (String.repeat p padChar)
                        |> Ok
                )


encodeChunk : Scheme -> List Int -> Result String String
encodeChunk ( octets, chars, padChar, intToChar, charToInt ) chunk =
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
                            (toFloat start / 8 |> floor)

                        b =
                            (toFloat (end - 1) / 8 |> floor)

                        aShift =
                            if start % 8 < (8 - bits) then
                                8 - end % 8
                            else
                                (8 - bits) - start % 8

                        bShift =
                            if a == b then
                                aShift
                            else
                                8 - end % 8

                        byte =
                            Maybe.map2
                                (\x y ->
                                    (or
                                        (if aShift < 0 then
                                            shiftLeftBy -aShift x
                                         else
                                            shiftRightZfBy aShift x
                                        )
                                        (shiftRightZfBy bShift y)
                                    )
                                        |> and (2 ^ bits - 1)
                                )
                                (getAt a chunk)
                                (getAt b chunk)
                                |> Maybe.withDefault 0
                    in
                        intToChar byte
                )
            |> combine
            |> andThen (\l -> String.fromList l |> Ok)
