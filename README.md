# Elm Coder

An encoder and decoder for Base64, Base32 and Base16 (Hexadecimal).

# Example

```elm

import Base32 exposing (decode, encode)


encodeFooBar =
    ("foobar"
        |> String.toList
        |> List.map Char.toCode
        |> encode
    )
        == Ok "Zm9vYmFy"


decodeFooBar =
    ("Zm9vYmFy"
        |> decode
        |> Result.map (List.map Char.fromCode >> String.fromList)
    )
        == Ok "foobar"
```
