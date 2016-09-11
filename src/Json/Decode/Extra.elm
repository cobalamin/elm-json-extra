module Json.Decode.Extra exposing (sequence)

{-| This library provides functions for use cases of Json.Decode/Json.Encode that are non-obvious to implement with the primitives these modules expose.

# Lists of Decoders
@docs sequence
-}

import Json.Decode as Decode
import Json.Decode exposing (Decoder)


{-| This function turns a list of decoders into a decoder that returns a list.

The returned decoder will zip the list of decoders with a list of values, matching each decoder with exactly one value at the same position. This is most often useful in cases where `Json.Decode.oneOf`, which will try every decoder for every value in the list, would be too lenient.

Note that this function, unlike `List.map2`'s behaviour, expects the list of decoders to have the same length as the list of values in the JSON.

    type FloatOrInt
        = I Int
        | F Float

    -- we'd like a list like [I, F, I] from this
    -- fairly contrived example, but data like this does exist!
    json = "[1, 2.0, 3]"

    intDecoder = Decode.map I Decode.int
    floatDecoder = Decode.map F Decode.float

    decoder : Decoder (List FloatOrInt)
    decoder =
        sequence [ intDecoder, floatDecoder, intDecoder ]

    decoded = Decode.decodeString decoder json
    -- Ok ([I 1,F 2,I 3]) : Result String (List FloatOrInt)

-}
sequence : List (Decoder a) -> Decoder (List a)
sequence decoders =
    Decode.customDecoder
        (Decode.list Decode.value)
        (\jsonValues ->
            if List.length jsonValues /= List.length decoders then
                Err "Number of decoders does not match number of values"
            else
                List.map2 Decode.decodeValue decoders jsonValues
                    |> List.foldr (Result.map2 (::)) (Ok [])
        )
