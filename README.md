
# Elm Fallible Encode

This module provides a version of Json.Encode that allows you to trigger & record errors during the
encoding process.

This allows you to perform a certain amount of validation on your data structure during the encoding
pass, rather than handling it separately.


## Usage

The API is designed to identical to Json.Encode except that it includes a `fail` to record an error
and that the errors are propagated through the encoding. The encoding produces a Result type instead
of a raw Json.Encode.Value.


## Example

```elm
import Json.Encode.Falliable as Encode


{-| When compiled and viewed in the browser, this displays:

Failed with errors: Nonempty (".nodes[2]","Encountered unknown node: C") []

-}
main : Html msg
main =
    case encodeNodeSet data of
        Ok data ->
            text "Encoded successfully!"

        Err errors ->
            text ("Failed with errors: " ++ toString errors)


encodeNodeSet : NodeSet -> Encode.ValueResult
encodeNodeSet nodeSet =
    let
        encodeNode node =
            case node of
                NodeA ->
                    Encode.string "a"

                NodeB ->
                    Encode.string "b"

                UnknownNode name ->
                    Encode.fail ("Encountered unknown node: " ++ name)
    in
    Encode.object
        [ ( "title", Encode.string nodeSet.title )
        , ( "nodes", Encode.list <| List.map encodeNode nodeSet.nodes )
        ]
```

