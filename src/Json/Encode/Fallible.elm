module Json.Encode.Fallible
    exposing
        ( Error
        , ValueResult
        , array
        , bool
        , fail
        , float
        , int
        , list
        , null
        , object
        , string
        )

import Array exposing (Array)
import Json.Encode
import List.Nonempty exposing ((:::), Nonempty)


type alias Path =
    String


type alias Message =
    String


type alias Error =
    Nonempty ( Path, Message )


type alias ValueResult =
    Result Error Json.Encode.Value


{-| Record an encoding failure. If you detect that the data is not something
you can encode then you can use this to trigger & record an error in the
encoding process

    case node of
        NodeA ->
            F.string "a"

        NodeB ->
            F.string "b"

        UnknownNode name ->
            F.fail ("Encountered unknown node: " ++ name)

-}
fail : String -> ValueResult
fail str =
    Err <| List.Nonempty.fromElement ( "", str )



-- Basics


{-| Encodes a string to a ValueResult
-}
string : String -> ValueResult
string value =
    Ok (Json.Encode.string value)


{-| Encodes a float to a ValueResult
-}
float : Float -> ValueResult
float value =
    Ok (Json.Encode.float value)


{-| Encodes an int to a ValueResult
-}
int : Int -> ValueResult
int value =
    Ok (Json.Encode.int value)


{-| Encodes a bool to a ValueResult
-}
bool : Bool -> ValueResult
bool value =
    Ok (Json.Encode.bool value)


{-| Creates a ValueResult for 'null'
-}
null : ValueResult
null =
    Ok Json.Encode.null


{-| Encodes a list of ValueResults into a single ValueResult representing a
Json list. If any errors are encountered then the result is a non-empty list of
all the errors.

The index is added to the error's path to help you understand where it was
encountered.

-}
list : List ValueResult -> ValueResult
list l =
    let
        reducer : ValueResult -> ( Result Error (List Json.Encode.Value), Int ) -> ( Result Error (List Json.Encode.Value), Int )
        reducer value ( acc, index ) =
            let
                prefix =
                    String.concat [ "[", toString index, "]" ]
            in
            case ( value, acc ) of
                ( Ok v, Ok a ) ->
                    ( Ok (v :: a), index - 1 )

                ( Ok v, Err a ) ->
                    ( Err a, index - 1 )

                ( Err v, Ok a ) ->
                    ( Err (addPrefix prefix v), index - 1 )

                ( Err v, Err a ) ->
                    ( Err (List.Nonempty.append (addPrefix prefix v) a), index - 1 )
    in
    List.foldr reducer ( Ok [], List.length l - 1 ) l
        |> Tuple.first
        |> Result.map Json.Encode.list


{-| Encodes an array of ValueResults into a single ValueResult representing a
Json list. If any errors are encountered then the result is a non-empty list of
all the errors.

The index is added to the error's path to help you understand where it was
encountered.

-}
array : Array ValueResult -> ValueResult
array arr =
    let
        reducer : ValueResult -> ( Result Error (Array Json.Encode.Value), Int ) -> ( Result Error (Array Json.Encode.Value), Int )
        reducer value ( acc, index ) =
            let
                prefix =
                    String.concat [ "[", toString index, "]" ]
            in
            case ( value, acc ) of
                ( Ok v, Ok a ) ->
                    ( Ok (Array.push v a), index - 1 )

                ( Ok v, Err a ) ->
                    ( Err a, index - 1 )

                ( Err v, Ok a ) ->
                    ( Err (addPrefix prefix v), index - 1 )

                ( Err v, Err a ) ->
                    ( Err (List.Nonempty.append (addPrefix prefix v) a), index - 1 )
    in
    Array.foldr reducer ( Ok Array.empty, Array.length arr - 1 ) arr
        |> Tuple.first
        |> Result.map Json.Encode.array


{-| Encodes a list of key-value pairs, represented as (String, ValueResult)
into a single ValueResult representing a Json object. If any errors are
encountered then the result is a non-empty list of all the errors.

The key is added to the error's path to help you understand where it was
encountered.

-}
object : List ( String, ValueResult ) -> ValueResult
object obj =
    let
        reducer : ( String, ValueResult ) -> Result Error (List ( String, Json.Encode.Value )) -> Result Error (List ( String, Json.Encode.Value ))
        reducer ( key, value ) acc =
            let
                prefix =
                    String.concat [ ".", key ]
            in
            case ( value, acc ) of
                ( Ok v, Ok a ) ->
                    Ok (( key, v ) :: a)

                ( Ok v, Err a ) ->
                    Err a

                ( Err v, Ok a ) ->
                    Err (addPrefix prefix v)

                ( Err v, Err a ) ->
                    Err (List.Nonempty.append (addPrefix prefix v) a)
    in
    obj
        |> List.foldr reducer (Ok [])
        |> Result.map Json.Encode.object



-- Helpers


addPrefix : String -> Error -> Error
addPrefix prefix =
    List.Nonempty.map (\( path, message ) -> ( prefix ++ path, message ))
