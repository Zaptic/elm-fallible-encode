module Json.Encode.Fallible exposing (..)

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


fail : String -> Result Error Json.Encode.Value
fail str =
    Err <| List.Nonempty.fromElement ( "", str )



-- Basics


string : String -> Result Error Json.Encode.Value
string value =
    Ok (Json.Encode.string value)


float : Float -> Result Error Json.Encode.Value
float value =
    Ok (Json.Encode.float value)


int : Int -> Result Error Json.Encode.Value
int value =
    Ok (Json.Encode.int value)


bool : Bool -> Result Error Json.Encode.Value
bool value =
    Ok (Json.Encode.bool value)


null : Result Error Json.Encode.Value
null =
    Ok Json.Encode.null


list : List (Result Error Json.Encode.Value) -> Result Error Json.Encode.Value
list l =
    let
        reducer : Result Error Json.Encode.Value -> ( Result Error (List Json.Encode.Value), Int ) -> ( Result Error (List Json.Encode.Value), Int )
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


array : Array (Result Error Json.Encode.Value) -> Result Error Json.Encode.Value
array arr =
    let
        reducer : Result Error Json.Encode.Value -> ( Result Error (Array Json.Encode.Value), Int ) -> ( Result Error (Array Json.Encode.Value), Int )
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


object : List ( String, Result Error Json.Encode.Value ) -> Result Error Json.Encode.Value
object obj =
    let
        reducer : ( String, Result Error Json.Encode.Value ) -> Result Error (List ( String, Json.Encode.Value )) -> Result Error (List ( String, Json.Encode.Value ))
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
