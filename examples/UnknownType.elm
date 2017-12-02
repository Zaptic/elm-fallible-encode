module UnknownType exposing (..)

import Html exposing (..)
import Json.Encode.Fallible as F


type Node
    = NodeA
    | NodeB
    | UnknownNode String


type alias NodeSet =
    { title : String, nodes : List Node }


data : NodeSet
data =
    { title = "Node Set"
    , nodes = [ NodeA, NodeB, UnknownNode "C", NodeA ]
    }


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


encodeNodeSet : NodeSet -> F.ValueResult
encodeNodeSet nodeSet =
    let
        encodeNode node =
            case node of
                NodeA ->
                    F.string "a"

                NodeB ->
                    F.string "b"

                UnknownNode name ->
                    F.fail ("Encountered unknown node: " ++ name)
    in
    F.object
        [ ( "title", F.string nodeSet.title )
        , ( "nodes", F.list <| List.map encodeNode nodeSet.nodes )
        ]
