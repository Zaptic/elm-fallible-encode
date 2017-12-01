module Tests exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Encode
import Json.Encode.Fallible as F
import List.Nonempty as Nonempty exposing ((:::))
import Test exposing (..)


suite : Test
suite =
    describe "Fallible"
        [ describe "string"
            [ test "encodes string" <|
                \() ->
                    F.string "this"
                        |> Expect.equal (Ok (Json.Encode.string "this"))
            ]
        , describe "int"
            [ test "encodes int" <|
                \() ->
                    F.int 1
                        |> Expect.equal (Ok (Json.Encode.int 1))
            ]
        , describe "float"
            [ test "encodes float" <|
                \() ->
                    F.float 2.0
                        |> Expect.equal (Ok (Json.Encode.float 2.0))
            ]
        , describe "bool"
            [ test "encodes bool" <|
                \() ->
                    F.bool True
                        |> Expect.equal (Ok (Json.Encode.bool True))
            ]
        , describe "null"
            [ test "encodes null" <|
                \() ->
                    F.null
                        |> Expect.equal (Ok Json.Encode.null)
            ]
        , describe "list"
            [ test "encodes list with failure" <|
                \() ->
                    F.list [ F.string "this", F.fail "Invalid value" ]
                        |> Expect.equal (Err <| Nonempty.fromElement ( "[1]", "Invalid value" ))
            , test "encodes list with two failures" <|
                \() ->
                    F.list [ F.string "this", F.fail "Invalid value", F.fail "Second invalid value" ]
                        |> Expect.equal (Err <| ( "[1]", "Invalid value" ) ::: Nonempty.fromElement ( "[2]", "Second invalid value" ))
            , test "encodes list with successes" <|
                \() ->
                    F.list [ F.string "this" ]
                        |> Expect.equal (Ok (Json.Encode.list [ Json.Encode.string "this" ]))
            ]
        , describe "array"
            [ test "encodes array with failure" <|
                \() ->
                    F.array (Array.fromList [ F.string "this", F.fail "Invalid value" ])
                        |> Expect.equal (Err <| Nonempty.fromElement ( "[1]", "Invalid value" ))
            , test "encodes array with two failures" <|
                \() ->
                    F.array (Array.fromList [ F.string "this", F.fail "Invalid value", F.fail "Second invalid value" ])
                        |> Expect.equal (Err <| ( "[1]", "Invalid value" ) ::: Nonempty.fromElement ( "[2]", "Second invalid value" ))
            , test "encodes array with successes" <|
                \() ->
                    F.array (Array.fromList [ F.string "this" ])
                        |> Expect.equal (Ok (Json.Encode.list [ Json.Encode.string "this" ]))
            ]
        , describe "object"
            [ test "encodes object with failure" <|
                \() ->
                    F.object [ ( "a", F.string "this" ), ( "b", F.fail "Invalid value" ) ]
                        |> Expect.equal (Err <| Nonempty.fromElement ( ".b", "Invalid value" ))
            , test "encodes object with successes" <|
                \() ->
                    F.object [ ( "a", F.string "this" ), ( "b", F.string "that" ) ]
                        |> Expect.equal
                            (Ok
                                (Json.Encode.object
                                    [ ( "a", Json.Encode.string "this" )
                                    , ( "b", Json.Encode.string "that" )
                                    ]
                                )
                            )
            ]
        , describe "nested lists"
            [ test "encodes list nested in list with failure" <|
                \() ->
                    F.list
                        [ F.string "this"
                        , F.list [ F.string "that", F.string "other", F.fail "Invalid value" ]
                        ]
                        |> Expect.equal (Err <| Nonempty.fromElement ( "[1][2]", "Invalid value" ))
            ]
        , describe "nested object"
            [ test "encodes object nested in object with failure" <|
                \() ->
                    F.object
                        [ ( "a", F.string "this" )
                        , ( "b", F.object [ ( "c", F.fail "Invalid value" ) ] )
                        ]
                        |> Expect.equal (Err <| Nonempty.fromElement ( ".b.c", "Invalid value" ))
            , test "encodes object nested in list with failure" <|
                \() ->
                    F.list
                        [ F.string "this"
                        , F.object [ ( "c", F.fail "Invalid value" ) ]
                        ]
                        |> Expect.equal (Err <| Nonempty.fromElement ( "[1].c", "Invalid value" ))
            ]
        ]
