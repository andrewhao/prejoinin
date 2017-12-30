module Page.SheetTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Page.Sheet exposing (sortByPosition)


suite : Test
suite =
    describe "Page.Sheet"
        [ describe "sortByPosition"
            [ test "sorts by position prop" <|
                \_ ->
                    let
                        sortables =
                            [ { position = 2 }, { position = 1 } ]
                    in
                        Page.Sheet.sortByPosition (sortables)
                            |> Expect.equal ([ { position = 1 }, { position = 2 } ])
            ]
        ]
