module PegTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Array
import Peg.Parser exposing (..)

suite : Test
suite =
  describe "Peg.Parser"
    [
      describe "match"
        [
        test "ok" <|
          \_ ->
            let str = "ABCDEFG" in
            match str
              |> parse str
              |> Expect.equal (Just str)

        , test "error" <|
          \_ ->
            let str1 = "ABCDEFG" in
            let str2 = "ABCDEF" in
            match str1
              |> parse str2
              |> Expect.equal Nothing
        ]
    ]
