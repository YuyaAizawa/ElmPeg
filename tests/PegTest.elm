module PegTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Array
import Peg.Parser exposing (..)

suite : Test
suite =
  describe "Peg.Parser"
    [
      describe "consume"
        [
        test "match" <|
          \_ ->
            let str = "ABCDEFG" in
            str
              |> consume
              |> parse str
              |> Expect.equal (Just ())

        , test "unmatch" <|
          \_ ->
            let str1 = "ABCDEFG" in
            let str2 = "ABCDEF" in
            consume str1
              |> parse str2
              |> Expect.equal Nothing
        ]
    ]
