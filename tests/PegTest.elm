module PegTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Array
import Peg.Rule as Rule

suite : Test
suite =
  describe "Peg.Rule"
    [
      describe "literal"
        [
        test "match" <|
          \_ ->
            let str = "ABCDEFG" in
            str
              |> Rule.literal
              |> Rule.evaluate (str |> String.toList |> Array.fromList) 0
              |> Expect.equal (str |> String.length |> Just)

        , test "unmatch" <|
          \_ ->
            let str1 = "ABCDEFG" in
            let str2 = "ABCDEF" in
            str1
              |> Rule.literal
              |> Rule.evaluate (str2 |> String.toList |> Array.fromList) 0
              |> Expect.equal Nothing
        ]
    ]
