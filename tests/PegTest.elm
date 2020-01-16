module PegTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Peg.Parser exposing (..)

suite : Test
suite =
  describe "Peg.Parser"
  [ describe "match"
    [ test "ok" <|
      \_ ->
        let str = "abc" in
        match str
          |> parse str
          |> Expect.equal (Just str)

    , test "error" <|
      \_ ->
        let str1 = "abc" in
        let str2 = "xyz" in
        match str1
          |> parse str2
          |> Expect.equal Nothing
    ]
  , describe "char"
    [ test "ok" <|
      \_ ->
        char Char.isUpper
          |> parse "A"
          |> Expect.equal (Just 'A')

    , test "error" <|
      \_ ->
        char Char.isUpper
          |> parse "a"
          |> Expect.equal Nothing
    ]
  , describe "chars"
    [ test "ok" <|
      \_ ->
        chars Char.isUpper
          |> parse "AAA"
          |> Expect.equal (Just "AAA")

    , test "error" <|
      \_ ->
        chars Char.isUpper
          |> parse "aaa"
          |> Expect.equal Nothing
    ]
  , let
      nParser =
        chars Char.isDigit
          |> andThen (\str ->
            case String.toInt str of
              Just i -> return i
              Nothing -> fail)
    in
      describe "andThen"
      [ test "ok" <|
        \_ ->
          nParser
            |> parse "42"
            |> Expect.equal (Just 42)

      , test "error" <|
        \_ ->
          nParser
            |> parse "1+1"
            |> Expect.equal Nothing
      ]
  , describe "seq2"
    [ test "ok" <|
      \_ ->
        seq2
        (match "con") (match "cat")
        (++)
          |> parse "concat"
          |> Expect.equal (Just "concat")
    ]
  , let
      foobar =
        choice
        [ \() -> match "foo"
        , \() -> match "bar"
        ]
    in
      describe "choice"
      [ test "foo" <|
        \_ ->
          foobar
            |> parse "foo"
            |> Expect.equal (Just "foo")
      , test "bar" <|
        \_ ->
          foobar
            |> parse "bar"
            |> Expect.equal (Just "bar")
      , test "baz" <|
        \_ ->
          foobar
            |> parse "baz"
            |> Expect.equal Nothing
      ]
  , describe "option"
    [ test "Nothing" <|
      \_ ->
        option (match "foo")
          |> parse ""
          |> Expect.equal (Just Nothing)
    ]
  , let p = zeroOrMore (match "a") in
    describe "zeroOrMore"
      [ test "3" <|
        \_ ->
          p |> parse "aaa"
            |> Expect.equal (Just ["a", "a", "a"])
      , test "0" <|
        \_ ->
          p |> parse ""
            |> Expect.equal (Just [])
      ]
  , let p = oneOrMore (match "a") in
    describe "oneOrMore"
      [ test "3" <|
        \_ ->
          p |> parse "aaa"
            |> Expect.equal (Just ["a", "a", "a"])
      , test "0" <|
        \_ ->
          p |> parse ""
            |> Expect.equal Nothing
      ]
  , let
      word = chars (always True)
      p = seq2 (andPredicate (match "A")) word (\_ w -> w)
    in
      describe "andPredicate"
      [ test "ok" <|
        \_ ->
          p |> parse "Apple"
            |> Expect.equal (Just "Apple")
      , test "error" <|
        \_ ->
          p |> parse "Banana"
            |> Expect.equal Nothing
      ]
  , let
      nums = chars Char.isDigit
      p = seq2 (notPredicate (match "0")) nums (\_ i -> i)
    in
      describe "notPredicate"
      [ test "ok" <|
        \_ ->
          p |> parse "1234"
            |> Expect.equal (Just "1234")
      , test "error" <|
        \_ ->
          p |> parse "0123"
            |> Expect.equal Nothing
      ]
  , let
      ws = match " " |> oneOrMore
      varTy = choice [ \() -> match "int", \() -> match "char" ]
      varName = chars Char.isAlpha
      varDecl =
        intersperseSeq2
        ws varTy varName
        (\ty name -> ( ty, name ))
    in
      describe "intersperseSeq2"
      [ test "int" <|
        \_ ->
          varDecl
            |> parse "int x"
            |> Expect.equal (Just ( "int", "x" ))
      , test "error" <|
        \_ ->
          varDecl
            |> parse "char  foo"
            |> Expect.equal (Just ( "char", "foo" ))
      ]
  ]
