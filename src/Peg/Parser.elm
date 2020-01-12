module Peg.Parser exposing
  ( Parser
  , parse
  , return
  , fail
  , match
  , char
  , chars
  , seq2
  , choice
  , option
  , zeroOrMore
  , oneOrMore
  , andPredicate
  , notPredicate
  , map
  , flatMap
  , seq3
  , seq4
  , seq5
  , seq6
  , intersperseSeq2
  , intersperseSeq3
  , intersperseSeq4
  , intersperseSeq5
  , intersperseSeq6
  )

{-| A parser combinator implementation for Persing Expression Grammer (PEG).

# Parse
@docs Parser, parse

# Parsers
return, fail, match, char, chars

# Basic Combinators
@docs seq2, choice, option, zeroOrMore, oneOrMore, andPredicate, notPredicate

# Transform
@docs map, flatMap

# Sequence Utils
@docs seq3, seq4, seq5, seq6, intersperseSeq2, intersperseSeq3, intersperseSeq4, intersperseSeq5, intersperseSeq6

-}

{-| A value decides wether the given input string should be accepted or not and
converts it into Elm object when accepted. -}
type Parser a
  = Parser (Int -> String -> ParseResult a)

type ParseResult a
  = Success Int a
  | Failed

{-| Parse the given string and return the result. It returns `Just` value when
the string is accepted, `Nothing` otherwise.

    parse "abc" (match "abc") == Just "abc"
    parse "xyz" (match "abc") == Nothing
-}
parse : String -> Parser a -> Maybe a
parse source (Parser p) =
  case p 0 source of
    Success end value ->
      if end == String.length source
      then Just value
      else Nothing

    Failed -> Nothing


{-| This `Parser` alway succeeds on parse and results in the given argument
without consumption.
-}
return : a -> Parser a
return a =
  Parser (\begin source ->
    Success begin a
  )


{-| This `Parser` always fails on parse. It is "mzero" on monad context.
-}
fail : Parser a
fail =
  Parser (\begin source ->
    Failed
  )


{-| Generate the parser accepts the specified string. The parser returns the same
string when accepts.

    parse "abc" (match "abc") == Just "abc"
    parse "xyz" (match "abc") == Nothing
-}
match : String -> Parser String
match str =
  Parser (\begin source ->
    let
      end = begin + String.length str
    in
      if String.slice begin end source == str
      then Success end str
      else Failed
  )


{-| Generate the parser accepts characters satisfied with the specified
predicator. The parser returns the specified character when accepted.

    char Char.isUpper |> parse "A"  == Just 'A'
    char Char.isUpper |> parse "a"  == Nothing
-}
char : (Char -> Bool) -> Parser Char
char predicate =
  Parser (\begin source ->
    let
      end = begin + 1
      target =
        source
          |> String.slice begin end
          |> String.toList
    in
      case target of
        [] -> Failed
        c :: tl ->
          if predicate c
          then Success end c
          else Failed
  )


{-| Generate the parser accepts consecutive characters satisfied with the
specified predicator. The parser returns the string when accepted.

    char Char.isUpper |> parse "AAA"  == Just "AAA"
    char Char.isUpper |> parse "aaa"  == Nothing
-}
chars : (Char -> Bool) -> Parser String
chars predicate =
  chars8 predicate
    |> oneOrMore
    |> map String.concat

chars8 predicate =
  Parser (\begin source ->
    let
      slice =
        source |>
          String.slice begin (begin + 8)

      help invList rest =
        case rest of
          [] ->
            invList

          hd :: tl ->
            if predicate hd
            then help (hd :: invList) tl
            else invList

      value =
        slice
        |> String.toList
        |> help []
        |> List.reverse
        |> String.fromList

      end =
        begin + String.length value
    in
      if end /= begin
      then Success end value
      else Failed
  )


{-| Incorporate specified parser to the parser. It re-parse the input using
specified parser with the original result when the original parser success
parse, otherwise the parse failed. Using it can set more stringent
conditions for parse success.

    intParser =
      chars Char.isDigit
        |> flatMap (\str ->
          case String.toInt str of
            Just i -> return i
            Nothing -> fail)
-}
flatMap : (a -> Parser b) -> Parser a -> Parser b
flatMap f (Parser p) =
  Parser (\begin source ->
    case p begin source of
      Failed ->
        Failed

      Success end v ->
        case f v of
          Parser b -> b end source
  )


{-| Generate new parser return mapped result by specifying mapper.
-}
map : (a -> b) -> Parser a -> Parser b
map f =
  flatMap (return << f)


{-| Concatenate two specified parsers, in other words, generate new parser
accepts the sequence. The result is also merged according to the 3rd parameter.

    seq2 (match "con") (match "cat") (++) |> parse "concat" == Just "concat"
-}
seq2 : Parser a -> Parser b -> (a -> b -> result) -> Parser result
seq2 pa pb f =
  pa
    |> flatMap (\va ->
      pb
        |> flatMap (\vb ->
          return (f va vb)))


or : (() -> Parser a) -> Parser a -> Parser a
or next (Parser this) =
  Parser (\begin source ->
    let
      result =
        this begin source
    in
      case result of
        Failed ->
          let (Parser next_) = next () in
          next_ begin source

        _ ->
          result
  )

{-| Generate new parser from specified parsers. result parser accepts all inputs
specified parsers accept. This combinator is ordered, in other words, if the
first parser accepts input, the second parser is ignored. These parsers needs
to be '() -> Parser' form. (It is useful for avoiding infinite reference.)

    p =
        choice
        [ () -> match "foo"
        , () -> match "bar"
        ]
    p |> parse "foo" == Just "foo"
    p |> parse "bar" == Just "bar"
-}
choice : List (() -> Parser a) -> Parser a
choice list =
  case list of
    [] ->
      fail

    hd :: tl ->
      List.foldl or (hd ()) tl


{-| Generate optional parser. It accepts whatever string and consumes only if
specified parser accepts. The parse result is `Maybe` value.

    option (match "foo") |> parse "bar" == Just Nothing
-}
option : Parser a -> Parser (Maybe a)
option =
    map Just >> or (\() -> return Nothing)


{-| Generate zero-or-more parser. It accept zero or more consecutive repetitions
of string specified parser accepts. it always behaves greedily, consuming as
much input as possible.

    p = zeroOrMore (match "a")
    p |> parse "aaa" == Just ["a", "a", "a"]
    p |> parse "" == Just []
-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore (Parser p) =
  let
    help list =
      \begin source ->
        case p begin source of
          Failed ->
            list
              |> List.reverse
              |> Success begin

          Success end v ->
            help (v :: list) end source
  in
    Parser (help [])


{-| Generate one-or-more parser. It accept one or more consecutive repetitions
of string specified parser accepts. it always behaves greedily, consuming as
much input as possible.

    p = oneOrMore (match "a")
    p |> parse "aaa" == Just ["a", "a", "a"]
    p |> parse "" == Nothing
-}
oneOrMore : Parser a -> Parser (List a)
oneOrMore p =
  seq2 p (zeroOrMore p) (\a b -> a :: b)


{-| Generate and-predicate parser. The parse succeeds if the specified parser
accepts the input and fails if the specified parser rejects, but in either case,
 never consumes any input.

    word = chars (always True)
    p = seq2 (andPredicate (match "A")) word (\_ i -> String.toInt i)
    p |> parse "Apple" == Just "Apple"
    p |> parse "Banana" == Nothing
-}
andPredicate : Parser a -> Parser ()
andPredicate (Parser p) =
  Parser (\begin source ->
    case p begin source of
      Success _ _ ->
        Success begin ()

      Failed ->
        Failed
  )


{-| Generate not-predicate parser. The parse succeeds if the specified parser
rejects the input and fails if the specified parser accepts, but in either case,
 never consumes any input.

    nums = chars Char.isNum
    p = seq2 (notPredicate (match "0")) nums (\_ i -> String.toInt i)
    p |> parse "1234" == Just 1234
    p |> parse "0123" == Nothing
-}
notPredicate : Parser a -> Parser ()
notPredicate (Parser p) =
  Parser (\begin source ->
    case p begin source of
      Success _ _ ->
        Failed

      Failed ->
        Success begin ()
  )


seq3 : Parser a -> Parser b -> Parser c
  -> (a -> b -> c -> result) -> Parser result
seq3 pa pb pc f =
  let post = seq2 pb pc in
  pa |> flatMap (post << f)


seq4 : Parser a -> Parser b -> Parser c -> Parser d
  -> (a -> b -> c -> d -> result) ->  Parser result
seq4 pa pb pc pd f =
  let post = seq3 pb pc pd in
  pa |> flatMap (post << f)


seq5 : Parser a -> Parser b -> Parser c -> Parser d -> Parser e
  -> (a -> b -> c -> d -> e -> result) ->  Parser result
seq5 pa pb pc pd pe f =
  let post = seq4 pb pc pd pe in
  pa |> flatMap (post << f)


seq6 : Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
  -> (a -> b -> c -> d -> e -> f -> result) ->  Parser result
seq6 pa pb pc pd pe pf f =
  let post = seq5 pb pc pd pe pf in
  pa |> flatMap (post << f)


seq7 pa pb pc pd pe pf pg f =
  let post = seq6 pb pc pd pe pf pg in
  pa |> flatMap (post << f)

seq8 pa pb pc pd pe pf pg ph f =
  let post = seq7 pb pc pd pe pf pg ph in
  pa |> flatMap (post << f)

seq9 pa pb pc pd pe pf pg ph pi f =
  let post = seq8 pb pc pd pe pf pg ph pi in
  pa |> flatMap (post << f)

seq10 pa pb pc pd pe pf pg ph pi pj f =
  let post = seq9 pb pc pd pe pf pg ph pi pj in
  pa |> flatMap (post << f)

seq11 pa pb pc pd pe pf pg ph pi pj pk f =
  let post = seq10 pb pc pd pe pf pg ph pi pj pk in
  pa |> flatMap (post << f)


{-| Concatenate two parsers with a specified parser in between.

    ws = match " " |> oneOrMore
    varTy = choise [ () -> match "int", () -> match "char" ]
    varName = char Char.alpha |> oneOrMore
    varDecl =
      intersperseSeq2
      ws varTy varName           -- matches like "int x" or "char  foo"
      (\ty name -> ( ty, name )) -- result like ("int", "x") or ("char", "foo")
-}
intersperseSeq2 : Parser i -> Parser a -> Parser b
  -> (a -> b -> result) -> Parser result
intersperseSeq2 pi pa pb fun =
  seq3
  pa pi pb
  (\a _ b -> fun a b)


intersperseSeq3 : Parser i -> Parser a -> Parser b -> Parser c
  -> (a -> b -> c -> result) -> Parser result
intersperseSeq3 pi pa pb pc fun =
  seq5
  pa pi pb pi pc
  (\a _ b _ c -> fun a b c)


intersperseSeq4 : Parser i -> Parser a -> Parser b -> Parser c -> Parser d
  -> (a -> b -> c -> d -> result) -> Parser result
intersperseSeq4 pi pa pb pc pd fun =
  seq7
  pa pi pb pi pc pi pd
  (\a _ b _ c _ d -> fun a b c d)


intersperseSeq5 : Parser i -> Parser a -> Parser b -> Parser c -> Parser d
  -> Parser e
  -> (a -> b -> c -> d -> e -> result) -> Parser result
intersperseSeq5 pi pa pb pc pd pe fun =
  seq9
  pa pi pb pi pc pi pd pi pe
  (\a _ b _ c _ d _ e -> fun a b c d e)


intersperseSeq6 : Parser i -> Parser a -> Parser b -> Parser c -> Parser d
  -> Parser e -> Parser f
  -> (a -> b -> c -> d -> e -> f -> result) -> Parser result
intersperseSeq6 pi pa pb pc pd pe pf fun =
  seq11
  pa pi pb pi pc pi pd pi pe pi pf
  (\a _ b _ c _ d _ e _ f -> fun a b c d e f)
