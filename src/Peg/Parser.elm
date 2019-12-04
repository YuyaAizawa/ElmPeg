module Peg.Parser exposing
  ( Parser
  , parse
  , return
  , flatMap
  , fail
  , map
  , seq2
  , choice
  , option
  , zeroOrMore
  , oneOrMore
  , seq3
  , seq4
  , consume
  , char
  , chars
  )

type ParseResult a
  = Success Int a
  | Failed

type Parser a
  = Parser (Int -> String -> ParseResult a)

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


{-| This `Parser` alway fails on parse, in other words "mzero".
-}
fail : Parser a
fail =
  Parser (\begin source ->
    Failed
  )


map : (a -> b) -> Parser a -> Parser b
map f =
  flatMap (f >> return)


seq2 : Parser a -> Parser b -> (a -> b -> result) -> Parser result
seq2 pa (Parser pb) f =
  pa
    |> flatMap (\va -> Parser (\begin source ->
      case pb begin source of
        Success end vb ->
          Success end (f va vb)

        Failed ->
          Failed
    ))


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

choice : List (() -> Parser a) -> Parser a
choice list =
  case list of
    [] ->
      fail

    hd :: tl ->
      List.foldl or (hd ()) tl


option : Parser a -> Parser (Maybe a)
option =
    map Just >> or (\() -> return Nothing)


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


oneOrMore : Parser a -> Parser (List a)
oneOrMore p =
  seq2 p (zeroOrMore p) (\a b -> a :: b)


andPredicate : Parser a -> Parser ()
andPredicate (Parser p) =
  Parser (\begin source ->
    case p begin source of
      Success _ _ ->
        Success begin ()

      Failed ->
        Failed
  )


notPredicate : Parser a -> Parser ()
notPredicate (Parser p) =
  Parser (\begin source ->
    case p begin source of
      Success _ _ -> Failed
      Failed -> Success begin ()
  )

seq3 : Parser a -> Parser b -> Parser c
  -> (a -> b -> c -> result) -> Parser result
seq3 pa pb pc f =
  pa
    |> flatMap (\va ->
      seq2 pb pc (f va)
    )

seq4 : Parser a -> Parser b -> Parser c -> Parser d
  -> (a -> b -> c -> d -> result) ->  Parser result
seq4 pa pb pc pd f =
  pa
    |> flatMap (\va ->
      seq3 pb pc pd (f va)
    )

consume : String -> Parser ()
consume str =
  Parser (\begin source ->
    let
      end =
        begin + String.length str
    in
      if String.slice begin end source == str
      then Success end ()
      else Failed
  )


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
        [] -> Failed -- never happen
        c :: tl ->
          if predicate c
          then Success end c
          else Failed
  )


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