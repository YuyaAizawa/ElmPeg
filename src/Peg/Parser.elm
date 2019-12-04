module Peg.Parser exposing
  ( Parser
  , parse
  , flatMap
  , return
  , fail
  , map
  , seq2
  , choice
  , option
  , zeroOrMore
  , oneOrMore
  , seq3
  , seq4
  , seq5
  , match
  , char
  , chars
  , ws
  , wss
  , int
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


{-| This `Parser` alway succeeds on parse and results in the given argument
without consumption.
-}
return : a -> Parser a
return a =
  Parser (\begin source ->
    Success begin a
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
  flatMap (return << f)


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


ws : Parser Char
ws =
  char (\c -> List.member c wsList)

wsList =
  [ '\u{0009}'
  , '\u{000B}'
  , '\u{000C}'
  , '\u{0020}'
  , '\u{00A0}'
  , '\u{FEFF}'
  ]


wss : Parser String
wss =
  chars (\c -> List.member c wsList)



int : Parser Int
int =
  seq2
  (match "-"
    |> option
    |> map (Maybe.withDefault ""))
  (chars Char.isDigit)
  (\s n -> s ++ n)
    |> flatMap (\str ->
      case String.toInt str of
        Just i -> return i
        Nothing -> fail)