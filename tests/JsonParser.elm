module JsonParser exposing (parse, JsonValue(..))

{- implemented with reference to https://www.json.org/json-en.html -}

import Peg.Parser as Parser exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)


type JsonValue
  = JsonObject (Dict String JsonValue)
  | JsonArray (Array JsonValue)
  | JsonString String
  | JsonNumber Float
  | JsonBool Bool
  | JsonNull

parse : String -> Maybe JsonValue
parse src =
  pElement |> Parser.parse src


pValue : Parser JsonValue
pValue =
  choice
  [ \_ -> pObject
  , \_ -> pArray
  , \_ -> pString |> map JsonString
  , \_ -> pNumber
  , \_ -> (match "true" |> map (always (JsonBool True)))
  , \_ -> (match "false" |> map (always (JsonBool False)))
  , \_ -> (match "null" |> map (always JsonNull))
  ]

pObject : Parser JsonValue
pObject =
  seq3
  (match "{")
  (choice
    [ \_ -> pMembers
    , \_ -> (pWs |> map (always []))
    ])
  (match "}")
  (\_ ms _ -> JsonObject <| Dict.fromList ms)


pMembers : Parser (List ( String, JsonValue ))
pMembers =
  join (match ",") pMember

pMember : Parser ( String, JsonValue )
pMember =
  seq5
  pWs
  pString
  pWs
  (match ":")
  pElement
  (\_ s _ _ v -> ( s, v ))

pArray =
  seq3
  (match "[")
  (choice
    [ \_ -> pElements
    , \_ -> (pWs |> map (always []))
    ])
  (match "]")
  (\_ list _ -> list |> Array.fromList |> JsonArray)


pElements : Parser (List JsonValue)
pElements =
  join (match ",") pElement

pElement =
  seq3
  pWs
  pValue
  pWs
  (\_ e _ -> e)

pString : Parser String
pString =
  seq3
  (match "\"")
  pCharacters
  (match "\"")
  (\_ cs _ -> cs)

pCharacters =
  zeroOrMore pCharacter
    |> map String.fromList

pCharacter : Parser Char
pCharacter =
  let
    pNoescapeChar =
      char (\c ->
        '\u{0020}' <= c && c <= '\u{10FFFF}' &&
        c /= '"' &&
        c /= '\\')

    pEscapeChar =
      seq2
      (match "\\")
      pEscape
      (\_ c -> c)
  in
    choice
    [ \_ -> pNoescapeChar
    , \_ -> pEscapeChar
    ]

-- \b and \f are not supported
pEscape : Parser Char
pEscape =
  let
    pEscapeChar =
      choice
      [ \_ -> match "\"" |> map (\_ -> '"')
      , \_ -> match "\\" |> map (\_ -> '\\')
      , \_ -> match "/"  |> map (\_ -> '/')
      , \_ -> match "n"  |> map (\_ -> '\n')
      , \_ -> match "r"  |> map (\_ -> '\r')
      , \_ -> match "t"  |> map (\_ -> '\t')
      ]

    --pUnicode =
    --  seq5
    --  (match "u") pHex pHex pHex pHex
    --  (\_ h1 h2 h3 h4 ->
    --    [h1, h2, h3, h4]
    --      |> String.fromList
    --      |> String.toHexInt
    --      |> Maybe.withDefault -1
    --      |> Char.fromCode
    --  )
  in
    choice
    [ \() -> pEscapeChar
    --, \() -> pUnicode
    ]

pHex =
  char (\c ->
    '0' <= c && c <= '9' ||
    'a' <= c && c <= 'f' ||
    'A' <= c && c <= 'F')

pNumber : Parser JsonValue
pNumber =
  seq3
  pInterger
  (option pFraction)
  (option pExponent)
  (\i mf me -> i++(mf |> Maybe.withDefault "")++(me |> Maybe.withDefault ""))
    |> andThen (\str ->
      case str |> String.toFloat of
        Nothing -> fail
        Just f -> return (JsonNumber f))


pInterger : Parser String
pInterger =
  seq2
  (option (match "-"))
  (choice
    [ \() -> pDigit |> map List.singleton
    , \() -> seq2 pOneNine pDigits (\hd tl -> hd :: tl)
    ]
  )
  (\om ds ->
    case om of
      Nothing -> ds
      Just _ -> '-' :: ds
  )
  |> map String.fromList

pDigits : Parser (List Char)
pDigits =
  oneOrMore pDigit

pDigit =
  char (\c ->
    '0' <= c && c <= '9')

pOneNine =
  char (\c ->
    '1' <= c && c <= '9')

pFraction =
  seq2
  (match ".")
  pDigits
  (\_ ds -> "." ++ String.fromList ds)

pExponent =
  seq3
  (char (\c -> c == 'E' || c == 'e'))
  pSign
  pDigits
  (\_ s ds -> "E" ++ s ++ String.fromList ds)

pSign =
  choice
  [ \() -> (match "+")
  , \() -> (match "-")
  , \() -> (match "")
  ]

pWs =
  zeroOrMore (char (\c ->
    c == '\u{0020}' ||
    c == '\u{000A}' ||
    c == '\u{000D}' ||
    c == '\u{0009}'))