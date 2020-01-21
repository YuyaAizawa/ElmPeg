module JsonParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import JsonParser exposing (..)
import Dict
import Array

suite : Test
suite =
  describe "Peg.JsonParser"
    [ test "glossary" <|
      \_ ->
        parse glossaryStr
          |> Expect.equal (Just glossaryObj)
    ]

objectFromList =
  JsonObject << Dict.fromList

arrayFromList =
  JsonArray << Array.fromList

glossaryStr =
  "{\n" ++
  "    \"glossary\": {\n" ++
  "        \"title\": \"example glossary\",\n"++
  "    \"GlossDiv\": {\n"++
  "            \"title\": \"S\",\n"++
  "      \"GlossList\": {\n"++
  "                \"GlossEntry\": {\n"++
  "                    \"ID\": \"SGML\",\n"++
  "          \"SortAs\": \"SGML\",\n"++
  "          \"GlossTerm\": \"Standard Generalized Markup Language\",\n"++
  "          \"Acronym\": \"SGML\",\n"++
  "          \"Abbrev\": \"ISO 8879:1986\",\n"++
  "          \"GlossDef\": {\n"++
  "                        \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",\n"++
  "            \"GlossSeeAlso\": [\"GML\", \"XML\"]\n"++
  "                    },\n"++
  "          \"GlossSee\": \"markup\"\n"++
  "                }\n"++
  "            }\n"++
  "        }\n"++
  "    }\n"++
  "}\n"

glossaryObj =
  JsonObject <| Dict.fromList
    [ ( "glossary", objectFromList
      [ ( "title", JsonString "example glossary" )
      , ( "GlossDiv", objectFromList
        [ ( "title", JsonString "S")
        , ( "GlossList", objectFromList
          [ ( "GlossEntry", objectFromList
            [ ( "ID", JsonString "SGML" )
            , ( "SortAs", JsonString "SGML" )
            , ( "GlossTerm", JsonString "Standard Generalized Markup Language" )
            , ( "Acronym", JsonString "SGML" )
            , ( "Abbrev", JsonString "ISO 8879:1986" )
            , ( "GlossDef", objectFromList
              [ ( "para", JsonString "A meta-markup language, used to create markup languages such as DocBook." )
              , ( "GlossSeeAlso", arrayFromList
                  [ JsonString "GML"
                  , JsonString "XML"
                  ]
                )
              ]
              )
            , ( "GlossSee", JsonString "markup" )
            ]
            )
          ]
          )
        ]
        )
      ]
      )
    ]