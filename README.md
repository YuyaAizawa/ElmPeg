# PEG for Elm
A parser combinator implementation for [Parsing Expression Grammer](https://en.wikipedia.org/wiki/Parsing_expression_grammar) (PEG).

## Usage
1. Make base parser.
2. Combine parsers.
3. Use [`parse`](https://package.elm-lang.org/packages/YuyaAizawa/peg/latest/Parser#parse) function to get result.

```elm

-- 1. Make base parser.
numbers =
  chars Char.isDigit

minus =
  match "-"
    |> option
    |> map (Maybe.withDefalut "")


-- 2. Combine parsers.
intParser =
  seq2
  minus numbers
  (\minusStr numberStr -> minusStr ++ numberStr)
    |> flatMap (\str ->
      case String.toInt str of
        Just i -> return i
        Nothing -> fail)

-- 3. Use `parse` function to get result.
result =
  intParser |> parse "42" -- Just 42
```

## Example
We will present mini-C parser example. COMING SOON!

## License
BSD-3-Clause. see LISENCE file.
