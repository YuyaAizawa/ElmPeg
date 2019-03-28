module Peg.Rule exposing
  ( Rule
  , evaluate
  , literal
  , sequence
  , choice
  )

import Array exposing (Array)



type Rule
  = Literal (List Char)
  | Sequence (List Rule)
  | Choice (List Rule)


evaluate : Array Char -> Int -> Rule -> Maybe Int
evaluate source start rule =
  case rule of
    Literal chars ->
      literalSupport chars start source

    Sequence rules ->
      sequenceSupport rules start source

    Choice rules ->
      choiceSupport rules start source

literalSupport chars index source =
  case chars of
    [] -> Just index
    c::rest ->
      if Array.get index source == Just c
      then literalSupport rest (index+1) source
      else Nothing

sequenceSupport list index source =
    case list of
      [] -> Just index
      r::rest ->
        case evaluate source index r of
          Nothing -> Nothing
          Just nextIndex -> sequenceSupport rest nextIndex source

choiceSupport list index source =
  case list of
    [] -> Nothing
    r::rest ->
      case evaluate source index r of
        Nothing -> choiceSupport list index source
        Just nextIndex -> Just nextIndex

literal : String -> Rule
literal str =
  Literal (String.toList str)

sequence : List Rule -> Rule
sequence rules =
  Sequence rules

choice : List Rule -> Rule
choice rules =
  Choice rules
