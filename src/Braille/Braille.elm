module Braille.Braille exposing
    ( BoxIndex
    , BrailleBits
    , BrailleCell
    , BrailleCharacter
    , CellIndex
    , CharIndex
    , numberSign
    , updateCell
    , updateCharacter
    , updateMaybeCell
    , updateMaybeCharacter
    )

import Dict exposing (Dict)



-- Model


type alias BrailleBits =
    List Bool


type alias CharIndex =
    Int


type alias CellIndex =
    Int


type alias BoxIndex =
    Int


type alias BrailleCell =
    { one : Bool
    , two : Bool
    , three : Bool
    , four : Bool
    , five : Bool
    , six : Bool
    }


type alias BrailleCharacter =
    Dict CellIndex BrailleCell



-- Constants


numberSign =
    BrailleCell False False True True True True



-- Update


updateCell : BoxIndex -> Bool -> BrailleCell -> BrailleCell
updateCell bIndex newValue record =
    case bIndex of
        1 ->
            { record | one = newValue }

        2 ->
            { record | two = newValue }

        3 ->
            { record | three = newValue }

        4 ->
            { record | four = newValue }

        5 ->
            { record | five = newValue }

        6 ->
            { record | six = newValue }

        _ ->
            record


updateMaybeCell : BoxIndex -> Bool -> Maybe BrailleCell -> Maybe BrailleCell
updateMaybeCell bIndex newValue maybeCell =
    case maybeCell of
        Nothing ->
            Nothing

        Just cellRecord ->
            cellRecord
                |> updateCell bIndex newValue
                |> Just


updateCharacter : CellIndex -> BoxIndex -> Bool -> BrailleCharacter -> BrailleCharacter
updateCharacter ceIndex bIndex newValue cellDict =
    cellDict
        |> Dict.update ceIndex (updateMaybeCell bIndex newValue)


updateMaybeCharacter : CellIndex -> BoxIndex -> Bool -> Maybe BrailleCharacter -> Maybe BrailleCharacter
updateMaybeCharacter ceIndex bIndex newValue maybeCellDict =
    case maybeCellDict of
        Nothing ->
            Nothing

        Just cellDict ->
            cellDict
                |> updateCharacter ceIndex bIndex newValue
                |> Just
