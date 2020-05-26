module Braille.Decode exposing
    ( parseBrailleCharacter
    , parseBrailleLeter
    , parseBrailleNumber
    )

import Braille.Braille as Braille exposing (BrailleBits, BrailleCell, BrailleCharacter)
import Dict exposing (Dict)



-- Model


type alias MappingSequenceBuilder =
    List ( BrailleBits, Char -> Maybe Char )


type alias MappingSequence =
    List ( BrailleBits, Maybe Char )



-- Utils


{-| Attempts to interpert the current braille character
Currently suports a-z and 0-9 (when num sign present in first cell or character)
-}
parseBrailleCharacter : BrailleCharacter -> Maybe String
parseBrailleCharacter char =
    case Dict.values char of
        head :: tail :: [] ->
            case brailleRecordToList head of
                -- Braille sequence for # indicates next cell to be read as a number
                [ False, False, True, True, True, True ] ->
                    tail
                        |> parseBrailleNumber
                        |> Maybe.andThen (Just << String.fromInt)

                _ ->
                    Nothing

        head :: [] ->
            head
                |> parseBrailleLeter
                |> Maybe.andThen (Just << String.fromChar)

        _ ->
            Nothing


brailleSeq startChar cell seq =
    let
        startCode =
            startChar |> Char.toCode

        brailleSequence =
            buildBrailleSeq seq startChar
    in
    List.filter (Tuple.first >> (==) cell) brailleSequence
        |> List.head
        |> Maybe.andThen Tuple.second


{-| Attempts to interpert the current braille cell as a letter from a-z
-}
parseBrailleLeter : BrailleCell -> Maybe Char
parseBrailleLeter cell =
    let
        cellList =
            brailleRecordToList cell
    in
    case cellList of
        one :: two :: False :: four :: five :: False :: [] ->
            brailleSeq 'a' [ one, two, four, five ] standardSequence

        one :: two :: True :: four :: five :: False :: [] ->
            -- One of next 10 letters (k-t)
            brailleSeq 'k' [ one, two, four, five ] standardSequence

        False :: two :: False :: True :: True :: True :: [] ->
            -- Special case for 'w'
            Just 'w'

        one :: two :: True :: four :: five :: True :: [] ->
            -- One of next 6 letters (u - z, excluding w)
            brailleSeq 'u' [ one, two, four, five ] specialSequence

        _ ->
            Nothing


{-| Attempts to interpert the current braille cell as a number from 0-9
-}
parseBrailleNumber : BrailleCell -> Maybe Int
parseBrailleNumber cell =
    case brailleRecordToList cell of
        one :: two :: False :: four :: five :: False :: [] ->
            brailleSeq '1' [ one, two, four, five ] numberSequence
                |> Maybe.andThen (String.fromChar >> String.toInt)

        _ ->
            Nothing


maybeOffsetCharCode : Int -> Char -> Maybe Char
maybeOffsetCharCode offset =
    Just << Char.fromCode << (+) offset << Char.toCode


buildBrailleSeq : MappingSequenceBuilder -> Char -> MappingSequence
buildBrailleSeq sequence startChar =
    let
        offsetTranlator =
            Tuple.mapBoth identity (\offsetter -> startChar |> offsetter)
    in
    List.map offsetTranlator sequence


{-| Get sequential list of braille bits
-}
brailleRecordToList : BrailleCell -> List Bool
brailleRecordToList cell =
    List.map (\accessor -> cell |> accessor)
        [ .one, .two, .three, .four, .five, .six ]


{-| List mapping normal braille character sequence to some offset
from some starting point designated by a character
-}
standardSequence : MappingSequenceBuilder
standardSequence =
    List.map2 Tuple.pair
        standardSequenceKeys
        [ maybeOffsetCharCode 0
        , maybeOffsetCharCode 1
        , maybeOffsetCharCode 2
        , maybeOffsetCharCode 3
        , maybeOffsetCharCode 4
        , maybeOffsetCharCode 5
        , maybeOffsetCharCode 6
        , maybeOffsetCharCode 7
        , maybeOffsetCharCode 8
        , maybeOffsetCharCode 9
        ]


{-| List mapping normal braille charater sequence to the offsets needed to
get the correct numeric character
-}
numberSequence : MappingSequenceBuilder
numberSequence =
    List.map2 Tuple.pair
        standardSequenceKeys
        [ maybeOffsetCharCode 0
        , maybeOffsetCharCode 1
        , maybeOffsetCharCode 2
        , maybeOffsetCharCode 3
        , maybeOffsetCharCode 4
        , maybeOffsetCharCode 5
        , maybeOffsetCharCode 6
        , maybeOffsetCharCode 7
        , maybeOffsetCharCode 8
        , maybeOffsetCharCode -1
        ]


{-| Expression of special sequence used for letters [u, v, x, y, z].
'w' does not follow the normal pattern and requires special handling
so it is not to be included in the sequence
-}
specialSequence : MappingSequenceBuilder
specialSequence =
    List.map2 Tuple.pair
        specialSequenceKeys
        [ maybeOffsetCharCode 0
        , maybeOffsetCharCode 1
        , maybeOffsetCharCode 3
        , maybeOffsetCharCode 4
        , maybeOffsetCharCode 5
        ]


{-| Expression of the normal sequence of braille characters
Used for [a-j], [k-t] and [0-9]
-}
standardSequenceKeys : List BrailleBits
standardSequenceKeys =
    [ True :: False :: False :: False :: []
    , True :: True :: False :: False :: []
    , True :: False :: True :: False :: []
    , True :: False :: True :: True :: []
    , True :: False :: False :: True :: []
    , True :: True :: True :: False :: []
    , True :: True :: True :: True :: []
    , True :: True :: False :: True :: []
    , False :: True :: True :: False :: []
    , False :: True :: True :: True :: []
    ]


{-| Special sequence used in 5 of the last 6 alphabetical characters (w is special case)
-}
specialSequenceKeys : List BrailleBits
specialSequenceKeys =
    [ True :: False :: False :: False :: []
    , True :: True :: False :: False :: []
    , True :: False :: True :: False :: []
    , True :: False :: True :: True :: []
    , True :: False :: False :: True :: []
    ]
