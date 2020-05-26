module Main exposing (main)

import Array exposing (Array)
import Braille.Braille as Braille
    exposing
        ( BoxIndex
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
import Braille.Decode as Braille exposing (parseBrailleCharacter)
import Browser
import Dict exposing (Dict)
import Element exposing (Element, centerX, centerY, fill, height, px, text, width)
import Element.Input as Input exposing (checkbox)
import Html exposing (Html)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init _ =
    ( listToIndexedDict
        [ listToIndexedDict
            [ numberSign
            , BrailleCell True False False True False False
            ]
        , listToIndexedDict
            [ BrailleCell False True False True False False
            ]
        ]
    , Cmd.none
    )


listToIndexedDict =
    Dict.fromList << List.indexedMap Tuple.pair



-- Msg


type Msg
    = NoOp
    | UpdateCell CharIndex CellIndex BoxIndex Bool



-- Model


type alias Model =
    Dict CharIndex BrailleCharacter


type alias ViewModel =
    List BrailleCharacter


type alias Flags =
    ()



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCell chIndex ceIndex bIndex newValue ->
            ( Dict.update chIndex (updateMaybeCharacter ceIndex bIndex newValue) model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view =
    viewSelector >> renderView


viewSelector : Model -> ViewModel
viewSelector model =
    Dict.values model


renderView : ViewModel -> Html Msg
renderView model =
    Element.layout [ width fill, height fill ] (brailleCharacters model)


brailleCharacters : List BrailleCharacter -> Element Msg
brailleCharacters characters =
    Element.column
        [ width fill
        , height fill
        , Element.spacing 24
        ]
        (List.indexedMap brailleCharacter characters)


brailleCharacter : CharIndex -> BrailleCharacter -> Element Msg
brailleCharacter cIndex bChar =
    Element.row
        [ centerX
        , centerY
        , Element.above
            (Element.el [ centerX, centerY ]
                (parseBrailleCharacter bChar |> Maybe.withDefault "" |> text)
            )
        ]
        (List.indexedMap (brailleCell cIndex) (Dict.values bChar))


brailleCell : CharIndex -> CellIndex -> BrailleCell -> Element Msg
brailleCell chIndex ceIndex cell =
    Element.row
        [ width (50 |> px)
        , height (75 |> px)
        , centerX
        , centerY
        ]
        [ brailleInputCol chIndex ceIndex [ ( 1, cell.one ), ( 2, cell.two ), ( 3, cell.three ) ]
        , brailleInputCol chIndex ceIndex [ ( 4, cell.four ), ( 5, cell.five ), ( 6, cell.six ) ]
        ]


brailleInputCol : CharIndex -> CellIndex -> List ( Int, Bool ) -> Element Msg
brailleInputCol chIndex ceIndex currentValues =
    Element.column [ width fill, height fill ]
        (List.map (brailleCheckbox chIndex ceIndex) currentValues)


brailleCheckbox : CharIndex -> CellIndex -> ( Int, Bool ) -> Element Msg
brailleCheckbox chIndex ceIndex ( bIndex, checked ) =
    checkbox [ width fill, height fill ]
        { onChange = UpdateCell chIndex ceIndex bIndex
        , icon = Input.defaultCheckbox
        , checked = checked
        , label = Input.labelHidden "brailleInput"
        }
