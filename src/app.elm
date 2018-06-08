module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { genomeCopy : Int, ctAmount : Int, cycleThresholdArray : Array Float, cycleCuttof : Int, positivePercentage : Float }


init : ( Model, Cmd Msg )
init =
    ( Model 0 0 Array.empty 40 0, Cmd.none )



-- update


type Msg
    = GCInputChange String
    | CTAmountChanged String
    | CycleThresholdInputChange Int String
    | CycleCuttofInputChange String
    | CycleThresholdArrayChanged (Array Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GCInputChange newSample ->
            ( { model | genomeCopy = convertToInt newSample }, Cmd.none )

        CTAmountChanged newAmt ->
            let
                amount =
                    convertToInt newAmt
            in
                ( { model | ctAmount = amount, cycleThresholdArray = Array.repeat amount 0 }, Cmd.none )

        CycleThresholdInputChange index newSample ->
            let
                cycleThresholdArray =
                    Array.set index (convertToFloat newSample) model.cycleThresholdArray

                positivePercentage =
                    calculatePositivePercentage cycleThresholdArray model.cycleCuttof
            in
                ( { model | cycleThresholdArray = cycleThresholdArray, positivePercentage = positivePercentage }, Cmd.none )

        CycleCuttofInputChange newCutoff ->
            let
                newCycleCuttoff =
                    convertToInt newCutoff

                positivePercentage =
                    calculatePositivePercentage model.cycleThresholdArray newCycleCuttoff
            in
                ( { model | cycleCuttof = newCycleCuttoff, positivePercentage = positivePercentage }, Cmd.none )

        CycleThresholdArrayChanged array ->
            ( { model | positivePercentage = calculatePositivePercentage model.cycleThresholdArray model.cycleCuttof }, Cmd.none )


convertToInt : String -> Int
convertToInt string =
    case String.toInt string of
        Err msg ->
            0

        Ok val ->
            val


convertToFloat : String -> Float
convertToFloat string =
    case String.toFloat string of
        Err msg ->
            0

        Ok val ->
            val


calculatePositivePercentage : Array Float -> Int -> Float
calculatePositivePercentage floatArray threshold =
    let
        testResults =
            Array.map (\s -> passesTest s threshold) floatArray

        positives =
            Array.filter (\r -> r == True) testResults
    in
        toFloat (Array.length positives) / toFloat (Array.length testResults)


passesTest : Float -> Int -> Bool
passesTest sample threshold =
    if sample < toFloat threshold then
        True
    else
        False



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [] [ text "GC number:" ]
            , input [ type_ "text", placeholder "GC number", onInput GCInputChange ] [ text (toString model.genomeCopy) ]
            ]
        , div []
            [ label [] [ text "ct amount:" ]
            , input [ type_ "text", placeholder "CT Amoutn", onInput CTAmountChanged ] [ text (toString model.ctAmount) ]
            ]
        , div []
            [ label [] [ text "Cycle Cuttoff:" ]
            , input [ type_ "text", placeholder "Cycle cuttoff", onInput CycleCuttofInputChange ] [ text (toString model.cycleCuttof) ]
            ]
        , div [] (generateTupleList model)
        , div [] [ (text (toString model.positivePercentage)) ]
        ]


generateTupleList : Model -> List (Html Msg)
generateTupleList model =
    Array.toList
        (Array.indexedMap
            (\i ct ->
                div []
                    [ label []
                        [ text ("Cycle Threshold " ++ toString i ++ ":") ]
                    , input [ type_ "text", placeholder "Cycle threshold", onInput (CycleThresholdInputChange i) ] [ text (toString (Array.get i model.cycleThresholdArray)) ]
                    ]
            )
            model.cycleThresholdArray
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
