module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)
import Random exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { genomeCopy : Int
    , ctAmount : Int
    , cycleThresholdArray : Array Float
    , bootstrappedData : Array Float
    , cycleCuttof : Int
    , positivePercentage : Float
    , bootstrapAmount : Int
    , randomIndexList : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 0 Array.empty Array.empty 40 0 100 [], Cmd.none )



-- update


type Msg
    = GCInputChange String
    | CTAmountChanged String
    | CycleThresholdInputChange Int String
    | CycleCuttofInputChange String
    | CycleThresholdArrayChanged (Array Float)
    | BootstrapValueChange String
    | NewRandomList (List Int)


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

                bootstrapData =
                    generate

                positivePercentage =
                    calculatePositivePercentage cycleThresholdArray model.cycleCuttof
            in
                ( { model | cycleThresholdArray = cycleThresholdArray, positivePercentage = positivePercentage }, Cmd.none )

        CycleCuttofInputChange newCutoff ->
            let
                newCycleCuttoff =
                    convertToInt newCutoff

                positivePercentage =
                    calculatePositivePercentage model.bootstrappedData newCycleCuttoff
            in
                ( { model | cycleCuttof = newCycleCuttoff, positivePercentage = positivePercentage }, Cmd.none )

        CycleThresholdArrayChanged array ->
            ( model, Cmd.none )

        BootstrapValueChange newBootstrapValueString ->
            let
                newAmount =
                    convertToInt newBootstrapValueString

                maxIndex =
                    ((Array.length model.cycleThresholdArray) - 1)
            in
                ( { model | bootstrapAmount = newAmount }, Random.generate NewRandomList (generateRandomIndexList newAmount maxIndex) )

        NewRandomList list ->
            let
                bootstrapData =
                    (generateBootstrap list model.cycleThresholdArray)

                positivePercentage =
                    calculatePositivePercentage bootstrapData model.cycleCuttof
            in
                ( { model | bootstrappedData = bootstrapData, positivePercentage = positivePercentage }, Cmd.none )


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


generateRandomIndexList : Int -> Int -> Random.Generator (List Int)
generateRandomIndexList len maxNumber =
    Random.list len (Random.int 0 maxNumber)


generateBootstrap : List Int -> Array Float -> Array Float
generateBootstrap list floatArray =
    Array.fromList (List.map (\i -> getArrayElem i floatArray) list)


getArrayElem : Int -> Array Float -> Float
getArrayElem index array =
    case Array.get index array of
        Just val ->
            val

        Nothing ->
            0



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [] [ text "GC number:" ]
            , input [ type_ "text", placeholder "GC number", onInput GCInputChange ] [ text (toString model.genomeCopy) ]
            ]
        , div []
            [ label [] [ text "Cycle Cuttoff:" ]
            , input [ type_ "text", placeholder "Cycle cuttoff", onInput CycleCuttofInputChange ] [ text (toString model.cycleCuttof) ]
            ]
        , div []
            [ label [] [ text "Bootstrap Amount:" ]
            , input [ type_ "text", placeholder "Bootstrap Amount", onInput BootstrapValueChange ] [ text (toString model.bootstrapAmount) ]
            ]
        , div []
            [ label [] [ text "Cycle Threshold Amount:" ]
            , input [ type_ "text", placeholder "CT Amount", onInput CTAmountChanged ] [ text (toString model.ctAmount) ]
            ]
        , div [] (generateTupleList model)
        , div [] [ (text ("Positive Percentage: " ++ (toString model.positivePercentage))) ]
        , div [] (viewRandomList model.bootstrappedData)
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


viewRandomList : Array Float -> List (Html Msg)
viewRandomList array =
    Array.toList (Array.indexedMap (\i value -> div [] [ text (toString (value)) ]) array)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
