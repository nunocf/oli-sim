module Main exposing (..)

import Html exposing (..)
import Array exposing (..)
import Material
import Material.Scheme
import Material.Layout as Layout
import Material.Options as Options exposing (css, Style)
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Button as Button
import Material.Color as Color
import Material.Typography as Typo
import Material.Grid as Grid exposing (..)
import Random exposing (..)
import ConversionUtils exposing (convertToInt, convertToFloat)


-- MODEL


type alias Model =
    { mdl :
        Material.Model
    , genomeCopyNumber : Int
    , sampleCode : String
    , replicateAmount : Int
    , cutoff : Int
    , ctArrayStrings : Array String
    , ctArray : Array Float
    , bootstrapData : List (Array Float)
    , bootstrapAmount : Int
    , bootstrapCycles : Int
    , result : Float
    , resultsReady : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model Material.model 0 "" 0 0 Array.empty Array.empty [] 0 0 0 False, Cmd.none )



-- ACTION, UPDATE


type Msg
    = GenomeCopyInputChange String
    | SampleCodeInputChange String
    | NumberOfReplicatesInputChange String
    | CTInputChanged Int String
    | CTStringsChanged
    | CutoffChanged String
    | BootstrapReplicatesChanged String
    | BootstrapCyclesChanged String
    | GenerateResults
    | NewRandomList (List (List Int))
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CutoffChanged cutoff ->
            ( { model | cutoff = convertToInt cutoff }, Cmd.none )

        GenomeCopyInputChange gc ->
            ( { model | genomeCopyNumber = convertToInt gc }, Cmd.none )

        SampleCodeInputChange sc ->
            ( { model | sampleCode = sc }, Cmd.none )

        NumberOfReplicatesInputChange nr ->
            let
                amount =
                    convertToInt nr
            in
                ( { model | replicateAmount = amount, ctArrayStrings = Array.repeat amount "" }, Cmd.none )

        CTStringsChanged ->
            ( { model | ctArray = Array.map (\val -> convertToFloat val) model.ctArrayStrings }, Cmd.none )

        CTInputChanged index newSample ->
            update CTStringsChanged { model | ctArrayStrings = Array.set index newSample model.ctArrayStrings }

        BootstrapReplicatesChanged br ->
            ( { model | bootstrapAmount = convertToInt br }, Cmd.none )

        BootstrapCyclesChanged bc ->
            ( { model | bootstrapCycles = convertToInt bc }, Cmd.none )

        GenerateResults ->
            let
                maxIndex =
                    ((Array.length model.ctArray) - 1)
            in
                ( { model | resultsReady = False }
                , Random.generate NewRandomList (generateRandomIndexList model.bootstrapCycles model.bootstrapAmount maxIndex)
                )

        NewRandomList listOfLists ->
            let
                bootstrapData =
                    generateBootstrap listOfLists model.ctArray

                result =
                    calculatePositivePercentage bootstrapData model.cutoff
            in
                ( { model | resultsReady = True, bootstrapData = bootstrapData, result = result }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


calculatePositivePercentage : List (Array Float) -> Int -> Float
calculatePositivePercentage listOfFloatArray threshold =
    let
        testResults =
            List.map
                (\array -> calculateSinglePercentage array threshold)
                listOfFloatArray
    in
        List.sum testResults / toFloat (List.length testResults)


calculateSinglePercentage : Array Float -> Int -> Float
calculateSinglePercentage floatArray threshold =
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


generateBootstrap : List (List Int) -> Array Float -> List (Array Float)
generateBootstrap listOfLists floatArray =
    List.map (\list -> Array.fromList (List.map (\i -> getArrayElem i floatArray) list)) listOfLists


generateRandomIndexList : Int -> Int -> Int -> Random.Generator (List (List Int))
generateRandomIndexList cycles len maxNumber =
    Random.list cycles (Random.list len (Random.int 0 maxNumber))



-- VIEW


type alias Mdl =
    Material.Model



-- Cell styling


style : Int -> List (Style a)
style h =
    [ css "text-sizing" "border-box"
    , css "background-color" "#FFFFFF"
    , css "height" (toString h ++ "px")
    , css "padding-left" "8px"
    , css "padding-top" "4px"
    , css "color" "black"
    ]



-- Cell variants


democell : Int -> List (Style a) -> List (Html a) -> Cell a
democell k styling =
    cell <| List.concat [ style k, styling ]


std : List (Style a) -> List (Html a) -> Cell a
std =
    democell 200


resizable : Int -> List (Style a) -> List (Html a) -> Cell a
resizable textfieldNumber =
    democell (textfieldNumber * 67)



-- Grid


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader ]
        { header = [ h1 [] [ text "Limit of Detection Calculator" ] ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


viewBody : Model -> Html Msg
viewBody model =
    div
        []
        [ grid [] [ viewGrid1 model, viewGrid2 model, viewGrid3 model, viewGrid4 model ]
        ]
        |> Material.Scheme.topWithScheme Color.Indigo Color.LightGreen


viewGrid1 : Model -> Cell Msg
viewGrid1 model =
    std [ Grid.size All 3 ]
        [ div []
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Genome Copy number"
                , Options.onInput GenomeCopyInputChange
                , toString model.genomeCopyNumber
                    |> normalizeZeroEmptyString
                    |> Textfield.value
                , Options.id "gc-number"
                ]
                []
            ]
        , div
            []
            [ Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Sample code"
                , Textfield.value model.sampleCode
                , Options.onInput SampleCodeInputChange
                ]
                []
            ]
        , div
            []
            [ Textfield.render Mdl
                [ 2 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Cutoff"
                , toString model.cutoff
                    |> normalizeZeroEmptyString
                    |> Textfield.value
                , Options.onInput CutoffChanged
                ]
                []
            ]
        , div
            []
            [ Textfield.render Mdl
                [ 3 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Number of Replicates"
                , toString model.replicateAmount
                    |> normalizeZeroEmptyString
                    |> Textfield.value
                , Options.onInput NumberOfReplicatesInputChange
                ]
                []
            ]
        ]


viewGrid2 : Model -> Cell Msg
viewGrid2 model =
    resizable model.replicateAmount
        [ Grid.size All 3 ]
        (model.ctArrayStrings
            |> Array.indexedMap
                (\i ct ->
                    div []
                        [ Textfield.render Mdl
                            [ 4 + i ]
                            model.mdl
                            [ Textfield.floatingLabel
                            , "CT value "
                                ++ toString (i + 1)
                                |> Textfield.label
                            , Options.onInput (CTInputChanged i)
                            , Textfield.value ct
                            ]
                            []
                        ]
                )
            |> Array.toList
        )


viewGrid3 : Model -> Cell Msg
viewGrid3 model =
    let
        index =
            4 + Array.length model.ctArrayStrings
    in
        std [ Grid.size All 3 ]
            [ div []
                [ Textfield.render Mdl
                    [ index + 1 ]
                    model.mdl
                    [ Textfield.floatingLabel
                    , Textfield.label "Bootstrap Replicas"
                    , Options.onInput BootstrapReplicatesChanged
                    , toString model.bootstrapAmount |> normalizeZeroEmptyString |> Textfield.value
                    ]
                    []
                ]
            , div []
                [ Textfield.render Mdl
                    [ index + 2 ]
                    model.mdl
                    [ Textfield.floatingLabel
                    , Textfield.label "Bootstrap Cycles"
                    , Options.onInput BootstrapCyclesChanged
                    , toString model.bootstrapCycles |> normalizeZeroEmptyString |> Textfield.value
                    ]
                    []
                ]
            , div []
                [ Button.render Mdl
                    [ index + 3 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.onClick GenerateResults
                    ]
                    [ text "Generate results" ]
                ]
            ]


viewGrid4 : Model -> Cell Msg
viewGrid4 model =
    let
        index =
            4 + Array.length model.ctArrayStrings + 4
    in
        std [ Grid.size All 3 ]
            [ div []
                [ Options.styled p
                    [ Typo.display2 ]
                    [ if model.resultsReady == True then
                        text "Generated result"
                      else
                        text ""
                    ]
                , Options.styled p
                    [ Typo.display2 ]
                    [ if model.resultsReady == True then
                        text (toString model.result)
                      else
                        text ""
                    ]
                ]
            ]


normalizeZeroEmptyString : String -> String
normalizeZeroEmptyString val =
    if val == "0" then
        ""
    else
        val


getArrayElem : Int -> Array Float -> Float
getArrayElem index array =
    case Array.get index array of
        Just val ->
            val

        Nothing ->
            0



-- Load Google Mdl CSS. You'll likely want to do that not in code as we
-- do here, but rather in your master .html file. See the documentation
-- for the `Material` module for details.


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
