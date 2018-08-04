module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme
import Material.Layout as Layout
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Material.Typography as Typo
import Material.Options as Options
import ConversionUtils


-- MODEL


type alias Model =
    { mdl :
        Material.Model
    , genomeCopyNumber : Int
    , sampleCode : String
    , replicateAmount : Int

    -- Boilerplate: model store for any and all Mdl components you use.
    }


init : ( Model, Cmd Msg )
init =
    ( Model Material.model 0 "" 0, Cmd.none )



-- ACTION, UPDATE


type Msg
    = GenomeCopyInputChange String
    | SampleCodeInputChange String
    | NumberOfReplicatesInputChange String
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenomeCopyInputChange gc ->
            ( { model | genomeCopyNumber = ConversionUtils.convertToInt gc 0 }, Cmd.none )

        SampleCodeInputChange sc ->
            ( { model | sampleCode = sc }, Cmd.none )

        NumberOfReplicatesInputChange nr ->
            ( { model | genomeCopyNumber = ConversionUtils.convertToInt nr 0 }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader ]
        { header = [ h1 [ style [ ( "padding", "2rem" ) ] ] [ text "Limit of Detection Calculator" ] ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


viewBody : Model -> Html Msg
viewBody model =
    div
        [ style [ ( "padding", "2rem" ) ] ]
        [ div []
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Genome Copy number"
                , Options.onInput GenomeCopyInputChange
                , Textfield.value (toString model.genomeCopyNumber)
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
                , Textfield.label "Number of Replicates"
                , Textfield.value (toString model.replicateAmount)
                , Options.onInput NumberOfReplicatesInputChange
                ]
                []
            ]
        ]
        |> Material.Scheme.top



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
