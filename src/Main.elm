module Main exposing (..)

import FlameGraph exposing (StackFrame(..))
import Html exposing (Html, button, div, pre, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, fetchExample )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { frames : Maybe (List StackFrame)
    , selected : Maybe StackFrame
    , hovered : Maybe StackFrame
    }


initialModel : Model
initialModel =
    { frames = Nothing
    , selected = Nothing
    , hovered = Nothing
    }


type Msg
    = SelectFrame StackFrame
    | ClearSelected
    | FrameHover StackFrame
    | FetchExample (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SelectFrame frame ->
            ( { model | selected = Just frame }, Cmd.none )

        ClearSelected ->
            ( { model | selected = Nothing }, Cmd.none )

        FrameHover frame ->
            ( { model | hovered = Just frame }, Cmd.none )

        FetchExample (Ok example) ->
            ( { model
                | frames = Just (FlameGraph.fromString example)
              }
            , Cmd.none
            )

        FetchExample (Err e) ->
            ( Debug.log (toString e) model, Cmd.none )


fetchExample : Cmd Msg
fetchExample =
    let
        url =
            "https://brandly.github.io/react-flame-graph/collapsed-perf.txt"
    in
    Http.send FetchExample (Http.getString url)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


containerStyles =
    [ ( "width", "1080px" ), ( "max-width", "100%" ), ( "margin", "0 auto" ) ]


view : Model -> Html Msg
view model =
    let
        sumFrames : List StackFrame -> Int
        sumFrames =
            List.map
                (\(StackFrame { count }) -> count)
                >> List.sum

        totalSamples : Int
        totalSamples =
            model.frames |> Maybe.map sumFrames |> Maybe.withDefault 0
    in
    div [ style containerStyles ]
        [ case model.selected of
            Just _ ->
                button
                    [ style
                        [ ( "float", "left" )
                        , ( "margin", "0 12px" )
                        , ( "border", "1px solid #CCC" )
                        , ( "border-radius", "2px" )
                        , ( "cursor", "pointer" )
                        ]
                    , onClick ClearSelected
                    ]
                    [ text "reset zoom" ]

            Nothing ->
                text ""
        , pre [ style [ ( "padding", "0 12px" ) ] ]
            [ case model.hovered of
                Just (StackFrame { name, count }) ->
                    text
                        (name
                            ++ " ("
                            ++ toString count
                            ++ " sample"
                            ++ (if count > 1 then
                                    "s"
                                else
                                    ""
                               )
                            ++ ", "
                            -- TODO: toFixed??
                            ++ toString (toFloat count / toFloat totalSamples * 100)
                            ++ "%)"
                        )

                Nothing ->
                    text ""
            ]
        , case ( model.selected, model.frames ) of
            ( Just selected, Just root ) ->
                FlameGraph.viewFromRoot
                    FrameHover
                    SelectFrame
                    selected
                    root

            ( _, Just root ) ->
                FlameGraph.view
                    FrameHover
                    SelectFrame
                    root

            _ ->
                text "Loading..."
        ]
