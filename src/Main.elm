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
    , selected : Maybe (List StackFrame)
    , hovered : Maybe StackFrame
    }


initialModel : Model
initialModel =
    { frames = Nothing
    , selected = Nothing
    , hovered = Nothing
    }


type Msg
    = SelectFrames (List StackFrame)
    | ClearSelected
    | FrameHover StackFrame
    | FetchExample (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SelectFrames frames ->
            ( { model | selected = Just frames }, Cmd.none )

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


view : Model -> Html Msg
view model =
    let
        flames =
            FlameGraph.view
                (\frame _ -> FrameHover frame)
                (\_ newSelected -> SelectFrames newSelected)

        sumFrames : List StackFrame -> Int
        sumFrames =
            List.map
                (\(StackFrame { count }) -> count)
                >> List.sum

        totalSamples =
            case model.frames |> Maybe.map sumFrames of
                Just total ->
                    total

                Nothing ->
                    0
    in
    div []
        [ case model.selected of
            Just _ ->
                button [ style [ ( "float", "left" ), ( "margin", "0 12px" ) ], onClick ClearSelected ] [ text "reset zoom" ]

            Nothing ->
                text ""
        , pre [ style [ ( "padding", "0 12px" ) ] ]
            [ case model.hovered of
                Just frame ->
                    case frame of
                        StackFrame { name, count } ->
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
            ( Just selected, _ ) ->
                flames selected

            ( _, Just selected ) ->
                flames selected

            _ ->
                text "Loading..."
        ]
