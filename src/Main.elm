module Main exposing (..)

import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class, style, title)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { frames : Maybe (List StackFrame)
    }


initialModel : Model
initialModel =
    { frames =
        Just
            [ StackFrame "java"
                10
                [ StackFrame "some" 4 []
                , StackFrame "cool" 3 []
                , StackFrame "funcs" 2 []
                ]
            ]
    }


type
    StackFrame
    -- name, count, children
    = StackFrame String Int (List StackFrame)


type Msg
    = MyMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


view : Model -> Html Msg
view model =
    div []
        [ case model.frames of
            Just frames ->
                viewFlameGraph frames

            Nothing ->
                text "Loading..."
        ]



-- FlameGraph


flameStyles =
    [ ( "width", "100%" )
    , ( "position", "relative" )
    , ( "display", "flex" )
    ]


barStyles =
    [ ( "position", "relative" )
    , ( "overflow-x", "hidden" )
    , ( "height", "15px" )
    , ( "border", "1px solid #666" )
    ]


columnStyles =
    [ ( "display", "flex" )
    , ( "flex-direction", "column" )
    ]


labelStyles =
    [ ( "font-size", "10px" )
    , ( "position", "absolute" )
    , ( "padding", "0 4px" )
    ]


viewFlameGraph : List StackFrame -> Html a
viewFlameGraph frames =
    let
        total : Int
        total =
            frames
                |> List.map
                    (\frame ->
                        case frame of
                            StackFrame _ count _ ->
                                count
                    )
                |> List.sum
    in
    div
        [ style flameStyles ]
        (List.map
            (\frame ->
                case frame of
                    StackFrame name count children ->
                        div
                            [ style
                                (( "width"
                                 , toString (toFloat count / toFloat total * 100) ++ "%"
                                 )
                                    :: columnStyles
                                )
                            ]
                            [ span
                                [ style barStyles
                                , title name
                                ]
                                [ span [ style labelStyles ] [ text name ] ]
                            , viewFlameGraph children
                            ]
            )
            frames
        )
