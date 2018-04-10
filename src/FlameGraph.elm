module FlameGraph
    exposing
        ( StackFrame(..)
        , fromString
        , view
        , viewFromRoot
        )

import Dict
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style, title)
import Html.Events exposing (onClick, onMouseEnter)


type StackFrame
    = StackFrame
        { name : String
        , count : Int
        , children : List StackFrame
        }



-- Render


type alias Viewer a =
    (StackFrame -> a)
    -> (StackFrame -> a)
    -> List StackFrame
    -> Html a


view : Viewer a
view onBarHover onBarClick frames =
    viewRow view barStyles onBarHover onBarClick frames


viewRow : Viewer a -> List ( String, String ) -> Viewer a
viewRow viewChildren barStyles onBarHover onBarClick frames =
    let
        total : Int
        total =
            frames
                |> List.map
                    (\(StackFrame { count }) -> count)
                |> List.sum
    in
    div
        [ style flameStyles ]
        (List.map
            (\frame ->
                case frame of
                    StackFrame { name, count, children } ->
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
                                , onClick (onBarClick frame)
                                , onMouseEnter (onBarHover frame)
                                ]
                                [ span [ style labelStyles ] [ text name ] ]
                            , viewChildren onBarHover onBarClick children
                            ]
            )
            frames
        )


flameStyles =
    [ ( "width", "100%" )
    , ( "position", "relative" )
    , ( "display", "flex" )
    ]


barStyles =
    [ ( "position", "relative" )
    , ( "overflow-x", "hidden" )
    , ( "height", "14px" )
    , ( "background-color", "rgba(89, 235, 89, 0.3)" )
    , ( "border-radius", "2px" )
    , ( "margin", "1px" )
    ]


columnStyles =
    [ ( "display", "flex" )
    , ( "flex-direction", "column" )
    ]


labelStyles =
    [ ( "font-size", "11px" )
    , ( "position", "absolute" )
    , ( "padding", "0 4px" )
    , ( "white-space", "nowrap" )
    ]


viewFromRoot :
    (StackFrame -> a)
    -> (StackFrame -> a)
    -> StackFrame
    -> List StackFrame
    -> Html a
viewFromRoot onBarHover onBarClick frame root =
    let
        preBars : List (Html a)
        preBars =
            stack frame root
                |> List.map
                    (List.singleton
                        >> viewRow
                            (\_ _ _ -> noHtml)
                            (( "opacity", "0.5" ) :: barStyles)
                            onBarHover
                            onBarClick
                    )
    in
    div []
        (preBars ++ [ view onBarHover onBarClick [ frame ] ])


noHtml : Html a
noHtml =
    text ""


stack : StackFrame -> List StackFrame -> List StackFrame
stack frame program =
    let
        findFrame : StackFrame -> List StackFrame -> Maybe StackFrame
        findFrame frame program =
            List.head <| List.filter ((==) frame) program

        recurse : StackFrame -> List StackFrame -> Maybe (List StackFrame)
        recurse frame program =
            case findFrame frame program of
                Just (StackFrame { name }) ->
                    -- found it
                    Just []

                Nothing ->
                    List.map
                        (\((StackFrame { children }) as fr) -> ( fr, recurse frame children ))
                        program
                        |> List.filterMap
                            (\pair ->
                                case pair of
                                    ( fr, Just children ) ->
                                        Just ( fr, children )

                                    _ ->
                                        Nothing
                            )
                        |> List.head
                        |> Maybe.map
                            (\( fr, children ) ->
                                fr :: children
                            )
    in
    Maybe.withDefault [] (recurse frame program)



-- Parse


type alias PreStackFrame =
    { children : List String
    , count : Int
    }


fromString : String -> List StackFrame
fromString =
    preParse >> nest


parseLine : String -> Result String ( List String, Int )
parseLine =
    let
        f : ( List String, String ) -> Result String ( List String, Int )
        f ( initial, last ) =
            last
                |> String.toInt
                |> Result.map
                    (\num -> ( String.split ";" (String.join " " initial), num ))
    in
    String.words
        >> unsnoc
        >> maybe (Err "Unable to split line") f


preParse : String -> List PreStackFrame
preParse =
    let
        f : Result String ( List String, Int ) -> PreStackFrame
        f result =
            case result of
                Ok ( stack, count ) ->
                    { children = stack
                    , count = count
                    }

                Err _ ->
                    { children = []
                    , count = 0
                    }
    in
    String.split "\n"
        >> List.map (parseLine >> f)


nest : List PreStackFrame -> List StackFrame
nest =
    groupBy (.children >> List.head >> Maybe.withDefault "")
        >> List.filter (\( name, _ ) -> name /= "")
        >> List.map
            (\( name, preFrames ) ->
                let
                    count : Int
                    count =
                        preFrames
                            |> List.map .count
                            |> List.sum

                    children : List PreStackFrame
                    children =
                        preFrames
                            |> List.filterMap
                                (\{ children, count } ->
                                    List.tail children
                                        |> Maybe.map
                                            (\remaining ->
                                                { children = remaining
                                                , count = count
                                                }
                                            )
                                )
                in
                StackFrame
                    { name = name
                    , count = count
                    , children = nest children
                    }
            )


groupBy : (a -> comparable) -> List a -> List ( comparable, List a )
groupBy fn =
    List.foldr
        (\x ->
            Dict.update
                (fn x)
                (Maybe.withDefault [] >> cons x >> Just)
        )
        Dict.empty
        >> Dict.toList


cons : a -> List a -> List a
cons =
    (::)


unsnoc : List a -> Maybe ( List a, a )
unsnoc xs =
    case xs of
        [] ->
            Nothing

        x :: xs ->
            case unsnoc xs of
                Nothing ->
                    Just ( [], x )

                Just ( ys, y ) ->
                    Just ( x :: ys, y )


maybe : b -> (a -> b) -> Maybe a -> b
maybe z f mx =
    case mx of
        Nothing ->
            z

        Just x ->
            f x
