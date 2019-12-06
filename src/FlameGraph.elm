module FlameGraph exposing
    ( StackFrame(..)
    , fromString
    , view
    , viewFromRoot
    )

import Dict
import Html exposing (Html, div, span, text)
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


viewRow : Viewer a -> List (Html.Attribute a) -> Viewer a
viewRow viewChildren barStyles_ onBarHover onBarClick frames =
    let
        total : Int
        total =
            frames
                |> List.map
                    (\(StackFrame { count }) -> count)
                |> List.sum
    in
    div flameStyles
        (List.map
            (\frame ->
                case frame of
                    StackFrame { name, count, children } ->
                        div
                            (style "width" (String.fromFloat (toFloat count / toFloat total * 100) ++ "%")
                                :: columnStyles
                            )
                            [ span
                                (barStyles_
                                    ++ [ title name
                                       , onClick (onBarClick frame)
                                       , onMouseEnter (onBarHover frame)
                                       ]
                                )
                                [ span labelStyles [ text name ] ]
                            , viewChildren onBarHover onBarClick children
                            ]
            )
            frames
        )


flameStyles : List (Html.Attribute a)
flameStyles =
    [ style "width" "100%"
    , style "position" "relative"
    , style "display" "flex"
    ]


barStyles : List (Html.Attribute a)
barStyles =
    [ style "position" "relative"
    , style "overflow-x" "hidden"
    , style "height" "14px"
    , style "background-color" "rgba(89, 235, 89, 0.3)"
    , style "border-radius" "3px"
    , style "border" "#FFF 1px solid"
    ]


columnStyles : List (Html.Attribute a)
columnStyles =
    [ style "display" "flex"
    , style "flex-direction" "column"
    ]


labelStyles : List (Html.Attribute a)
labelStyles =
    [ style "font-size" "11px"
    , style "position" "absolute"
    , style "padding" "0 4px"
    , style "white-space" "nowrap"
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
                            (style "opacity" "0.5" :: barStyles)
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
        find : (a -> Bool) -> List a -> Maybe a
        find pred lst =
            List.head <| List.filter pred lst

        recurse : StackFrame -> List StackFrame -> Maybe (List StackFrame)
        recurse frame_ program_ =
            case find ((==) frame_) program_ of
                Just _ ->
                    -- found it
                    Just []

                Nothing ->
                    List.map
                        (\((StackFrame { children }) as fr) -> ( fr, recurse frame_ children ))
                        program_
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
                |> Result.fromMaybe "Unable to parse int"
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
                Ok ( children, count ) ->
                    { children = children
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
                    count_ : Int
                    count_ =
                        preFrames
                            |> List.map .count
                            |> List.sum

                    children_ : List PreStackFrame
                    children_ =
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
                    , count = count_
                    , children = nest children_
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

        x :: xs_ ->
            case unsnoc xs_ of
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
