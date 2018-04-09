module FlameGraph
    exposing
        ( StackFrame(..)
        , fromString
        , view
        )

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style, title)
import Html.Events exposing (onClick)
import Set


type
    StackFrame
    -- name, count, children
    = StackFrame String Int (List StackFrame)



-- Render


view : (StackFrame -> List StackFrame -> a) -> List StackFrame -> Html a
view onBarClick frames =
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
                                , onClick (onBarClick frame frames)
                                ]
                                [ span [ style labelStyles ] [ text name ] ]
                            , view onBarClick children
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



-- Parse


type PreStackFrame
    = PreStackFrame (List String) Int


fromString : String -> List StackFrame
fromString =
    preParse >> nest


initialLast : List a -> Maybe ( List a, a )
initialLast lst =
    let
        divider =
            List.length lst - 1
    in
    case List.head <| List.drop divider lst of
        Just last ->
            Just ( List.take divider lst, last )

        Nothing ->
            Nothing


parseLine : String -> Result String ( List String, Int )
parseLine line =
    case initialLast (String.words line) of
        Just ( initial, last ) ->
            case String.toInt last of
                Ok num ->
                    Ok ( String.split ";" (String.join " " initial), num )

                Err error ->
                    Err error

        Nothing ->
            Err "Unable to split line"


preParse : String -> List PreStackFrame
preParse input =
    String.split "\n" input
        |> List.map
            (\line ->
                case parseLine line of
                    Ok ( stack, count ) ->
                        PreStackFrame stack count

                    Err error ->
                        PreStackFrame [] 0
            )


nest : List PreStackFrame -> List StackFrame
nest frames =
    let
        namedLists : List ( String, List PreStackFrame )
        namedLists =
            frames
                |> groupBy
                    (\f ->
                        case f of
                            PreStackFrame children _ ->
                                case List.head children of
                                    Just name ->
                                        name

                                    Nothing ->
                                        ""
                    )
                |> List.sortBy (\( name, _ ) -> name)

        frameCount pre =
            case pre of
                PreStackFrame _ count ->
                    count
    in
    List.map
        (\( name, preFrames ) ->
            let
                count : Int
                count =
                    preFrames |> List.map frameCount |> List.sum

                children =
                    preFrames
                        |> List.map
                            (\pre ->
                                case pre of
                                    PreStackFrame lst count ->
                                        case List.tail lst of
                                            Just remaining ->
                                                PreStackFrame remaining count

                                            Nothing ->
                                                -- TODO: this shouldn't happen, right?
                                                PreStackFrame [] count
                            )
                        |> List.filter
                            (\pre ->
                                case pre of
                                    PreStackFrame lst _ ->
                                        List.length lst > 0
                            )
            in
            StackFrame name count (nest children)
        )
        namedLists


groupBy : (a -> comparable) -> List a -> List ( comparable, List a )
groupBy fn lst =
    let
        keys =
            Set.fromList (List.map fn lst)
    in
    List.map
        (\key -> ( key, List.filter ((==) key << fn) lst ))
        (Set.toList keys)
