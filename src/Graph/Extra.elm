module Graph.Extra exposing
    ( Config
    , dotClickDecoder
    , toDot
    , view
    )

{-| DOT is Graphviz textual format
-}

import Graph exposing (Edge, Graph)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


type alias Config v e msg =
    { vertexId : v -> String
    , vertexToString : v -> String
    , vertexToMsg : Maybe (v -> String)
    , edgeToString : e -> String
    , onClick : { vertexId : String } -> msg
    }


toDot : Config v e msg -> Graph v e -> String
toDot config graph =
    let
        vertices : List v
        vertices =
            graph
                |> Graph.vertices
                |> List.reverse

        edges : List (Edge v e)
        edges =
            Graph.edges graph

        vertexStrings : List String
        vertexStrings =
            case config.vertexToMsg of
                Nothing ->
                    List.map vertexStringInert vertices

                Just toMsg ->
                    List.map (vertexStringClickable toMsg) vertices

        vertexStringInert : v -> String
        vertexStringInert vertex =
            config.vertexId vertex
                ++ " [label = < "
                ++ tableTag
                ++ inertTr (config.vertexToString vertex)
                ++ endTableTag
                ++ " > ];"

        vertexStringClickable : (v -> String) -> v -> String
        vertexStringClickable toMsg vertex =
            config.vertexId vertex
                ++ " [label = < "
                ++ tableTag
                ++ clickableTr
                    (toMsg vertex)
                    (config.vertexToString vertex)
                ++ endTableTag
                ++ " > ];"

        edgeStrings : List String
        edgeStrings =
            edges
                |> List.map
                    (\{ from, to, data } ->
                        config.vertexId from
                            ++ " -> "
                            ++ config.vertexId to
                            ++ " [ label=\""
                            ++ config.edgeToString data
                            ++ "\" ];"
                    )
    in
    [ [ "digraph G { "
      , "node [ shape = plain ];"
      ]
    , vertexStrings
    , edgeStrings
    , [ "}" ]
    ]
        |> List.concat
        |> String.join "\n"


quoted : String -> String
quoted string =
    "&quot;" ++ string ++ "&quot;"


tableTag : String
tableTag =
    "<TABLE BORDER="
        ++ quoted "0"
        ++ " CELLBORDER="
        ++ quoted "1"
        ++ " CELLSPACING="
        ++ quoted "0"
        ++ " CELLPADDING="
        ++ quoted "4"
        ++ ">\n"


endTableTag : String
endTableTag =
    "</TABLE>"


inertTr : String -> String
inertTr vertexString =
    "<TR><TD>" ++ vertexString ++ "</TD></TR>"


clickableTr : String -> String -> String
clickableTr msg vertexString =
    "<TR><TD ID="
        ++ quoted (vertexString ++ ".x")
        ++ " PORT="
        ++ quoted msg
        ++ " HREF="
        ++ quoted " "
        ++ ">"
        ++ vertexString
        ++ "</TD></TR>"


dotClickDecoder : ({ vertexId : String } -> msg) -> Decoder msg
dotClickDecoder toMsg =
    Decode.field "detail" <|
        (Decode.field "id" Decode.string
            |> Decode.andThen
                (\string ->
                    (case String.split "." string of
                        [ vertexId, "x" ] ->
                            Just <| toMsg { vertexId = vertexId }

                        _ ->
                            Nothing
                    )
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail "Bad format of clicked item ID")
                )
        )


view : Config v e msg -> Graph v e -> Html msg
view config graph =
    Html.node "x-graphviz"
        [ Attrs.attribute "dot" (toDot config graph)
        , Events.on "x-graphviz-node-click" (dotClickDecoder config.onClick)
        ]
        []
