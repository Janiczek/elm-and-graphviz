module Main exposing (main)

import Browser
import Graph exposing (Edge, Graph)
import Graph.Extra
import Html exposing (Html)


type alias Flags =
    ()


type alias Model =
    { log : List Msg }


type Msg
    = ItemClicked { vertexId : String }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { log = [] }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemClicked _ ->
            ( { model | log = msg :: model.log }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Elm and Graphviz"
    , body =
        [ viewGraph exampleGraph
        , viewLog model.log
        ]
    }


viewLog : List Msg -> Html Msg
viewLog log =
    log
        |> List.reverse
        |> List.map viewMsg
        |> Html.ul []


viewMsg : Msg -> Html Msg
viewMsg (ItemClicked { vertexId }) =
    Html.li [] [ Html.text vertexId ]


type alias Vertex =
    { id : Int
    , label : String
    }


type alias Edge =
    ()


viewGraph : Graph Vertex Edge -> Html Msg
viewGraph graph =
    Graph.Extra.view
        { onClick = ItemClicked
        , edgeToString = \_ -> ""
        , vertexId = .id >> String.fromInt
        , vertexToMsg = Just (.id >> String.fromInt)
        , vertexToString = \{ id, label } -> "(" ++ String.fromInt id ++ ") " ++ label
        }
        graph


exampleGraph : Graph Vertex Edge
exampleGraph =
    Graph.fromVerticesAndEdges
        []
        [ Edge (Vertex 0 "Heap") (Vertex 1 "0") ()
        , Edge (Vertex 0 "Heap") (Vertex 2 "Just") ()
        , Edge (Vertex 0 "Heap") (Vertex 3 "Nothing") ()
        , Edge (Vertex 2 "Just") (Vertex 4 "Heap") ()
        , Edge (Vertex 4 "Heap") (Vertex 5 "1") ()
        , Edge (Vertex 4 "Heap") (Vertex 6 "Nothing") ()
        , Edge (Vertex 4 "Heap") (Vertex 7 "Nothing") ()
        ]
