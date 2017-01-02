port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing ( value, class, type_, height, width )
import Html.Events exposing ( onSubmit, onInput, onClick )
import Maybe exposing ( Maybe(..),  withDefault )
import Markdown exposing ( toHtml )
import String
import List
import Svg
import Svg.Attributes

main =
  Html.program { init = init, view = view, update = update, subscriptions= subscriptions }

type alias Row = {
    n: String,
    v: Float
}

type alias Model = {
    cn: Maybe String,
    cv: Maybe String,
    e: List Error,
    rows: List Row,
    chartHtml: Maybe String
}

type Msg = AddRow | NameChange String | ValueChange String | DeleteRow Row

type Error = MissingName | MissingValue | InvalidValue | NameExist

init: ( Model, Cmd Msg )
init = ( Model Nothing Nothing [] [] Nothing, Cmd.none )

updateName: Model -> String -> Model
updateName model n = { model | cn = if String.isEmpty n then Nothing else Just n }

updateValue: Model -> String -> Model
updateValue model v = { model | cv = if String.isEmpty v then Nothing else Just v }

validateName: Model -> Result Error String
validateName model =
    case model.cn of
        Just n -> if List.any (\ x -> x.n == n) model.rows then Err NameExist else Ok n
        Nothing -> Err MissingName

validateValue: Model -> Result Error Float
validateValue model =
    case model.cv of
        Just v ->
            case String.toFloat v of
                Ok fv -> Ok fv
                Err msg -> Err InvalidValue
        Nothing -> Err MissingValue

validate: Model -> Result ( List Error ) ( String, Float )
validate model =
    case ( validateName model, validateValue model ) of 
        ( Err e1, Err e2 ) -> Err [ e1, e2 ]
        ( Err e, _ ) -> Err [ e ]
        ( _, Err e ) -> Err [ e ]
        ( Ok n, Ok v ) -> Ok ( n, v )

toTwoDecimalPercentage: Float -> Float
toTwoDecimalPercentage f = ( toFloat <| round ( f * 1000 )  ) / 10

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        NameChange n -> ( updateName model n, Cmd.none )
        ValueChange v -> ( updateValue model v, Cmd.none )
        AddRow -> 
            case validate model of
                Ok ( n, v ) -> 
                    let
                        rows: List Row
                        rows = ( Row n v ) :: model.rows
                    in
                        ( { model |
                            cn = Nothing,
                            cv = Nothing,
                            e = [],
                            rows = rows
                        }, genChart rows )
                Err e -> ( { model | e = e }, Cmd.none )
        DeleteRow r ->
            let
                rows: List Row
                rows = List.filter (\ x-> x /= r ) model.rows
            in
              ( { model | rows = rows }, genChart rows )

theTable: ( Row -> Msg ) -> Model -> Html Msg
theTable deleteRow model =
    let
        total: Float
        total = List.foldr (\r sum -> sum + r.v ) 0 model.rows
    in
        table [ class "table table-striped" ] [
            thead [] [
                tr [] [
                    th [] [ text "factor" ],
                    th [] [ text "value" ],
                    th [] [ text "pct." ],
                    th [] []
                ]
            ],
            tbody [] ( 
                List.map (\ x -> tr [] [
                    td [] [ text x.n ],
                    td [] [ text <| toString x.v ],
                    td [] [ text ( ( toString <| toTwoDecimalPercentage ( x.v / total ) ) ++ "%" ) ],
                    td [] [ button [ class "close", onClick ( deleteRow x ) ] [ span [] [ toHtml [] "&times;" ] ] ]
                ]) model.rows
            )
        ]

theForm: ( Msg, String -> Msg, String -> Msg ) -> Model -> Html Msg
theForm ( addRow, nameChange, valueChange ) model =
    form [ onSubmit addRow ] [
        div [ class "form-group" ] [
            label [] [ text "factor" ],
            input [ class "form-control", value ( withDefault "" model.cn ), onInput nameChange ] []
        ],
        div [ class "form-group" ] [
            label [] [ text "value" ],
            input [ class "form-control", value ( withDefault "" model.cv ), onInput valueChange ] []
        ],
        button [ class "btn btn-default", type_ "submit" ] [ text "OK" ]
    ]

theNotifications: Model -> Html Msg
theNotifications model =
    div [] (
        List.map (\ e -> 
            p [ class "bg-danger" ] [
                strong [] [
                    text ( case e of
                        MissingName -> "Name is missing"
                        MissingValue -> "Value is missing"
                        InvalidValue -> "Value is invalid"
                        NameExist -> "Same name exists"
                    )
                ]
            ]
        ) model.e
    )

view: Model -> Html Msg
view model =
   div [ class "container" ] [
        div [ class "jumbotron pa-jumbo" ] [
            h1 [] [ text "Pareto diagram generator" ],
            p [] [ text "a simple pareto diagram generator powered by Elm and D3" ]
        ],
        div [ class "col-md-4 pa-form" ] [
            theTable DeleteRow model,
            theForm ( AddRow, NameChange, ValueChange ) model,
            theNotifications model
       ],
       div [ class "col-md-8 pa-chart" ] [
           Svg.svg [ Svg.Attributes.width "100%", Svg.Attributes.viewBox "0 0 750 600", Svg.Attributes.preserveAspectRatio "xMidYMid meet"] []
       ]
    ]

port genChart: List Row -> Cmd msg

subscriptions: Model -> Sub Msg
subscriptions model = Sub.none