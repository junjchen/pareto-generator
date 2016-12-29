import Html exposing (..)
import Html.Attributes exposing ( value, class, type_ )
import Html.Events exposing ( onSubmit, onInput, onClick )
import Maybe exposing ( Maybe(..),  withDefault )
import Markdown exposing ( toHtml )
import String
import List

main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Row = {
    n: String,
    v: Float
}

type alias Model = {
    cn: Maybe String,
    cv: Maybe String,
    e: List Error,
    rows: List Row
}

type Msg = AddRow | NameChange String | ValueChange String | DeleteRow Row

type Error = MissingName | MissingValue | InvalidValue

model : Model
model = Model Nothing Nothing [] []

updateName: Model -> String -> Model
updateName model n = { model | cn = if String.isEmpty n then Nothing else Just n }

updateValue: Model -> String -> Model
updateValue model v = { model | cv = if String.isEmpty v then Nothing else Just v }

validateName: Model -> Result Error String
validateName model =
    case model.cn of
        Just n -> Ok n
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

update: Msg -> Model -> Model
update msg model = 
    case msg of
        NameChange n -> updateName model n
        ValueChange v -> updateValue model v
        AddRow -> 
            case validate model of
                Ok ( n, v ) -> { model |
                    cn = Nothing,
                    cv = Nothing,
                    e = [],
                    rows = ( Row n v ) :: model.rows
                }
                Err e -> { model | e = e }
        DeleteRow r -> { model | rows = List.filter (\ x-> x /= r ) model.rows }


theTable: ( Row -> Msg ) -> Model -> Html Msg
theTable deleteRow model =
    let
        total: Float
        total = List.foldr (\r sum -> sum + r.v ) 0 model.rows
    in
        table [ class "table table-striped" ] [
            thead [] [
                tr [] [
                    th [] [ text "name" ],
                    th [] [ text "value" ],
                    th [] [ text "percentage" ],
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
            label [] [ text "name" ],
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
                    )
                ]
            ]
        ) model.e
    )


view: Model -> Html Msg
view model =
    div [ class "container" ] [
        div [ class "col-md-3 example-form" ] [
            theTable DeleteRow model,
            theForm ( AddRow, NameChange, ValueChange ) model,
            theNotifications model
       ],
       div [ class "col-md-9" ] []
    ]