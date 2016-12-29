import Html exposing (..)
import Html.Attributes exposing ( value )
import Html.Events exposing ( onInput, onClick )
import Maybe exposing ( Maybe(..),  withDefault )
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

type Msg = AddRow | NameChange String | ValueChange String

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

view: Model -> Html Msg
view model = div []
    [
        table [] [
            thead [] [
                tr [] [
                    th [] [ text "name" ],
                    th [] [ text "value" ]
                ]
            ],
            tbody [] ( 
                List.map (\x -> tr [] [
                    td [] [ text x.n ],
                    td [] [ text ( toString x.v ) ]
                ]) model.rows
            )
        ],
        div [] [
            label [] [ text "name" ],
            input [ value ( withDefault "" model.cn ), onInput NameChange ] [],
            label [] [ text "value" ],
            input [ value ( withDefault "" model.cv ), onInput ValueChange ] [],
            button [ onClick AddRow ] [ text "OK" ],
            strong [] [ text ( if List.member MissingName model.e then "NameIsEmpty" else "" ) ],
            strong [] [ text ( if List.member MissingValue model.e then "ValueIsEmpty" else "" ) ],
            strong [] [ text ( if List.member InvalidValue model.e then "ValueIsInvalid" else "" ) ]
        ]
    ]