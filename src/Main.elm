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
    currentName: Maybe String,
    currentValue: Maybe String,
    currentErrors: List Err,
    rows: List Row
}

type Msg = AddRow | CurrentNameChange String | CurrentValueChange String
type Err = NameIsEmpty | ValueIsEmpty | ValueIsInvalid

model : Model
model = Model Nothing Nothing [] []

update: Msg -> Model -> Model
update msg model = 
    case msg of
        CurrentNameChange n -> { model | currentName = Just n }
        CurrentValueChange v -> { model | currentValue = Just v }
        AddRow ->
            let
                m: Model
                m = checkRequiredFields model
            in
                if ( List.member NameIsEmpty m.currentErrors ) || ( List.member ValueIsEmpty m.currentErrors ) then
                    m
                else
                    case String.toFloat ( withDefault "-1" m.currentValue ) of
                        Err msg -> { m | currentErrors = ValueIsInvalid :: m.currentErrors }
                        Ok v -> { m | rows = ( Row ( withDefault "" m.currentName ) v ) :: m.rows }
                    
clearErrors: Model -> Model
clearErrors model =
    {
        model | currentErrors = []
    }

checkCurrentNameEmpty: Model -> Model
checkCurrentNameEmpty model =
    { 
        model | currentErrors =
            case model.currentName of
                Just n -> model.currentErrors
                Nothing -> NameIsEmpty :: model.currentErrors
    }

checkCurrentValueEmpty: Model -> Model
checkCurrentValueEmpty model =
    {
        model | currentErrors =
            case model.currentValue of
                Just n -> model.currentErrors
                Nothing -> ValueIsEmpty :: model.currentErrors
    }

checkRequiredFields: Model -> Model
checkRequiredFields = clearErrors >> checkCurrentNameEmpty >> checkCurrentValueEmpty

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
            input [ value ( withDefault "" model.currentName ), onInput CurrentNameChange ] [],
            label [] [ text "value" ],
            input [ value ( withDefault "" model.currentValue ), onInput CurrentValueChange ] [],
            button [ onClick AddRow ] [ text "OK" ],
            strong [] [ text ( if List.member NameIsEmpty model.currentErrors then "NameIsEmpty" else "" ) ],
            strong [] [ text ( if List.member ValueIsEmpty model.currentErrors then "ValueIsEmpty" else "" ) ],
            strong [] [ text ( if List.member ValueIsInvalid model.currentErrors then "ValueIsInvalid" else "" ) ]
        ]
    ]