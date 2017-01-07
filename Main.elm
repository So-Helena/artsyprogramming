in exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
        beginnerProgram { model = initModel, view = view, update = update }


initModel : Model
    initModel =
            { name = ""
                , age = 0
                    , sexuality = Nothing
                        , savedpersons = []
                            }



-- TYPES


type Sexuality
    = PanSexual
        | HomoSexual
            | ASexual
                | BiSexual
                    | Other


type alias Person =
        { name : String
            , age : Int
                , sexuality : Sexuality
                    }


type alias Model =
        { name : String
            , age : Int
                , sexuality : Maybe Sexuality
                    , savedpersons : List Person
                        }



-- UPDATE


type Msg
    = Namechange String
        | Agechange String
            | SexChange String
                | SexClear
                    | Submit


update : Msg -> Model -> Model
update msg model =
        case msg of
                    Namechange newName ->
                                    { model | name = newName }

        Agechange newAge ->
                        let
                                            newInt =
                                                                    Result.withDefault 0 (String.toInt newAge)
                                                                                in
                                                                                                    { model | age = newInt }

        SexChange sexuality ->
                        { model | sexuality = Just (updateSexuality sexuality) }

        SexClear ->
                        { model | sexuality = Nothing }

        Submit ->
                        case model.sexuality of
                                            Nothing ->
                                                                    model

                Just sexuality ->
                                        let
                                                                    newPerson =
                                                                                                    { name = model.name
                                                                                                                                , age = model.age
                                                                                                                                                            , sexuality = sexuality
                                                                                                                                                                                        }
                                                                                                                                                                                                            in
                                                                                                                                                                                                                                        { model | savedpersons = newPerson :: model.savedpersons }


updateSexuality : String -> Sexuality
updateSexuality text =
        let
                    firstchar =
                                    String.left 1 text
                                        in
                                                    case firstchar of
                                                                    "p" ->
                                                                                        PanSexual

            "h" ->
                                HomoSexual

            "a" ->
                                ASexual

            "b" ->
                                BiSexual

            _ ->
                                Other



-- VIEW


view : Model -> Html Msg
view model =
        div []
                [ input [ placeholder "name", value model.name, onInput Namechange ] []
                        , input [ placeholder "age", value (toString model.age), onInput Agechange ] []
                                , input [ placeholder "sexuality", value (viewSexualityValue model.sexuality), onInput SexChange ] []
                                        , button [ onClick Submit ] [ text "submit" ]
                                                , button [ onClick SexClear ] [ text "clear sexuality" ]
                                                        , viewPersons model.savedpersons
                                                                ]


viewSexualityValue : Maybe Sexuality -> String
viewSexualityValue modelSexuality =
        case modelSexuality of
                    Nothing ->
                                    ""

        Just sexuality ->
                        toString sexuality


viewPersons : List Person -> Html msg
viewPersons persons =
        div [] (List.map viewPerson persons)


viewPerson : Person -> Html msg
viewPerson person =
        case person.sexuality of
                    PanSexual ->
                                    viewSexuality "red" "good times being pansexual"

        HomoSexual ->
                        viewSexuality "green" "i love being homo"

        ASexual ->
                        viewSexuality "blue" "i don't like anything"

        BiSexual ->
                        viewSexuality "yellow" "i swing both ways"

        Other ->
                        viewSexuality "purple" "im confused"


viewSexuality : String -> String -> Html msg
viewSexuality color description =
        div
                [ style [ ( "color", color ) ] ]
                        [ text description ]
                        
