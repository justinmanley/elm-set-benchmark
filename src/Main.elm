port module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Json.Decode as Decode exposing (Decoder, bool, decodeValue, field, index, int, list, oneOf)
import Json.Encode as Encode
import Set exposing (Set)


type alias Vector2 =
    ( Int, Int )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { values : Set Vector2 }


emptyModel : Model
emptyModel =
    { values = Set.empty }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel
    , Cmd.none
    )


view : Model -> Html Msg
view _ =
    div [] []


type Msg
    = Init (Set Vector2)
    | Reinsert
    | Noop


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map
            (\value ->
                case value of
                    Ok msg ->
                        msg

                    Err error ->
                        Debug.log (Decode.errorToString error) (Init Set.empty)
            )
            (messageReceiver <| decodeValue decoder)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        Init vectors ->
            { values = vectors }

        Reinsert ->
            { values = Set.foldl Set.insert Set.empty model.values
            }

        Noop ->
            model
    , Cmd.none
    )


port sendMessage : Encode.Value -> Cmd msg


port messageReceiver : (Decode.Value -> msg) -> Sub msg


decoder : Decoder Msg
decoder =
    oneOf
        [ Decode.map (\_ -> Reinsert)
            (field "Reinsert" <| bool)
        , Decode.map (Init << Set.fromList)
            (field "Init" <| list vector2Decoder)
        , Decode.map (\_ -> Noop)
            (field "Noop" <| bool)
        ]


vector2Decoder : Decoder Vector2
vector2Decoder =
    Decode.map2 Tuple.pair
        (index 0 int)
        (index 1 int)
