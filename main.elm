module Main exposing (..)

import Http
import Html exposing (Html, div, text, ul, li, h1, table, tbody, tr, td, thead, th, a)
import Html.Attributes exposing (href)
import Json.Decode exposing (int, string, float, list, nullable, bool, decodeString, field, map7, map6, Decoder)
import Json.Decode exposing (..)
import Debug exposing (log)
import Date
import Task exposing (perform)


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.batch [ getPerformances, now ] )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias ShowRecord =
    { character : String
    , title : String
    , company : String
    , start_date : String
    , end_date : String
    , ticket_url : Maybe String
    , sung_in_translation : Bool
    }


type alias Resume =
    List ShowRecord


type alias Model =
    { resume : Resume
    , renderDate : Maybe Date.Date
    }


decodeShow : Decoder ShowRecord
decodeShow =
    map7 ShowRecord
        (field "character" string)
        (field "title" string)
        (field "company" string)
        (field "start_date" string)
        (field "end_date" string)
        (maybe (field "ticket_url" string))
        (field "sung_in_translation" bool)


decodeResume : Decoder Resume
decodeResume =
    list decodeShow

compareDates : Date.Date -> Date.Date -> Order
compareDates a b =
    Basics.compare (Date.toTime a) (Date.toTime b)


showIsInTheFuture : Date.Date -> { a | end_date : String } -> Bool
showIsInTheFuture now show =
    case Date.fromString (show.end_date) of
        Err _ ->
            False

        Ok date ->
            case log "cpm" (compareDates now date) of
                LT ->
                    False

                GT ->
                    True

                EQ ->
                    True


futureResume : ( Date.Date, Resume ) -> Resume
futureResume ( now, resume ) =
    let
        filterFuture =
            List.filter (showIsInTheFuture now)
    in
        filterFuture resume


model : Model
model =
    { resume = []
    , renderDate = Nothing
    }


extractYear : String -> String
extractYear date =
    case Date.fromString (date) of
        Err _ ->
            ""

        Ok d ->
            toString (Date.year d)


upcomingShowRowView : ShowRecord -> Html Msg
upcomingShowRowView show =
    let
        { character, title, company, start_date, ticket_url } =
            show
    in
        tr []
            [ td [] [ text character ]
            , td [] [ text title ]
            , td [] [ text company ]
            , td [] [ text (extractYear start_date) ]
            , td []
                [ case ticket_url of
                    Nothing ->
                        -- TODO: Find a way to have no node here
                        a [] []

                    Just url ->
                        a [ href url ] [ text "Tickets" ]
                ]
            ]

upcomingView : Resume -> Html Msg
upcomingView resume =
    table []
        [ thead []
            [ th [] [ text "Character" ]
            , th [] [ text "Title" ]
            , th [] [ text "Company" ]
            , th [] [ text "Date" ]
            , th [] [] -- Tickets
            ]
        , tbody [] <|
            List.map upcomingShowRowView <|
                resume
        ]


-- UPDATE
type Msg = GotPerformances (Result Http.Error Resume)
    | SetDate Date.Date


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPerformances (Ok resume) ->
            ( { model | resume = resume }, Cmd.none )

        GotPerformances (Err error) ->
            ( model, Cmd.none )

        SetDate date ->
            ( { model | renderDate = Just date }, Cmd.none )



-- VIEW
view : Model -> Html Msg
view model =
    case model.renderDate of
        Just renderDate ->
            div []
                [ h1 [] [text "Upcomming"]
                , upcomingView (futureResume ( renderDate, model.resume ))
                ]

        Nothing ->
            div [] []


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


getPerformances : Cmd Msg
getPerformances =
    let
        url =
            "https://raw.githubusercontent.com/captbaritone/singer-website/master/performances.json"
    in
        Http.send GotPerformances (Http.get url decodeResume)


now : Cmd Msg
now =
    Date.now |> Task.perform SetDate
