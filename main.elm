module Main exposing (..)

import Http
import Html exposing (Html, div, text, ul, li, h1, table, tbody, tr, td, thead, th, a)
import Html.Attributes exposing (href)
import Json.Decode exposing (string, list, bool, decodeString, field, map7, maybe, Decoder)
import Debug exposing (log)
import Date
import Task exposing (perform)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


getPerformances : Cmd Msg
getPerformances =
    let
        url =
            "https://raw.githubusercontent.com/captbaritone/singer-website/master/performances.json"
    in
        Http.send GotPerformances (Http.get url (list decodeShow))


getDate : Cmd Msg
getDate =
    Date.now |> Task.perform SetDate

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- MODEL

-- Full application state
type alias Model =
    { resume : Resume
    , renderDate : Maybe Date.Date
    }

-- A list of shows
type alias Resume =
    List Show

-- Show
type alias Show =
    { character : String
    , title : String
    , company : String
    , startDate : String
    , endDate : String
    , ticketUrl : Maybe String
    , sungInTranslation : Bool
    }

init = ( 
    { resume = []
    , renderDate = Nothing
    }, Cmd.batch [ getPerformances, getDate ] )


-- UPDATE

type Msg
    = GotPerformances (Result Http.Error Resume)
    | SetDate Date.Date


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPerformances (Ok resume) ->
            ( { model | resume = resume }, Cmd.none )

        GotPerformances (Err error) ->
            -- TODO: Surface some kind of error?
            ( model, Cmd.none )

        SetDate date ->
            ( { model | renderDate = Just date }, Cmd.none )




decodeShow : Decoder Show
decodeShow =
    map7 Show
        (field "character" string)
        (field "title" string)
        (field "company" string)
        (field "start_date" string)
        (field "end_date" string)
        (maybe (field "ticket_url" string))
        (field "sung_in_translation" bool)


compareDates : Date.Date -> Date.Date -> Order
compareDates a b = Basics.compare (Date.toTime a) (Date.toTime b)


showIsInTheFuture : Date.Date -> Show -> Bool
showIsInTheFuture now show =
    case Date.fromString show.endDate of
        Err _ ->
            False

        Ok date ->
            case compareDates now date of
                LT ->
                    False

                GT ->
                    True

                EQ ->
                    True


futureResume : Date.Date -> Resume -> Resume
futureResume now = List.filter (showIsInTheFuture now)

extractYear : String -> String
extractYear date =
    case Date.fromString (date) of
        Err _ ->
            ""

        Ok d ->
            toString (Date.year d)


-- VIEW

-- The entire HTML page
view : Model -> Html Msg
view model =
    case model.renderDate of
        Just renderDate ->
            div []
                [ h1 [] [text "Upcomming"]
                , viewUpcomingShows (futureResume renderDate model.resume)
                ]

        Nothing ->
            div [] []

viewUpcomingShowRow : Show -> Html Msg
viewUpcomingShowRow show =
    let
        { character, title, company, startDate, ticketUrl } =
            show
    in
        tr []
            [ td [] [ text character ]
            , td [] [ text title ]
            , td [] [ text company ]
            , td [] [ text (extractYear startDate) ]
            , td []
                [ case ticketUrl of
                    Nothing ->
                        -- TODO: Find a way to have no node here
                        a [] []

                    Just url ->
                        a [ href url ] [ text "Tickets" ]
                ]
            ]

viewUpcomingShows : Resume -> Html Msg
viewUpcomingShows resume =
    table []
        [ thead []
            [ th [] [ text "Character" ]
            , th [] [ text "Title" ]
            , th [] [ text "Company" ]
            , th [] [ text "Date" ]
            , th [] [] -- Tickets
            ]
        , tbody [] <|
            List.map viewUpcomingShowRow <|
                resume
        ]


