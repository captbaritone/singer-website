module Main exposing (..)

import Http
import Html exposing (Html, div, text, ul, li, h1, h3, h4, table, tbody, tr, td, thead, th, a, img, small, p)
import Html.Attributes exposing (href, src, class, id)
import Json.Decode exposing (int, string, float, list, nullable, bool, decodeString, field, map7, map6, Decoder)
import Json.Decode exposing (..)
import Task exposing (perform)
import Time exposing (Time, second)
import Date
import Date.Extra exposing (toFormattedString)


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
    Time.every (second * 8) Tick


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


getPerformances : Cmd Msg
getPerformances =
    Http.send GotPerformances (Http.get "../performances.json" decodeResume)



-- Msg


now : Cmd Msg
now =
    Date.now |> Task.perform SetDate


type Msg
    = GotPerformances (Result Http.Error Resume)
    | SetDate Date.Date
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPerformances (Ok resume) ->
            ( { model | resume = resume }, Cmd.none )

        GotPerformances (Err error) ->
            ( model, Cmd.none )

        SetDate date ->
            ( { model | renderDate = Just date }, Cmd.none )

        Tick time ->
            ( { model | caroselCount = model.caroselCount + 1 }, Cmd.none )


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity



-- VIEW


compareDates : Date.Date -> Date.Date -> Order
compareDates a b =
    Basics.compare (Date.toTime a) (Date.toTime b)


showIsInTheFuture : Date.Date -> { a | end_date : String } -> Bool
showIsInTheFuture now show =
    case Date.fromString (show.end_date) of
        Err _ ->
            False

        Ok date ->
            case (compareDates date now) of
                LT ->
                    False

                GT ->
                    True

                EQ ->
                    True


showIsInThePast : Date.Date -> { a | end_date : String } -> Bool
showIsInThePast now show =
    case Date.fromString (show.end_date) of
        Err _ ->
            False

        Ok date ->
            case (compareDates date now) of
                LT ->
                    True

                GT ->
                    False

                EQ ->
                    False


extractYear : String -> String
extractYear date =
    case Date.fromString (date) of
        Err _ ->
            ""

        Ok d ->
            toString (Date.year d)


formatDateString : String -> String
formatDateString date =
    case Date.fromString (date) of
        Err _ ->
            "?"

        Ok d ->
            toFormattedString "MMM ddd" d


futureResume : ( Date.Date, Resume ) -> Resume
futureResume ( now, resume ) =
    List.filter (showIsInTheFuture now) resume


pastResume : ( Date.Date, Resume ) -> Resume
pastResume ( now, resume ) =
    List.filter (showIsInThePast now) resume


upcomingShowRowView : ShowRecord -> Html Msg
upcomingShowRowView show =
    tr []
        [ td [] [ text show.character ]
        , td [] [ text show.title ]
        , td [] [ text show.company ]
        , td [] [ text (formatDateString show.start_date ++ " - " ++ formatDateString show.end_date) ]
        , td []
            [ case show.ticket_url of
                Nothing ->
                    Html.text ""

                Just url ->
                    a [ href url ] [ text "Tickets" ]
            ]
        ]


pastShowRowView : ShowRecord -> Html Msg
pastShowRowView show =
    tr []
        [ td [] [ text show.character ]
        , td []
            [ text
                (show.title
                    ++ (if show.sung_in_translation then
                            "*"
                        else
                            ""
                       )
                )
            ]
        , td [] [ text show.company ]
        , td [] [ text (extractYear show.start_date) ]
        ]


upcomingView : Resume -> Html Msg
upcomingView resume =
    if List.isEmpty resume then
        Html.text ""
    else
        div []
            [ h1 [] [ text "Upcomming" ]
            , table []
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
            ]


pastView : Resume -> Html Msg
pastView resume =
    if List.isEmpty resume then
        Html.text ""
    else
        div []
            [ h4 [ id "resume", class "text-muted" ] [ text "Resume" ]
            , table [ class "table table-hover table-condensed" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Character" ]
                        , th [] [ text "Title" ]
                        , th [] [ text "Company" ]
                        , th [] [ text "Date" ]
                        ]
                    ]
                , tbody [] <|
                    List.map pastShowRowView <|
                        resume
                ]
            , p [ class "pull-right" ]
                [ small [] [ text "*Sung in translation" ]
                ]
            ]


caroselItemView : String -> Html Msg
caroselItemView imgSrc =
    div
        [ class
            ("item "
                ++ (if True then
                        "active"
                    else
                        ""
                   )
            )
        ]
        [ img [ src imgSrc, class "img-responsive img-rounded" ] [] ]


caroselView : Model -> Html Msg
caroselView model =
    div [ class "carousel fade" ]
        [ div [ class "carousel-inner" ] <|
            List.map caroselItemView <|
                model.carouselImages
        ]



-- Model


view : Model -> Html Msg
view model =
    case model.renderDate of
        Just renderDate ->
            div [ class "container" ]
                [ div [ class "header" ]
                    [ h3 []
                        [ text "Jordan Eldredge"
                        , small [] [ text "Baritone" ]
                        ]
                    ]
                , caroselView model
                , upcomingView (futureResume ( renderDate, model.resume ))
                , div [ class "row marketing" ]
                    [ div [ class "col-lg-12" ]
                        [ div [ class "table-responsive" ]
                            [ pastView (pastResume ( renderDate, model.resume ))
                            ]
                        ]
                    ]
                , div [ class "footer marketing" ]
                    [ p [] [ text ("© Jordan Eldredge 2013 - " ++ (toString (Date.year renderDate))) ]
                    ]
                ]

        Nothing ->
            Html.text ""


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
    , caroselCount : Int
    , carouselImages : List String
    }


model : Model
model =
    { resume = []
    , renderDate = Nothing
    , caroselCount = 0
    , carouselImages =
        [ "https://jordaneldredge.com/singer/images/ottone.jpg"
        , "https://jordaneldredge.com/singer/images/pirate-king.jpg"
        , "https://jordaneldredge.com/singer/images/papageno.jpg"
        ]
    }
