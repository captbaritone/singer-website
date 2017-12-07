module Main exposing (..)

import Http exposing (send, get)
import Html exposing (Html, div, text, ul, li, h1, h3, h4, table, tbody, tr, td, thead, th, a, img, small, p)
import Html.Attributes exposing (href, src, class, id, alt)
import Json.Decode exposing (int, string, float, list, nullable, bool, decodeString, field, map8, Decoder, maybe)
import Task exposing (perform)
import Time
import Date
import Date.Extra


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
    Time.every (Time.second * 8) Tick


decodeShow : Decoder ShowRecord
decodeShow =
    map8 ShowRecord
        (field "id" string)
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
    | Tick Time.Time


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
            ( { model | carouselPosition = model.carouselPosition + 1 }, Cmd.none )


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


showIsInIdList : List String -> ShowRecord -> Bool
showIsInIdList idList show =
    List.member show.id idList


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
            Date.Extra.toFormattedString "MMM ddd" d


futureResume : Date.Date -> Resume -> Resume
futureResume now resume =
    List.filter (showIsInTheFuture now) resume


pastResume : Date.Date -> List String -> Resume -> Resume
pastResume now resumeList resume =
    List.filter (showIsInIdList resumeList) (List.filter (showIsInThePast now) resume)


upcomingShowRowView : ShowRecord -> Html Msg
upcomingShowRowView show =
    tr []
        [ td [] [ text show.character ]
        , td [] [ text show.title ]
        , td [] [ text show.company ]
        , td [] [ text (formatDateString show.start_date ++ " - " ++ formatDateString show.end_date) ]
        , td []
            [ case show.ticket_url of
                Just url ->
                    a [ href url ] [ text "Tickets" ]

                Nothing ->
                    text ""
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
        div [ class "row marketing" ]
            [ div [ class "col-lg-12" ]
                [ h4 [ id "upcoming", class "text-muted" ] [ text "Upcoming" ]
                , div [ class "table-responsive" ]
                    [ div []
                        [ table [ class "table table-hover table-condensed" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Character" ]
                                    , th [] [ text "Title" ]
                                    , th [] [ text "Company" ]
                                    , th [] [ text "Date" ]
                                    , th [] [] -- Tickets
                                    ]
                                ]
                            , tbody [] <|
                                List.map upcomingShowRowView <|
                                    resume
                            ]
                        ]
                    ]
                ]
            ]


pastView : Resume -> Html Msg
pastView resume =
    if List.isEmpty resume then
        Html.text ""
    else
        div [ class "row marketing" ]
            [ div [ class "col-lg-12" ]
                [ h4 [ id "resume", class "text-muted" ] [ text "Resume" ]
                , div [ class "table-responsive" ]
                    [ div []
                        [ table [ class "table table-hover table-condensed" ]
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
                        ]
                    ]
                , p [ class "pull-right" ]
                    [ small [] [ text "*Sung in translation" ]
                    ]
                ]
            ]


photoCreditView : Maybe PhotoCredit -> Html Msg
photoCreditView credit =
    small []
        [ case credit of
            Nothing ->
                text ""

            Just credit ->
                case credit.url of
                    Nothing ->
                        text credit.name

                    Just url ->
                        a [ href url ]
                            [ text credit.name
                            ]
        ]


carouselItemView : Int -> Int -> Photo -> Html Msg
carouselItemView carouselCount index photo =
    div
        [ class
            ("item "
                ++ (if (carouselCount % 3 == index) then
                        "active"
                    else
                        ""
                   )
            )
        ]
        [ img [ src photo.url, alt photo.alt, class "img-responsive img-rounded" ] []
        , photoCreditView photo.credit
        ]


carouselView : Model -> Html Msg
carouselView model =
    div [ class "carousel fade" ]
        [ div [ class "carousel-inner" ] <|
            List.indexedMap (carouselItemView model.carouselPosition) <|
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
                , carouselView model
                , upcomingView (futureResume renderDate model.resume)
                , pastView (pastResume renderDate model.resumeShows model.resume)
                , div [ class "footer marketing" ]
                    [ p [] [ text ("© Jordan Eldredge 2013 - " ++ (toString (Date.year renderDate))) ]
                    ]
                ]

        Nothing ->
            Html.text ""


type alias PhotoCredit =
    { name : String, url : Maybe String }


type alias Photo =
    { url : String
    , alt : String
    , credit : Maybe PhotoCredit
    }


type alias ShowRecord =
    { id : String
    , character : String
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
    , resumeShows : List String
    , renderDate : Maybe Date.Date
    , carouselPosition : Int
    , carouselImages : List Photo
    }


model : Model
model =
    { resume = []
    , resumeShows =
        [ "58"
        , "54"
        , "53"
        , "50"
        , "49"
        , "48"
        , "47"
        , "46"
        , "45"
        , "44"
        , "43"
        , "42"
        , "41"
        , "40"
        , "39"
        , "38"
        , "37"
        , "35"
        , "34"
        , "33"
        , "31"
        ]
    , renderDate = Nothing
    , carouselPosition = 0
    , carouselImages =
        [ { url = "./images/ottone.jpg"
          , alt = "Poppea, San Francisco State University"
          , credit = Nothing
          }
        , { url = "./images/pirate-king.jpg"
          , alt = "Pirates of Penzance, Lyric Theater San Jose, 2012"
          , credit = Just { name = "Bob March", url = Just "http://doverbeach.zenfolio.com/lyr12pirates_dr1_act2/h2cd40783#h2cd40783" }
          }
        , { url = "./images/papageno.jpg"
          , alt = "The Magic Flute, San Francisco State University"
          , credit = Nothing
          }
        , { url = "./images/edward.jpg"
          , alt = "The Witching Hour and a Half, Miscreant's Cabaret"
          , credit = Just { name = "Cody Giannotti", url = Nothing }
          }
        ]
    }
