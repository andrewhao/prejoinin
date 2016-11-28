{-
   MovieInfo - Information and ratings for Movies, TV Series and games
   Copyright (C) 2016 Siavash Askari Nasr

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}


port module Main exposing (..)

import Html exposing (Html, header, main_, article, div, p, span, a, img, text, strong, hr, h1, form, input, button)
import Html.Attributes as Attr exposing (href, class, src, type_, placeholder, value, autofocus)
import Html.Events exposing (onClick, onInput, onSubmit)
import Navigation
import Hop exposing (makeUrl, makeUrlFromLocation, matchUrl)
import Hop.Types
import Hop.Matchers
import Http
import Json.Decode as Json
import Json.Decode.Pipeline as JsonPipeline
import Task
import String exposing (toLower)
import String.Extra exposing (capitalize)
import Regex
import Dict


-- ROUTES


type Route
    = MainRoute
    | SearchRoute String
    | TitleRoute String
    | NotFoundRoute


matchers : List (Hop.Types.PathMatcher Route)
matchers =
    [ Hop.Matchers.match1 MainRoute ""
    , Hop.Matchers.match2 SearchRoute "/search/" Hop.Matchers.str
    , Hop.Matchers.match2 TitleRoute "/title/" Hop.Matchers.str
    ]


routerConfig : Hop.Types.Config Route
routerConfig =
    { hash = True
    , basePath = ""
    , matchers = matchers
    , notFound = NotFoundRoute
    }



-- MESSAGES


type Msg
    = NoOp
    | NavigateTo String
    | SearchFetchSuccess Search
    | TitleFetchSuccess Title
    | FetchFailure Http.Error
    | UpdateSearchQuery String
    | UpdateSearchYear String



-- MODEL


type alias Model =
    { route : Route
    , location : Hop.Types.Location
    , loading : Bool
    , fetchFailed : Bool
    , query : String
    , year : String
    , results : Results
    , title : Maybe Title
    }


type alias Result =
    { title : String
    , year : String
    , imdbID : String
    , type_ : String
    , poster : String
    }


type alias Results =
    List Result


type alias Search =
    { search : Results }


type alias Title =
    { title : String
    , year : String
    , mpaaRating : String
    , releaseDate : String
    , duration : String
    , genre : String
    , directors : String
    , writers : String
    , stars : String
    , plot : String
    , language : String
    , country : String
    , awards : String
    , poster : String
    , metascore : String
    , imdbRating : String
    , imdbVotes : String
    , imdbID : String
    , type_ : String
    , tomatoMeter : String
    , tomatoReviews : String
    , tomatoUserMeter : String
    , tomatoUserReviews : String
    , tomatoURL : String
    , production : String
    , website : String
    , seasons : String
    }


port setPageTitle : String -> Cmd msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        NavigateTo path ->
            let
                command =
                    Navigation.newUrl <| makeUrl routerConfig path
            in
                model ! [ command ]

        SearchFetchSuccess { search } ->
            { model | results = search, loading = False } ! []

        TitleFetchSuccess title ->
            { model | title = Just title, loading = False }
                ! [ setPageTitle <| concatTitleYear title.title title.year ]

        FetchFailure error ->
            let
                model =
                    { model | fetchFailed = True, loading = False }

                cmd =
                    [ setPageTitle "" ]
            in
                case error of
                    Http.UnexpectedPayload _ ->
                        { model | fetchFailed = False } ! cmd

                    _ ->
                        model ! cmd

        UpdateSearchQuery query ->
            { model | query = query } ! []

        UpdateSearchYear year ->
            { model | year = year } ! []


urlParser : Navigation.Parser ( Route, Hop.Types.Location )
urlParser =
    Navigation.makeParser (.href >> matchUrl routerConfig)


urlUpdate : ( Route, Hop.Types.Location ) -> Model -> ( Model, Cmd Msg )
urlUpdate ( route, location ) model =
    let
        model =
            initialModel ( route, location )
    in
        case route of
            SearchRoute q ->
                let
                    year =
                        location.query
                            |> Dict.get "year"
                            |> Maybe.withDefault ""
                            |> Http.uriDecode

                    query =
                        Http.uriDecode q
                in
                    { model | query = query, year = year, loading = True }
                        ! [ searchTitle query year
                          , setPageTitle <| "Search " ++ query
                          ]

            TitleRoute id ->
                { model | loading = True } ! [ getTitle id ]

            NotFoundRoute ->
                model ! [ setPageTitle "Page Not Found" ]

            _ ->
                model ! [ setPageTitle "" ]


request : List ( String, String ) -> ( Http.Error -> a, b -> a ) -> Json.Decoder b -> Cmd a
request params ( fetchFailure, fetchSuccess ) decoder =
    let
        baseUrl =
            "https://www.omdbapi.com/?v=1&"

        query =
            params
                |> List.map (\( key, value ) -> String.join "=" [ key, value ])
                |> String.join "&"

        url =
            baseUrl ++ query
    in
        Task.perform fetchFailure fetchSuccess (Http.get decoder url)


searchTitle : String -> String -> Cmd Msg
searchTitle title year =
    request [ ( "s", title ), ( "y", year ) ] ( FetchFailure, SearchFetchSuccess ) decodeSearch


decodeResults : Json.Decoder Results
decodeResults =
    Json.list decodeResult


decodeResult : Json.Decoder Result
decodeResult =
    JsonPipeline.decode Result
        |> JsonPipeline.required "Title" Json.string
        |> JsonPipeline.required "Year" Json.string
        |> JsonPipeline.required "imdbID" Json.string
        |> JsonPipeline.required "Type" Json.string
        |> JsonPipeline.required "Poster" Json.string


decodeSearch : Json.Decoder Search
decodeSearch =
    JsonPipeline.decode Search
        |> JsonPipeline.required "Search" decodeResults


getTitle : String -> Cmd Msg
getTitle id =
    request
        [ ( "plot", "full" )
        , ( "tomatoes", "true" )
        , ( "i", id )
        ]
        ( FetchFailure, TitleFetchSuccess )
        decodeTitle


decodeTitle : Json.Decoder Title
decodeTitle =
    JsonPipeline.decode Title
        |> JsonPipeline.required "Title" Json.string
        |> JsonPipeline.required "Year" Json.string
        |> JsonPipeline.required "Rated" Json.string
        |> JsonPipeline.required "Released" Json.string
        |> JsonPipeline.required "Runtime" Json.string
        |> JsonPipeline.required "Genre" Json.string
        |> JsonPipeline.required "Director" Json.string
        |> JsonPipeline.required "Writer" Json.string
        |> JsonPipeline.required "Actors" Json.string
        |> JsonPipeline.required "Plot" Json.string
        |> JsonPipeline.required "Language" Json.string
        |> JsonPipeline.required "Country" Json.string
        |> JsonPipeline.required "Awards" Json.string
        |> JsonPipeline.required "Poster" Json.string
        |> JsonPipeline.required "Metascore" Json.string
        |> JsonPipeline.required "imdbRating" Json.string
        |> JsonPipeline.required "imdbVotes" Json.string
        |> JsonPipeline.required "imdbID" Json.string
        |> JsonPipeline.required "Type" Json.string
        |> JsonPipeline.required "tomatoMeter" Json.string
        |> JsonPipeline.required "tomatoReviews" Json.string
        |> JsonPipeline.required "tomatoUserMeter" Json.string
        |> JsonPipeline.required "tomatoUserReviews" Json.string
        |> JsonPipeline.required "tomatoURL" Json.string
        |> JsonPipeline.required "Production" Json.string
        |> JsonPipeline.required "Website" Json.string
        |> JsonPipeline.optional "totalSeasons" Json.string ""



-- VIEWS


view : Model -> Html Msg
view model =
    div [ class "container" ] <| viewPage model


viewPage : Model -> List (Html Msg)
viewPage model =
    let
        pageContent =
            viewPageContent model
    in
        case model.route of
            MainRoute ->
                [ div [ class "home" ]
                    [ main_ [ class "home-main" ] [ viewLogo, viewSearchForm model.query model.year True ]
                    , div [ class "home-footer small" ]
                        [ a [ href "https://gitlab.com/CIAvash/MovieInfo" ] [ text "MovieInfo" ]
                        , text " is Free/Libre Software created by "
                        , a [ href "http://ciavash.name" ] [ text "Siavash Askari Nasr" ]
                        , text ". It uses "
                        , a [ href "https://omdbapi.com/" ] [ text "OMDb API" ]
                        , text " for obtaining data."
                        ]
                    ]
                ]

            SearchRoute query ->
                pageContent <|
                    div [ class "search-results" ] <|
                        if List.length model.results > 0 then
                            List.map viewResult model.results
                        else if not model.loading then
                            [ text "Could not find any titles" ]
                        else
                            []

            TitleRoute id ->
                pageContent <| viewTitle model.title

            NotFoundRoute ->
                pageContent <| div [] [ text "Page Not found" ]


viewPageContent : Model -> Html Msg -> List (Html Msg)
viewPageContent model content =
    [ header [ class "page-header" ] [ viewLogo, viewSearchForm model.query model.year False ]
    , p [] <|
        if model.loading then
            [ text "Loading..." ]
        else
            []
    , main_ []
        [ if model.fetchFailed then
            p [] [ text "An error occurred while fetching data" ]
          else
            content
        ]
    ]


viewLogo : Html Msg
viewLogo =
    a [ href <| makeUrl routerConfig "/" ] [ h1 [ class "logo" ] [ text "MovieInfo" ] ]


viewSearchForm : String -> String -> Bool -> Html Msg
viewSearchForm query year homeSearchForm =
    form
        [ class "search-form", onSubmit <| submitSearch query year ]
        [ input
            [ class "search-form-query"
            , autofocus homeSearchForm
            , type_ "search"
            , placeholder "Title"
            , value query
            , onInput UpdateSearchQuery
            ]
            []
        , input
            [ class "search-form-year"
            , type_ "text"
            , placeholder "Year"
            , value year
            , onInput UpdateSearchYear
            ]
            []
        , button [ class "search-form-btn btn btn-info", type_ "submit" ] [ text "Search" ]
        ]


submitSearch : String -> String -> Msg
submitSearch query year =
    let
        query =
            query
                |> String.trim
                |> rmExtraSpaces

        year =
            String.trim year
    in
        if not <| String.isEmpty query then
            NavigateTo <|
                "/search/"
                    ++ query
                    ++ if not <| String.isEmpty year then
                        "?year=" ++ year
                       else
                        ""
        else
            NoOp


viewResult : Result -> Html Msg
viewResult result =
    a
        [ class "search-result"
        , Attr.title <| concatTitleYear result.title result.year
        , href <| makeUrl routerConfig <| "/title/" ++ result.imdbID
        ]
        [ viewPoster result.poster
        , div [ class "search-result-title" ] [ text result.title ]
        , span [ class "search-result-type" ] [ text <| capitalize True result.type_ ]
        , span [ class "search-result-year" ] [ text result.year ]
        ]


viewTitle : Maybe Title -> Html Msg
viewTitle title =
    case title of
        Just title ->
            article [ class "title" ]
                [ header []
                    [ h1 [] [ text <| concatTitleYear title.title title.year ]
                    , div [ class "infobar text-muted" ] <| viewTitleInfoBar title
                    ]
                , div [ class "row" ]
                    [ div [ class "col-sm-12 col-md-8 col-lg-7" ]
                        [ viewPoster title.poster
                        , div [ class "all-ratings" ] <| viewTitleRatings title
                        ]
                    , div [ class "col-sm-12 col-md-4 col-lg-5" ]
                        [ p [] <|
                            if isValid title.plot then
                                [ text title.plot, hr [] [] ]
                            else
                                []
                        , div [] <| viewTitleDetails title
                        ]
                    ]
                ]

        Nothing ->
            div [] []


viewTitleInfoBar : Title -> List (Html Msg)
viewTitleInfoBar title =
    let
        validInfo =
            List.filter (\info -> isValid info)
                [ (capitalize True title.type_)
                , title.mpaaRating
                , title.duration
                , title.genre
                , title.releaseDate
                ]
    in
        List.map (\info -> span [] [ text info ]) validInfo


viewTitleRatings : Title -> List (Html Msg)
viewTitleRatings title =
    let
        tomatoUrl =
            if isValid title.tomatoURL then
                title.tomatoURL
            else
                makeTomatoUrl title.title

        titleRatings =
            [ ( "IMDB", "votes", title.imdbRating, title.imdbVotes, makeIMDBUrl title.imdbID )
            , ( "Tomatometer", "reviews", title.tomatoMeter, title.tomatoReviews, tomatoUrl )
            , ( "Audience", "ratings", title.tomatoUserMeter, title.tomatoUserReviews, tomatoUrl )
            , ( "Metascore", "", title.metascore, "", makeMetacriticUrl title.title title.year )
            ]

        validTitleRatings =
            List.filter (\( _, _, rating, _, _ ) -> isValid rating) titleRatings
    in
        [ div [ class "rating average-rating" ]
            [ span [ class "rating-source" ] [ text "Overall" ]
            , span [ class "rating-number" ]
                [ span [ class "rating-circle" ]
                    [ if List.length validTitleRatings > 0 then
                        text <| toString <| averageTitleRating titleRatings
                      else
                        text "N/A"
                    ]
                ]
            ]
        , div [ class "ratings" ] <| List.map viewTitleRating titleRatings
        ]


viewTitleRating : ( String, String, String, String, String ) -> Html Msg
viewTitleRating ( source, ratingWord, rating, votes, url ) =
    div [ class <| "rating rating-" ++ source ]
        [ div []
            [ span [ class "rating-source" ] <|
                if isValid url then
                    [ a [ href url ] [ text source ] ]
                else
                    [ text source ]
            , span [ class "rating-number" ] [ span [ class "rating-circle" ] [ text rating ] ]
            ]
        , div [ class "rating-description" ]
            [ text <|
                if isValid votes then
                    "from " ++ votes ++ " " ++ ratingWord
                else
                    ""
            ]
        ]


averageTitleRating : List ( String, String, String, String, String ) -> Int
averageTitleRating ratings =
    let
        numbers =
            convertRatingsToFloats ratings

        sum =
            List.sum numbers
    in
        round <| sum / toFloat (List.length numbers)


convertRatingsToFloats : List ( String, String, String, String, String ) -> List Float
convertRatingsToFloats ratings =
    List.filterMap convertRatingToMaybeFloat ratings


convertRatingToMaybeFloat : ( String, String, String, String, String ) -> Maybe Float
convertRatingToMaybeFloat ( source, _, rating, _, _ ) =
    let
        ratingNum =
            rating
                |> String.toFloat
                |> Result.toMaybe
    in
        case ratingNum of
            Just num ->
                if source == "IMDB" then
                    Just <| num * 10
                else
                    Just num

            _ ->
                Nothing


viewTitleDetails : Title -> List (Html Msg)
viewTitleDetails title =
    let
        validTitleDetails =
            List.filter (\( _, detail ) -> isValid detail)
                [ ( "Directors", title.directors )
                , ( "Writers", title.writers )
                , ( "Stars", title.stars )
                , ( "Seasons", title.seasons )
                , ( "Awards", title.awards )
                , ( "Country", title.country )
                , ( "Language", title.language )
                , ( "Production", title.production )
                , ( "Website", title.website )
                ]
    in
        List.map viewTitleDetail validTitleDetails


viewTitleDetail : ( String, String ) -> Html Msg
viewTitleDetail ( detailName, info ) =
    let
        name =
            strong [] [ text <| detailName ++ ": " ]
    in
        p [] <|
            if detailName == "Website" then
                [ name, linkify info ]
            else
                [ name, text info ]


viewPoster : String -> Html Msg
viewPoster imgUrl =
    if isValid imgUrl then
        img [ class "poster", src imgUrl ] []
    else
        span [ class "poster empty-poster" ] []



-- HELPERS


isValid : String -> Bool
isValid str =
    let
        str =
            toLower str
    in
        str /= "n/a" && str /= ""


rmExtraSpaces : String -> String
rmExtraSpaces =
    Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> " ")


makeIMDBUrl : String -> String
makeIMDBUrl imdbID =
    "http://imdb.com/title/" ++ imdbID


makeTomatoUrl : String -> String
makeTomatoUrl title =
    "https://www.rottentomatoes.com/search/?search=" ++ title


makeMetacriticUrl : String -> String -> String
makeMetacriticUrl title year =
    let
        dateRange =
            "01-01-" ++ year
    in
        "http://www.metacritic.com/search/all/"
            ++ title
            ++ "/results?date_range_from="
            ++ dateRange
            ++ "&search_type=advanced"


concatTitleYear : String -> String -> String
concatTitleYear title year =
    title ++ " (" ++ year ++ ")"


linkify : String -> Html Msg
linkify url =
    a [ href url ] [ text url ]



-- INIT


initialModel : ( Route, Hop.Types.Location ) -> Model
initialModel ( route, location ) =
    Model route location False False "" "" [] Nothing


init : ( Route, Hop.Types.Location ) -> ( Model, Cmd Msg )
init ( route, location ) =
    urlUpdate ( route, location ) <| initialModel ( route, location )



-- MAIN


main : Program Never
main =
    Navigation.program urlParser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = (always Sub.none)
        }
