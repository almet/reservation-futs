module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, colspan)
import Time
import Time.Format
import Time.Format.Config.Config_fr_fr exposing (config)



---- MODEL ----


type alias Model =
    { reservations : List Reservation
    , brews : List Brew
    , beers : List String
    , inventories : List Inventory
    }


type alias Order =
    Dict String Int


type alias Inventory =
    { date : Time.Posix
    , stock : Dict String Int
    }


type alias Reservation =
    { date : Time.Posix
    , person : String
    , contact : String
    , order : Order
    , tap : Bool
    , notes : String
    , cups : Int
    , done : Bool
    }


type alias Brew =
    { date : Time.Posix
    , beer : String
    , quantity : Int
    }


type Line
    = BrewWrapper Brew
    | ReservationWrapper Reservation
    | InventoryWrapper Inventory


init : ( Model, Cmd Msg )
init =
    ( { reservations =
            [ Reservation
                (Time.millisToPosix 0)
                "Alexis"
                "0766554900"
                (Dict.fromList
                    [ ( "Souffle Tropical", 2 ), ( "Nouveau Monde", 4 ) ]
                )
                False
                "Notes"
                0
                False
            ]
      , brews = [ Brew (Time.millisToPosix 1000) "Souffle Tropical" 75 ]
      , beers = [ "Souffle Tropical", "Nouveau Monde" ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


combineAndSort : List Brew -> List Reservation -> List Line
combineAndSort brews reservations =
    let
        combined =
            List.map BrewWrapper brews ++ List.map ReservationWrapper reservations

        sorter item =
            case item of
                BrewWrapper brew ->
                    brew.date |> Time.posixToMillis

                ReservationWrapper reservation ->
                    reservation.date |> Time.posixToMillis
    in
    List.sortBy sorter combined



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ "Suivi des réservations de fûts" |> text ]
        , viewReservationTable model
        ]


viewReservationTable : Model -> Html Msg
viewReservationTable model =
    let
        headerLine header =
            th [] [ header |> text ]
    in
    table [ class "table" ]
        [ thead []
            [ tr []
                (List.concat
                    [ [ "Date", "Personne", "Notes" ]
                    , model.beers
                    , [ "Tireuse ?", "Gobelets", "Fait" ]
                    ]
                    |> List.map headerLine
                )
            ]
        , tbody []
            (combineAndSort model.brews model.reservations |> List.map (viewLine model))
        ]


viewLine : Model -> Line -> Html Msg
viewLine model line =
    case line of
        BrewWrapper brew ->
            viewBrewLine model brew

        ReservationWrapper reservation ->
            viewReservationLine model reservation


viewBrewLine : Model -> Brew -> Html Msg
viewBrewLine model brew =
    tr [ class "mise-en-futs" ]
        (List.concat
            [ [ td [] [ brew.date |> formatDate |> text ]
              , td [] [ "Mise en fûts" |> text ]
              , td [] [ brew.beer |> text ]
              ]
            , model.beers
                |> List.map
                    (\b ->
                        if b == brew.beer then
                            brew.quantity

                        else
                            0
                    )
                |> List.map (\x -> td [] [ x |> String.fromInt |> (++) "+" |> text ])
            , [ td [ colspan 4 ] [] ]
            ]
        )


viewReservationLine : Model -> Reservation -> Html Msg
viewReservationLine model reservation =
    tr []
        (List.concat
            [ [ td [] [ reservation.date |> formatDate |> text ]
              , td [] [ reservation.person |> text ]
              , td [] [ reservation.notes |> text ]
              ]
            , model.beers
                |> List.map (\beer -> reservation.order |> Dict.get beer)
                |> List.map
                    (\value ->
                        td []
                            [ case value of
                                Nothing ->
                                    text ""

                                Just v ->
                                    String.fromInt v |> (++) "-" |> text
                            ]
                    )
            , [ td []
                    [ text
                        (if reservation.tap then
                            "Oui"

                         else
                            "Non"
                        )
                    ]
              , td []
                    [ case reservation.cups of
                        0 ->
                            text "Non"

                        _ ->
                            reservation.cups |> String.fromInt |> text
                    ]
              , td []
                    [ case reservation.done of
                        True ->
                            text "✓"

                        _ ->
                            text ""
                    ]
              ]
            ]
        )


formatDate : Time.Posix -> String
formatDate date =
    Time.Format.format config "%-d %B %Y" Time.utc date



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
