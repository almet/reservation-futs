module Main exposing (..)

import Browser
import DateRangePicker as Picker
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, colspan)
import Html.Events exposing (onClick)
import Html.Styled as Styled
import Select
import Time
import Time.Format
import Time.Format.Config.Config_fr_fr exposing (config)



---- MODEL ----


type alias Model =
    { reservations : List Reservation
    , brews : List Brew
    , beers : List String
    , inventories : List Inventory
    , datePicker : Picker.State
    , selectState : Select.State
    , items : List (Select.MenuItem LineType)
    , selectedLineType : Maybe LineType
    }


type alias NewLine =
    { date : Time.Posix
    , lineType : Maybe LineType
    }


type LineType
    = LineReservation
    | LineInventory
    | LineBrew


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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        pickerConfig =
            Picker.configure
                (\default ->
                    { default
                        | translations =
                            { close = "Fermer"
                            , clear = "Effacer"
                            , apply = "Valider"
                            , pickStart = "Selectionnez la date de l'évènement"
                            , pickEnd = "Selectionnez la date de retour"
                            }
                        , noRangeCaption = "Selectionnez une date"
                        , weekdayFormatter = weekdayToString
                        , monthFormatter = monthToString

                        --, monthFormatter = monthFormatter
                    }
                )

        picker =
            Picker.init pickerConfig Nothing
    in
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
      , inventories = [ Inventory (Time.millisToPosix 500) (Dict.fromList [ ( "Souffle Tropical", 10 ), ( "Nouveau Monde", 10 ) ]) ]
      , selectState = Select.initState
      , items =
            [ Select.basicMenuItem
                { item = LineReservation, label = "Réservation de fûts" }
            , Select.basicMenuItem
                { item = LineBrew, label = "Enfûtage" }
            , Select.basicMenuItem
                { item = LineInventory, label = "Inventaire" }
            ]
      , selectedLineType = Just LineReservation
      , datePicker = picker
      }
    , Picker.now PickerChanged picker
    )


weekdayToString : Time.Weekday -> String
weekdayToString day =
    case day of
        Time.Sun ->
            "Dim"

        Time.Mon ->
            "Lu"

        Time.Tue ->
            "Ma"

        Time.Wed ->
            "Me"

        Time.Thu ->
            "Je"

        Time.Fri ->
            "Ve"

        Time.Sat ->
            "Sa"


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Janvier"

        Time.Feb ->
            "Février"

        Time.Mar ->
            "Mars"

        Time.Apr ->
            "Avril"

        Time.May ->
            "Mai"

        Time.Jun ->
            "Juin"

        Time.Jul ->
            "Juillet"

        Time.Aug ->
            "Août"

        Time.Sep ->
            "Septembre"

        Time.Oct ->
            "Octobre"

        Time.Nov ->
            "Novembre"

        Time.Dec ->
            "Décembre"



---- UPDATE ----


type Msg
    = NoOp
    | CreateNewLine
    | PickerChanged Picker.State
    | SelectMsg (Select.Msg LineType)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateNewLine ->
            ( model, Cmd.none )

        PickerChanged state ->
            ( { model | datePicker = state }, Cmd.none )

        SelectMsg selectMsg ->
            let
                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg model.selectState

                updatedLineType =
                    case maybeAction of
                        Just (Select.Select lineType) ->
                            Just lineType

                        Just Select.ClearSingleSelectItem ->
                            Nothing

                        _ ->
                            model.selectedLineType
            in
            ( { model
                | selectState = updatedSelectState
                , selectedLineType = updatedLineType
              }
            , Cmd.map SelectMsg selectCmds
            )

        NoOp ->
            ( model, Cmd.none )


combineAndSort : List Brew -> List Reservation -> List Inventory -> List Line
combineAndSort brews reservations inventories =
    let
        combined =
            List.map BrewWrapper brews ++ List.map ReservationWrapper reservations ++ List.map InventoryWrapper inventories

        sorter item =
            case item of
                BrewWrapper brew ->
                    brew.date |> Time.posixToMillis

                ReservationWrapper reservation ->
                    reservation.date |> Time.posixToMillis

                InventoryWrapper inventory ->
                    inventory.date |> Time.posixToMillis
    in
    List.sortBy sorter combined



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ "Suivi des réservations de fûts" |> text ]
        , div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "column" ] [ Picker.view PickerChanged model.datePicker ]
                , div [ class "column" ] [ Html.map SelectMsg (Styled.toUnstyled <| renderSelect model) ]
                , div [ class "column" ]
                    [ a
                        [ class "button", onClick CreateNewLine ]
                        [ "Ajouter une commande" |> text ]
                    ]
                ]
            ]
        , viewReservationTable model
        ]


selectedLineTypeToMenuItem : LineType -> Select.MenuItem LineType
selectedLineTypeToMenuItem lineType =
    case lineType of
        LineBrew ->
            Select.basicMenuItem { item = LineBrew, label = "Enfûtage" }

        LineReservation ->
            Select.basicMenuItem { item = LineReservation, label = "Réservation de fûts" }

        LineInventory ->
            Select.basicMenuItem { item = LineInventory, label = "Inventaire" }


renderSelect : Model -> Styled.Html (Select.Msg LineType)
renderSelect model =
    Select.view
        ((Select.single <| Maybe.map selectedLineTypeToMenuItem model.selectedLineType)
            |> Select.state model.selectState
            |> Select.menuItems model.items
            |> Select.placeholder "Quel type d'info ?"
        )
        (Select.selectIdentifier "LineTypeSelector")


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
            (combineAndSort model.brews model.reservations model.inventories |> List.map (viewLine model))
        ]


viewLine : Model -> Line -> Html Msg
viewLine model line =
    case line of
        BrewWrapper brew ->
            viewBrewLine model brew

        ReservationWrapper reservation ->
            viewReservationLine model reservation

        InventoryWrapper inventory ->
            viewInventoryLine model inventory


viewInventoryLine : Model -> Inventory -> Html Msg
viewInventoryLine model inventory =
    tr [ class "inventaire" ]
        (List.concat
            [ [ td [] [ inventory.date |> formatDate |> text ]
              , td [] [ "Inventaire" |> text ]
              , td [] [ "" |> text ]
              ]
            , model.beers
                |> List.map (\beer -> inventory.stock |> Dict.get beer)
                |> List.map
                    (\value ->
                        td []
                            [ case value of
                                Nothing ->
                                    text ""

                                Just v ->
                                    String.fromInt v |> (++) "=" |> text
                            ]
                    )
            , [ td [ colspan 4 ] [] ]
            ]
        )


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


subscriptions : Model -> Sub Msg
subscriptions { datePicker } =
    Picker.subscriptions PickerChanged datePicker



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
