module Main exposing (..)

import Browser
import DateRangePicker as Picker
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, colspan)
import Html.Events exposing (onClick)
import Html.Styled as Styled
import List.Extra
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
                (Time.millisToPosix 1659625547126)
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
      , brews = [ Brew (Time.millisToPosix 1659020747126) "Souffle Tropical" 75 ]
      , beers = [ "Souffle Tropical", "Nouveau Monde" ]
      , inventories = [ Inventory (Time.millisToPosix 1658415947126) (Dict.fromList [ ( "Souffle Tropical", 10 ), ( "Nouveau Monde", 10 ) ]) ]
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


computeTotals : List String -> List Line -> List (Dict String Int)
computeTotals beers lines =
    let
        firstAccValue =
            beers |> List.map (\beer -> ( beer, 0 )) |> Dict.fromList

        accumulator currentLine previousLine =
            case currentLine of
                BrewWrapper brew ->
                    previousLine
                        |> Dict.map
                            (\key previousValue ->
                                (+) previousValue
                                    (if brew.beer == key then
                                        brew.quantity

                                     else
                                        0
                                    )
                            )

                ReservationWrapper reservation ->
                    previousLine |> Dict.map (\key previousValue -> (-) previousValue (Dict.get key reservation.order |> Maybe.withDefault 0))

                InventoryWrapper inventory ->
                    previousLine |> Dict.map (\key previousValue -> Dict.get key inventory.stock |> Maybe.withDefault previousValue)

        totals =
            List.Extra.scanl accumulator firstAccValue lines |> List.tail
    in
    case totals of
        Nothing ->
            []

        Just values ->
            values



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


renderSelect : Model -> Styled.Html (Select.Msg LineType)
renderSelect model =
    Select.view
        ((Select.single <| Maybe.map selectedLineTypeToMenuItem model.selectedLineType)
            |> Select.state model.selectState
            |> Select.menuItems model.items
            |> Select.placeholder "Quel type d'info ?"
        )
        (Select.selectIdentifier "LineTypeSelector")


selectedLineTypeToMenuItem : LineType -> Select.MenuItem LineType
selectedLineTypeToMenuItem lineType =
    case lineType of
        LineBrew ->
            Select.basicMenuItem { item = LineBrew, label = "Enfûtage" }

        LineReservation ->
            Select.basicMenuItem { item = LineReservation, label = "Réservation de fûts" }

        LineInventory ->
            Select.basicMenuItem { item = LineInventory, label = "Inventaire" }


viewReservationTable : Model -> Html Msg
viewReservationTable model =
    let
        headerLine header =
            th [] [ header |> text ]

        lines =
            combineAndSort model.brews model.reservations model.inventories

        linesWithTotals =
            List.map2 Tuple.pair lines (computeTotals model.beers lines)
    in
    table [ class "table" ]
        [ thead []
            [ tr []
                (List.concat
                    [ [ "Date", "Personne" ] |> List.map headerLine
                    , model.beers |> List.map (\beer -> th [ class "move", colspan 2 ] [ text beer ])
                    , [ "Notes", "Tireuse ?", "Gobelets", "Fait" ] |> List.map headerLine
                    ]
                )
            ]
        , tbody []
            (linesWithTotals |> List.map (viewLine model))
        ]


viewLine : Model -> ( Line, Dict String Int ) -> Html Msg
viewLine model ( line, totals ) =
    case line of
        BrewWrapper brew ->
            viewBrewLine model brew totals

        ReservationWrapper reservation ->
            viewReservationLine model reservation totals

        InventoryWrapper inventory ->
            viewInventoryLine model inventory totals


viewInventoryLine : Model -> Inventory -> Dict String Int -> Html Msg
viewInventoryLine model inventory totals =
    tr [ class "inventaire" ]
        (List.concat
            [ [ td [] [ inventory.date |> formatDate |> text ]
              , td [] [ "Inventaire" |> text ]
              ]
            , model.beers
                |> List.map
                    (\beer ->
                        let
                            value =
                                inventory.stock |> Dict.get beer
                        in
                        [ td [ class "move" ]
                            [ case value of
                                Nothing ->
                                    text ""

                                Just v ->
                                    String.fromInt v |> (++) "=" |> text
                            ]
                        , td [ class "total" ] [ totals |> Dict.get beer |> Maybe.withDefault 0 |> String.fromInt |> text ]
                        ]
                    )
                |> List.concat
            , [ td [ colspan 6 ] [] ]
            ]
        )


viewBrewLine : Model -> Brew -> Dict String Int -> Html Msg
viewBrewLine model brew totals =
    tr [ class "mise-en-futs" ]
        (List.concat
            [ [ td [] [ brew.date |> formatDate |> text ]
              , td [] [ "Mise en fûts " ++ brew.beer |> text ]
              ]
            , model.beers
                |> List.map
                    (\beer ->
                        let
                            quantity =
                                if beer == brew.beer then
                                    brew.quantity

                                else
                                    0
                        in
                        [ td [ class "move" ]
                            [ case quantity of
                                0 ->
                                    text ""

                                _ ->
                                    quantity |> String.fromInt |> (++) "+" |> text
                            ]
                        , td [ class "total" ] [ totals |> Dict.get beer |> Maybe.withDefault 0 |> String.fromInt |> text ]
                        ]
                    )
                |> List.concat
            , [ td [ colspan 6 ] [] ]
            ]
        )


viewReservationLine : Model -> Reservation -> Dict String Int -> Html Msg
viewReservationLine model reservation totals =
    tr []
        (List.concat
            [ [ td [] [ reservation.date |> formatDate |> text ]
              , td [] [ reservation.person |> text ]
              ]
            , model.beers
                |> List.map
                    (\beer ->
                        let
                            value =
                                reservation.order |> Dict.get beer
                        in
                        [ td [ class "move" ]
                            [ case value of
                                Nothing ->
                                    text ""

                                Just v ->
                                    String.fromInt v |> (++) "-" |> text
                            ]
                        , td [ class "total" ] [ totals |> Dict.get beer |> Maybe.withDefault 0 |> String.fromInt |> text ]
                        ]
                    )
                |> List.concat
            , [ td []
                    [ reservation.notes |> text ]
              , td []
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
