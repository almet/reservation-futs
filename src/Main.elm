module Main exposing (..)

import Browser
import DateRangePicker as Picker
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (checked, class, colspan, selected, type_, value)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Html.Styled as Styled
import List.Extra
import Random exposing (Seed, initialSeed, step)
import Select
import Time
import Time.Format
import Time.Format.Config.Config_fr_fr exposing (config)
import Utils exposing (..)
import Uuid



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
    , currentSeed : Seed
    , currentUuid : Maybe Uuid.Uuid
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
    { id : Int
    , date : Time.Posix
    , stock : Dict String Int
    }


type alias Reservation =
    { id : Int
    , date : Time.Posix
    , name : String
    , contact : String
    , order : Order
    , tap : Bool
    , notes : String
    , cups : Int
    , done : Bool
    }


type alias Brew =
    { id : Int
    , date : Time.Posix
    , beer : String
    , quantity : Int
    }


type Line
    = BrewWrapper Brew
    | ReservationWrapper Reservation
    | InventoryWrapper Inventory


init : Int -> ( Model, Cmd Msg )
init seed =
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
                1
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
      , brews = [ Brew 2 (Time.millisToPosix 1659020747126) "Nouveau Monde" 75 ]
      , beers = [ "Souffle Tropical", "Nouveau Monde" ]
      , inventories = [ Inventory 3 (Time.millisToPosix 1658415947126) (Dict.fromList [ ( "Souffle Tropical", 10 ), ( "Nouveau Monde", 10 ) ]) ]
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
      , currentSeed = initialSeed seed
      , currentUuid = Nothing
      }
    , Picker.now PickerChanged picker
    )



---- UPDATE ----


type Msg
    = NoOp
    | CreateNewLine
    | PickerChanged Picker.State
    | SelectMsg (Select.Msg LineType)
    | NewUuid
    | BrewUpdateSelectedBeer Int String
    | BrewUpdateQuantity Int String
    | InventoryUpdateQuantity Int String String
    | ReservationUpdateQuantity Int String String
    | ReservationUpdateName Int String
    | ReservationUpdateNotes Int String
    | ReservationUpdateTap Int Bool
    | ReservationUpdateCups Int String
    | ReservationUpdateDone Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUuid ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator model.currentSeed
            in
            ( { model
                | currentUuid = Just newUuid
                , currentSeed = newSeed
              }
            , Cmd.none
            )

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

        BrewUpdateSelectedBeer id selectedBeer ->
            let
                newBrews =
                    model.brews |> List.Extra.updateIf (\x -> x.id == id) (\x -> { x | beer = selectedBeer })
            in
            ( { model | brews = newBrews }, Cmd.none )

        BrewUpdateQuantity id quantityStr ->
            let
                quantity =
                    quantityStr |> String.replace "+" "" |> String.toInt |> Maybe.withDefault 0

                newBrews =
                    model.brews |> List.Extra.updateIf (\x -> x.id == id) (\x -> { x | quantity = quantity })
            in
            ( { model | brews = newBrews }, Cmd.none )

        InventoryUpdateQuantity id beer quantityStr ->
            let
                quantity =
                    quantityStr |> String.replace "=" "" |> String.toInt |> Maybe.withDefault 0

                newInventories =
                    model.inventories |> List.Extra.updateIf (\x -> x.id == id) (\x -> { x | stock = x.stock |> Dict.insert beer quantity })
            in
            ( { model | inventories = newInventories }, Cmd.none )

        ReservationUpdateQuantity id beer quantityStr ->
            let
                quantity =
                    quantityStr |> String.replace "-" "" |> String.toInt |> Maybe.withDefault 0

                newReservations =
                    model.reservations |> List.Extra.updateIf (\x -> x.id == id) (\x -> { x | order = x.order |> Dict.insert beer quantity })
            in
            ( { model | reservations = newReservations }, Cmd.none )

        ReservationUpdateName id name ->
            let
                newReservations =
                    model.reservations |> List.Extra.updateIf (\x -> x.id == id) (\x -> { x | name = name })
            in
            ( { model | reservations = newReservations }, Cmd.none )

        ReservationUpdateNotes id notes ->
            ( { model
                | reservations = model.reservations |> List.Extra.updateIf (\x -> x.id == id) (\x -> { x | notes = notes })
              }
            , Cmd.none
            )

        ReservationUpdateTap id tap ->
            let
                newReservations =
                    model.reservations |> List.Extra.updateIf (\x -> x.id == id) (\x -> { x | tap = tap })
            in
            ( { model | reservations = newReservations }, Cmd.none )

        ReservationUpdateCups id cups ->
            let
                value =
                    cups |> String.toInt |> Maybe.withDefault 0

                newReservations =
                    model.reservations |> List.Extra.updateIf (\x -> x.id == id) (\x -> { x | cups = value })
            in
            ( { model | reservations = newReservations }, Cmd.none )

        ReservationUpdateDone id ->
            let
                newReservations =
                    model.reservations |> List.Extra.updateIf (\x -> x.id == id) (\x -> { x | done = not x.done })
            in
            ( { model | reservations = newReservations }, Cmd.none )

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
                        [ class "button ", onClick CreateNewLine ]
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
                    [ [ "Date", "Nom" ] |> List.map headerLine
                    , model.beers |> List.map (\beer -> th [ class "move", colspan 2 ] [ beer ++ " (dispo)" |> text ])
                    , [ "Notes", "Tireuse", "Gobelets", "Fait" ] |> List.map headerLine
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
                            displayableValue =
                                case inventory.stock |> Dict.get beer of
                                    Nothing ->
                                        ""

                                    Just v ->
                                        "=" ++ String.fromInt v

                            total =
                                totals |> Dict.get beer |> Maybe.withDefault 0 |> String.fromInt
                        in
                        [ td [ class "move" ] [ input [ type_ "text", value displayableValue, onChange (InventoryUpdateQuantity inventory.id beer) ] [] ]
                        , td [ class "total" ] [ "(" ++ total ++ ")" |> text ]
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
              , td []
                    [ select [ onChange (BrewUpdateSelectedBeer brew.id) ]
                        (model.beers
                            |> List.map
                                (\beer ->
                                    let
                                        isSelected =
                                            if beer == brew.beer then
                                                True

                                            else
                                                False
                                    in
                                    option [ value beer, selected isSelected ] [ beer |> text ]
                                )
                        )
                    ]
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
                            [ if beer == brew.beer then
                                input [ type_ "text", value ("+" ++ String.fromInt quantity), onChange (BrewUpdateQuantity brew.id) ] []

                              else
                                "" |> text
                            ]
                        , td [ class "total" ] [ ("(" ++ (totals |> Dict.get beer |> Maybe.withDefault 0 |> String.fromInt) ++ ")") |> text ]
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
              , td [] [ input [ type_ "text", value reservation.name, onChange (ReservationUpdateName reservation.id) ] [] ]
              ]
            , model.beers
                |> List.map
                    (\beer ->
                        let
                            stringValue =
                                case reservation.order |> Dict.get beer of
                                    Nothing ->
                                        ""

                                    Just 0 ->
                                        "0"

                                    Just v ->
                                        String.fromInt v |> (++) "-"

                            total =
                                totals |> Dict.get beer |> Maybe.withDefault 0 |> String.fromInt
                        in
                        [ td [ class "move" ] [ input [ type_ "text", value stringValue, onChange (ReservationUpdateQuantity reservation.id beer) ] [] ]
                        , td [ class "total" ] [ "(" ++ total ++ ")" |> text ]
                        ]
                    )
                |> List.concat
            , [ td []
                    [ input [ type_ "text", value reservation.notes, onChange (ReservationUpdateNotes reservation.id) ] [] ]
              , td []
                    [ text
                        (if reservation.tap then
                            "Oui"

                         else
                            "Non"
                        )
                    ]
              , td []
                    [ let
                        cups =
                            case reservation.cups of
                                0 ->
                                    "Non"

                                _ ->
                                    reservation.cups |> String.fromInt
                      in
                      input [ type_ "text", value cups, onChange (ReservationUpdateCups reservation.id) ] []
                    ]
              , td []
                    [ input [ type_ "checkbox", checked reservation.done, onClick (ReservationUpdateDone reservation.id) ] [] ]
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


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
