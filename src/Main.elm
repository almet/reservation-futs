module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Html.Keyed as Keyed
import Html.Styled as Styled
import List.Extra
import Random exposing (Seed, initialSeed, step)
import Time
import Utils exposing (..)
import Uuid



---- MODEL ----


type alias Model =
    { reservations : List Reservation
    , brews : List Brew
    , beers : List String
    , inventories : List Inventory
    , currentSeed : Seed
    , currentUuid : Maybe Uuid.Uuid
    , displayNewLineSelect : Bool
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
      , currentSeed = initialSeed seed
      , currentUuid = Nothing
      , displayNewLineSelect = False
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | NewUuid
    | DisplayNewLineSelect Bool
    | CreateNewLine String
    | BrewUpdateDate Int String
    | BrewUpdateSelectedBeer Int String
    | ReservationUpdateDate Int String
    | BrewUpdateQuantity Int String
    | InventoryUpdateDate Int String
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
                    Random.step Uuid.uuidGenerator model.currentSeed
            in
            ( { model
                | currentUuid = Just newUuid
                , currentSeed = newSeed
              }
            , Cmd.none
            )

        DisplayNewLineSelect value ->
            ( { model | displayNewLineSelect = value }, Cmd.none )

        CreateNewLine lineType ->
            --case lineType of
            --    "Enfûtage" -> {model | brews = model.brews ++ [Brew ]}
            ( model, Cmd.none )

        BrewUpdateSelectedBeer id selectedBeer ->
            ( { model
                | brews =
                    model.brews
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | beer = selectedBeer })
              }
            , Cmd.none
            )

        BrewUpdateQuantity id quantityStr ->
            ( { model
                | brews =
                    model.brews
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | quantity = quantityStr |> String.replace "+" "" |> String.toInt |> Maybe.withDefault 0 })
              }
            , Cmd.none
            )

        BrewUpdateDate id date ->
            ( { model
                | brews =
                    model.brews
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | date = date |> dateToPosix })
              }
            , Cmd.none
            )

        InventoryUpdateQuantity id beer quantityStr ->
            let
                quantity =
                    quantityStr |> String.replace "=" "" |> String.toInt |> Maybe.withDefault 0
            in
            ( { model
                | inventories =
                    model.inventories
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | stock = x.stock |> Dict.insert beer quantity })
              }
            , Cmd.none
            )

        InventoryUpdateDate id date ->
            ( { model
                | inventories =
                    model.inventories
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | date = date |> dateToPosix })
              }
            , Cmd.none
            )

        ReservationUpdateQuantity id beer quantityStr ->
            let
                quantity =
                    quantityStr |> String.replace "-" "" |> String.toInt |> Maybe.withDefault 0
            in
            ( { model
                | reservations =
                    model.reservations
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | order = x.order |> Dict.insert beer quantity })
              }
            , Cmd.none
            )

        ReservationUpdateName id name ->
            ( { model
                | reservations =
                    model.reservations
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | name = name })
              }
            , Cmd.none
            )

        ReservationUpdateNotes id notes ->
            ( { model
                | reservations =
                    model.reservations
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | notes = notes })
              }
            , Cmd.none
            )

        ReservationUpdateTap id tap ->
            ( { model
                | reservations =
                    model.reservations
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | tap = tap })
              }
            , Cmd.none
            )

        ReservationUpdateCups id cups ->
            ( { model
                | reservations =
                    model.reservations
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | cups = cups |> String.toInt |> Maybe.withDefault 0 })
              }
            , Cmd.none
            )

        ReservationUpdateDone id ->
            ( { model
                | reservations =
                    model.reservations
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | done = not x.done })
              }
            , Cmd.none
            )

        ReservationUpdateDate id date ->
            ( { model
                | reservations =
                    model.reservations
                        |> List.Extra.updateIf
                            (\x -> x.id == id)
                            (\x -> { x | date = date |> dateToPosix })
              }
            , Cmd.none
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
        , renderCreationButton model
        , renderReservationTable model
        ]


renderCreationButton : Model -> Html Msg
renderCreationButton model =
    case model.displayNewLineSelect of
        False ->
            a [ class "button ", onClick (DisplayNewLineSelect True) ] [ "Ajouter une commande" |> text ]

        True ->
            select [ onChange CreateNewLine ]
                ([ "Réservation", "Enfûtage", "Inventaire" ]
                    |> List.map (\opt -> option [ value opt ] [ opt |> text ])
                )


renderReservationTable : Model -> Html Msg
renderReservationTable model =
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
        , Keyed.node "tbody" [] (linesWithTotals |> List.map (viewLine model))
        ]


viewLine : Model -> ( Line, Dict String Int ) -> ( String, Html Msg )
viewLine model ( line, totals ) =
    case line of
        BrewWrapper brew ->
            ( brew.id |> String.fromInt, viewBrewLine model brew totals )

        ReservationWrapper reservation ->
            ( reservation.id |> String.fromInt, viewReservationLine model reservation totals )

        InventoryWrapper inventory ->
            ( inventory.id |> String.fromInt, viewInventoryLine model inventory totals )


renderDateInput : Int -> Time.Posix -> (String -> Msg) -> Html Msg
renderDateInput uid date event =
    input [ type_ "date", id (uid |> String.fromInt), value (date |> formatDate), onInput event, required True ] []


viewInventoryLine : Model -> Inventory -> Dict String Int -> Html Msg
viewInventoryLine model inventory totals =
    tr
        [ class "inventaire" ]
        (List.concat
            [ [ td [] [ renderDateInput inventory.id inventory.date (InventoryUpdateDate inventory.id) ]
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
            [ [ td [] [ renderDateInput brew.id brew.date (BrewUpdateDate brew.id) ]
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
            [ [ td [] [ renderDateInput reservation.id reservation.date (ReservationUpdateDate reservation.id) ]
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



---- PROGRAM ----


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
