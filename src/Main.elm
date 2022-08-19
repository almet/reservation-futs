port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Html.Keyed as Keyed
import Json.Decode
import Json.Encode
import List.Extra
import Random exposing (initialSeed)
import ScrollTo
import Task
import Time
import Types exposing (..)
import Utils exposing (..)
import Uuid


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { reservations =
            Json.Decode.decodeString reservationsDecoder flags.reservations |> Result.withDefault []
      , brews = Json.Decode.decodeString brewsDecoder flags.brews |> Result.withDefault []
      , beers = [ "Souffle Tropical", "Nouveau Monde" ]
      , inventories = Json.Decode.decodeString inventoriesDecoder flags.inventories |> Result.withDefault []
      , currentSeed = initialSeed flags.seed
      , currentUuid = Nothing
      , displayNewLineSelect = False
      , now = Time.millisToPosix 0
      , scrollTo = ScrollTo.init
      , pastWeeksToDisplay = 2
      }
    , getTime
    )


type Msg
    = NoOp
    | NewUuid
    | ScrollToMsg ScrollTo.Msg
    | OnTime Time.Posix
    | DisplayNewLineSelect Bool
    | BrewUpdateDate Uuid.Uuid String
    | BrewUpdateSelectedBeer Uuid.Uuid String
    | ReservationUpdateDate Uuid.Uuid String
    | BrewUpdateQuantity Uuid.Uuid String
    | InventoryUpdateDate Uuid.Uuid String
    | InventoryUpdateQuantity Uuid.Uuid String String
    | ReservationUpdateQuantity Uuid.Uuid String String
    | ReservationUpdateName Uuid.Uuid String
    | ReservationUpdateNotes Uuid.Uuid String
    | ReservationUpdateTap Uuid.Uuid Bool
    | ReservationUpdateCups Uuid.Uuid String
    | ReservationUpdateDone Uuid.Uuid
    | CreateBrew Time.Posix
    | CreateInventory Time.Posix
    | CreateReservation Time.Posix
    | ReplaceReservations String
    | ReplaceInventories String
    | ReplaceBrews String
    | DeleteReservation Uuid.Uuid
    | DeleteInventory Uuid.Uuid
    | DeleteBrew Uuid.Uuid
    | IncreaseDisplayedPastWeeks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScrollToMsg scrollToMsg ->
            let
                ( scrollToModel, scrollToCmds ) =
                    ScrollTo.update
                        scrollToMsg
                        model.scrollTo
            in
            ( { model | scrollTo = scrollToModel }
            , Cmd.map ScrollToMsg scrollToCmds
            )

        OnTime time ->
            ( { model | now = time }, Cmd.none )

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

        CreateBrew posix ->
            let
                ( model_, _ ) =
                    update NewUuid model

                newModel =
                    { model_
                        | brews =
                            model_.brews
                                ++ [ Brew
                                        model_.currentUuid
                                        (posix |> Time.posixToMillis |> (+) 1 |> Time.millisToPosix)
                                        (model_.beers |> List.head |> Maybe.withDefault "")
                                        75
                                   ]
                    }
            in
            ( newModel, storeData (encodeLines newModel) )

        CreateInventory posix ->
            let
                ( model_, _ ) =
                    update NewUuid model

                newModel =
                    { model_
                        | inventories =
                            model_.inventories
                                ++ [ Inventory
                                        model_.currentUuid
                                        (posix |> Time.posixToMillis |> (+) 1 |> Time.millisToPosix)
                                        (Dict.fromList
                                            (model_.beers |> List.map (\x -> ( x, 0 )))
                                        )
                                   ]
                    }
            in
            ( newModel, storeData (encodeLines newModel) )

        CreateReservation posix ->
            let
                ( model_, _ ) =
                    update NewUuid model

                newModel =
                    { model_
                        | reservations =
                            model_.reservations
                                ++ [ Reservation
                                        model_.currentUuid
                                        (posix |> Time.posixToMillis |> (+) 1 |> Time.millisToPosix)
                                        ""
                                        ""
                                        (Dict.fromList (model_.beers |> List.map (\x -> ( x, 0 ))))
                                        True
                                        ""
                                        0
                                        False
                                   ]
                    }
            in
            ( newModel
            , Cmd.batch
                [ storeData (encodeLines newModel)
                , focusId model_.currentUuid
                , case model_.currentUuid of
                    Just id ->
                        let
                            f { viewport } { element } =
                                { from = { x = viewport.x, y = viewport.y }
                                , to = { x = viewport.x, y = element.y - 100000 }
                                }
                        in
                        Cmd.map ScrollToMsg <| ScrollTo.scrollToCustom f (id |> Uuid.toString)

                    Nothing ->
                        Cmd.none
                ]
            )

        BrewUpdateSelectedBeer id selectedBeer ->
            let
                newModel =
                    { model
                        | brews =
                            model.brews
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | beer = selectedBeer })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        BrewUpdateQuantity id quantityStr ->
            let
                newModel =
                    { model
                        | brews =
                            model.brews
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | quantity = quantityStr |> String.replace "+" "" |> String.toInt |> Maybe.withDefault 0 })
                    }
            in
            ( newModel, storeData (encodeLines newModel) )

        BrewUpdateDate id date ->
            let
                newModel =
                    { model
                        | brews =
                            model.brews
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | date = date |> dateToPosix })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        InventoryUpdateQuantity id beer quantityStr ->
            let
                quantity =
                    quantityStr |> String.replace "=" "" |> String.toInt |> Maybe.withDefault 0
            in
            let
                newModel =
                    { model
                        | inventories =
                            model.inventories
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | stock = x.stock |> Dict.insert beer quantity })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        InventoryUpdateDate id date ->
            let
                newModel =
                    { model
                        | inventories =
                            model.inventories
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | date = date |> dateToPosix })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        ReservationUpdateQuantity id beer quantityStr ->
            let
                quantity =
                    quantityStr |> String.replace "-" "" |> String.toInt |> Maybe.withDefault 0
            in
            let
                newModel =
                    { model
                        | reservations =
                            model.reservations
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | order = x.order |> Dict.insert beer quantity })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        ReservationUpdateName id name ->
            let
                newModel =
                    { model
                        | reservations =
                            model.reservations
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | name = name })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        ReservationUpdateNotes id notes ->
            let
                newModel =
                    { model
                        | reservations =
                            model.reservations
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | notes = notes })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        ReservationUpdateTap id tap ->
            let
                newModel =
                    { model
                        | reservations =
                            model.reservations
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | tap = tap })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        ReservationUpdateCups id cups ->
            let
                newModel =
                    { model
                        | reservations =
                            model.reservations
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | cups = cups |> String.toInt |> Maybe.withDefault 0 })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        ReservationUpdateDone id ->
            let
                newModel =
                    { model
                        | reservations =
                            model.reservations
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | done = not x.done })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        ReservationUpdateDate id date ->
            let
                newModel =
                    { model
                        | reservations =
                            model.reservations
                                |> List.Extra.updateIf
                                    (\x -> x.id == Just id)
                                    (\x -> { x | date = date |> dateToPosix })
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        DeleteReservation id ->
            let
                newModel =
                    { model
                        | reservations =
                            model.reservations
                                |> List.filter (\x -> x.id /= Just id)
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        DeleteInventory id ->
            let
                newModel =
                    { model
                        | inventories =
                            model.inventories
                                |> List.filter (\x -> x.id /= Just id)
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        DeleteBrew id ->
            let
                newModel =
                    { model
                        | brews =
                            model.brews
                                |> List.filter (\x -> x.id /= Just id)
                    }
            in
            ( newModel
            , storeData (encodeLines newModel)
            )

        ReplaceReservations encoded ->
            let
                newReservations =
                    Json.Decode.decodeString reservationsDecoder encoded
                        |> Result.withDefault model.reservations
            in
            ( { model | reservations = newReservations }, Cmd.none )

        ReplaceInventories encoded ->
            let
                newInventories =
                    Json.Decode.decodeString inventoriesDecoder encoded
                        |> Result.withDefault model.inventories
            in
            ( { model | inventories = newInventories }, Cmd.none )

        ReplaceBrews encoded ->
            let
                newBrews =
                    Json.Decode.decodeString brewsDecoder encoded
                        |> Result.withDefault model.brews
            in
            ( { model | brews = newBrews }, Cmd.none )

        IncreaseDisplayedPastWeeks ->
            ( { model | pastWeeksToDisplay = model.pastWeeksToDisplay + 1 }, Cmd.none )

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
    div [ class "wrapper" ]
        [ h1 [] [ "⛽ " |> text ]
        , renderReservationTable model
        ]


renderReservationTable : Model -> Html Msg
renderReservationTable model =
    let
        headerLine header =
            th [ class (header |> String.toLower) ] [ header |> text ]

        lines =
            combineAndSort model.brews model.reservations model.inventories

        linesWithTotals =
            List.map2 Tuple.pair lines (computeTotals model.beers lines)
                |> List.filter
                    (\( x, _ ) ->
                        let
                            n_weeks_ago =
                                (model.now |> Time.posixToMillis) - (604800000 * model.pastWeeksToDisplay)

                            compare item =
                                (item.date |> Time.posixToMillis) > n_weeks_ago
                        in
                        case x of
                            BrewWrapper e ->
                                compare e

                            InventoryWrapper e ->
                                compare e

                            ReservationWrapper e ->
                                compare e
                    )
    in
    case lines |> List.length of
        0 ->
            div [ class "empty" ] [ a [ onClick (CreateInventory model.now) ] [ "Aucune donnée. Peut-être voulez vous faire un état des stocks" |> text ] ]

        _ ->
            table [ class "table table-wrapper" ]
                [ thead []
                    [ tr []
                        (List.concat
                            [ [ "", "Date", "Nom" ] |> List.map headerLine
                            , model.beers |> List.map (\beer -> th [ class "move", colspan 2 ] [ beer ++ " (dispo)" |> text ])
                            , [ "Notes", "Gobelets" ] |> List.map headerLine
                            , [ renderLinesToDisplay model ]
                            ]
                        )
                    ]
                , Keyed.node "tbody" [] (linesWithTotals |> List.map (viewLine model))
                ]


renderLinesToDisplay : Model -> Html Msg
renderLinesToDisplay model =
    let
        number =
            model.pastWeeksToDisplay |> String.fromInt
    in
    th [ class "more" ] [ a [ onClick IncreaseDisplayedPastWeeks ] [ "Voir plus (" ++ number ++ ")" |> text ] ]


renderNothing : Html msg
renderNothing =
    div [] []


viewLine : Model -> ( Line, Dict String Int ) -> ( String, Html Msg )
viewLine model ( line, totals ) =
    case line of
        BrewWrapper brew ->
            case brew.id of
                Just uuid ->
                    ( uuid |> Uuid.toString, viewBrewLine model brew totals uuid )

                Nothing ->
                    ( "", renderNothing )

        ReservationWrapper reservation ->
            case reservation.id of
                Just uuid ->
                    ( uuid |> Uuid.toString, viewReservationLine model reservation totals uuid )

                Nothing ->
                    ( "", renderNothing )

        InventoryWrapper inventory ->
            case inventory.id of
                Just uuid ->
                    ( uuid |> Uuid.toString, viewInventoryLine model inventory totals uuid )

                Nothing ->
                    ( "", renderNothing )


renderDateInput : Time.Posix -> (String -> Msg) -> Uuid.Uuid -> Html Msg
renderDateInput date event uuid =
    input
        [ id ((uuid |> Uuid.toString) ++ "-date")
        , type_ "date"
        , value (date |> formatDate)
        , onInput event
        , required True
        ]
        []


renderTotalCell total =
    let
        polarity =
            if total >= 0 then
                "positive"

            else
                "negative"
    in
    td [ class ("total " ++ polarity) ] [ "(" ++ String.fromInt total ++ ")" |> text ]


renderActions posix =
    td [ class "actions" ]
        [ ul [ class "creation-links" ]
            [ li [] [ a [ onClick (CreateBrew posix) ] [ "+ enfûtage" |> text ] ]
            , li [] [ a [ onClick (CreateInventory posix) ] [ "= inventaire" |> text ] ]
            , li [] [ a [ onClick (CreateReservation posix) ] [ "- résa" |> text ] ]
            ]
        ]


viewInventoryLine : Model -> Inventory -> Dict String Int -> Uuid.Uuid -> Html Msg
viewInventoryLine model inventory totals uuid =
    tr
        [ class "inventaire" ]
        (List.concat
            [ [ renderActions inventory.date
              , td [] [ renderDateInput inventory.date (InventoryUpdateDate uuid) uuid ]
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
                                totals |> Dict.get beer |> Maybe.withDefault 0
                        in
                        [ td [ class "move" ] [ input [ type_ "text", value displayableValue, onChange (InventoryUpdateQuantity uuid beer) ] [] ]
                        , renderTotalCell total
                        ]
                    )
                |> List.concat
            , [ td [ colspan 2 ] []
              , td []
                    [ renderDeleteLink (DeleteInventory uuid) ]
              ]
            ]
        )


viewBrewLine : Model -> Brew -> Dict String Int -> Uuid.Uuid -> Html Msg
viewBrewLine model brew totals uuid =
    tr [ class "mise-en-futs" ]
        (List.concat
            [ [ renderActions brew.date
              , td [] [ renderDateInput brew.date (BrewUpdateDate uuid) uuid ]
              , td []
                    [ select [ onChange (BrewUpdateSelectedBeer uuid) ]
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
                                input [ type_ "text", value ("+" ++ String.fromInt quantity), onChange (BrewUpdateQuantity uuid) ] []

                              else
                                "" |> text
                            ]
                        , renderTotalCell (totals |> Dict.get beer |> Maybe.withDefault 0)
                        ]
                    )
                |> List.concat
            , [ td [ colspan 2 ] []
              , td []
                    [ renderDeleteLink (DeleteBrew uuid) ]
              ]
            ]
        )


viewReservationLine : Model -> Reservation -> Dict String Int -> Uuid.Uuid -> Html Msg
viewReservationLine model reservation totals uuid =
    let
        isEmpty =
            (reservation.order |> Dict.values |> List.sum) == 0

        class_ =
            if isEmpty then
                "empty"

            else
                ""
    in
    tr [ id (uuid |> Uuid.toString), class class_ ]
        (List.concat
            [ [ renderActions reservation.date
              , td [] [ renderDateInput reservation.date (ReservationUpdateDate uuid) uuid ]
              , td [] [ input [ id ((uuid |> Uuid.toString) ++ "-name"), type_ "text", value reservation.name, onChange (ReservationUpdateName uuid) ] [] ]
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
                                totals |> Dict.get beer |> Maybe.withDefault 0
                        in
                        [ td [ class "move" ] [ input [ type_ "text", value stringValue, onChange (ReservationUpdateQuantity uuid beer) ] [] ]
                        , renderTotalCell total
                        ]
                    )
                |> List.concat
            , [ td []
                    [ textarea [ onChange (ReservationUpdateNotes uuid) ] [ reservation.notes |> text ] ]
              , td []
                    [ let
                        cups =
                            case reservation.cups of
                                0 ->
                                    "Non"

                                _ ->
                                    reservation.cups |> String.fromInt
                      in
                      input [ type_ "text", value cups, onChange (ReservationUpdateCups uuid) ] []
                    ]
              , td []
                    [ renderDeleteLink (DeleteReservation uuid) ]
              ]
            ]
        )


renderDeleteLink event =
    a [ class "delete", onClick event ] [ text "✘" ]


getTime =
    Time.now
        |> Task.perform OnTime


focusId : Maybe Uuid.Uuid -> Cmd Msg
focusId uuid =
    case uuid of
        Nothing ->
            Cmd.none

        Just id ->
            Task.attempt
                (\a ->
                    let
                        _ =
                            Debug.log "Value of a: " a
                    in
                    NoOp
                )
                (Dom.focus ((id |> Uuid.toString) ++ "-name"))



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ replaceReservations ReplaceReservations
        , replaceInventories ReplaceInventories
        , replaceBrews ReplaceBrews
        ]



-- Ports


port storeData : Json.Encode.Value -> Cmd msg


port replaceReservations : (String -> msg) -> Sub msg


port replaceInventories : (String -> msg) -> Sub msg


port replaceBrews : (String -> msg) -> Sub msg



-- Program


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
