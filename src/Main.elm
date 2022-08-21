port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Css.Global exposing (svg)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Events.Extra exposing (onChange)
import Html.Keyed as Keyed
import Json.Decode
import Json.Encode
import List.Extra
import Murmur3
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
      , userEmail = flags.userEmail
      , loginFormEmail = ""
      , userMenuOpen = False
      }
    , Cmd.batch
        [ getTime
        , fetchData "yeah"
        ]
    )


type Msg
    = NoOp
    | NewUuid
    | OnTime Time.Posix
    | UpdateLoginFormEmail String
    | SubmitLoginForm
    | LoginSuccess String
    | Logout
    | ToggleUserMenu
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

        UpdateLoginFormEmail email ->
            ( { model | loginFormEmail = email }, Cmd.none )

        SubmitLoginForm ->
            ( model, startLogin model.loginFormEmail )

        LoginSuccess email ->
            ( { model | userEmail = email }, fetchData email )

        Logout ->
            ( { model | userEmail = "", userMenuOpen = False }, Cmd.none )

        ToggleUserMenu ->
            ( { model | userMenuOpen = not model.userMenuOpen }, Cmd.none )

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
    case model.userEmail of
        "" ->
            renderLoginView model

        _ ->
            renderApp model


renderUserConnected model =
    nav [ class "bg-gray-800" ]
        [ div [ class "max-w-7xl mx-auto px-2 sm:px-6 lg:px-8" ]
            [ div [ class "relative flex items-center justify-between h-16" ]
                [ div [ class "absolute inset-y-0 left-0 flex items-center sm:hidden" ]
                    [ button [ attribute "aria-controls" "mobile-menu", attribute "aria-expanded" "false", class "inline-flex items-center justify-center p-2 rounded-md text-gray-400 hover:text-white hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-white", type_ "button" ]
                        [ span [ class "sr-only" ]
                            [ text "Open main menu" ]
                        ]
                    ]
                , div [ class "flex-1 flex items-center justify-center sm:items-stretch sm:justify-start" ]
                    []
                , let
                    transition =
                        if model.userMenuOpen then
                            ""

                        else
                            "hidden"
                  in
                  div [ class "absolute inset-y-0 right-0 flex items-center pr-2 sm:static sm:inset-auto sm:ml-6 sm:pr-0" ]
                    [ div [ class "ml-3 relative" ]
                        [ div []
                            [ button [ onClick ToggleUserMenu, attribute "aria-expanded" "false", attribute "aria-haspopup" "true", class "bg-gray-800 flex text-sm rounded-full focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white", id "user-menu-button", type_ "button" ]
                                [ span [ class "sr-only" ]
                                    [ text "Open user menu" ]
                                , img [ alt "", class "h-8 w-8 rounded-full", src ("https://robohash.org/" ++ (model.userEmail |> Murmur3.hashString 1234 |> String.fromInt) ++ "?set=set4") ]
                                    []
                                ]
                            ]
                        , div [ attribute "aria-labelledby" "user-menu-button", attribute "aria-orientation" "vertical", class ("origin-top-right absolute right-0 mt-2 w-48 rounded-md shadow-lg py-1 bg-white ring-1 ring-black ring-opacity-5 focus:outline-none z-40 " ++ transition), attribute "role" "menu", attribute "tabindex" "-1" ]
                            [ a [ class "block px-4 py-2 text-sm text-gray-700", href "#", id "user-menu-item-0", attribute "role" "menuitem", attribute "tabindex" "-1" ]
                                [ text model.userEmail ]
                            , a [ onClick Logout, class "block px-4 py-2 text-sm text-gray-700", href "#", id "user-menu-item-2", attribute "role" "menuitem", attribute "tabindex" "-1" ]
                                [ text "Se déconnecter" ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "sm:hidden", id "mobile-menu" ]
            [ div [ class "px-2 pt-2 pb-3 space-y-1" ]
                [ a [ attribute "aria-current" "page", class "bg-gray-900 text-white block px-3 py-2 rounded-md text-base font-medium", href "#" ]
                    [ text "Dashboard" ]
                , a [ class "text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium", href "#" ]
                    [ text "Team" ]
                , a [ class "text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium", href "#" ]
                    [ text "Projects" ]
                , a [ class "text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium", href "#" ]
                    [ text "Calendar" ]
                ]
            ]
        ]


renderApp model =
    div [ class "wrapper" ]
        [ renderUserConnected model
        , renderReservationTable model
        ]


renderLoginView : a -> Html Msg
renderLoginView model =
    section
        [ class "h-screen"
        ]
        [ div
            [ class "px-6 h-full text-gray-800"
            ]
            [ div
                [ class "flex xl:justify-center lg:justify-between justify-center items-center flex-wrap h-full g-6"
                ]
                [ div
                    [ class "grow-0 shrink-1 md:shrink-0 basis-auto xl:w-6/12 lg:w-6/12 md:w-9/12 mb-12 md:mb-0"
                    ]
                    [ img
                        [ src "having-fun.png"
                        , class "w-full"
                        , alt "Sample image"
                        ]
                        []
                    ]
                , div
                    [ class "xl:ml-20 xl:w-5/12 lg:w-5/12 md:w-8/12 mb-12 md:mb-0"
                    ]
                    [ h2 [ class "text-xl" ] [ text "Connectez-vous" ]
                    , Html.form [ onSubmit SubmitLoginForm ]
                        [ div
                            [ class "flex items-center my-4 before:flex-1 before:border-t before:border-gray-300 before:mt-0.5 after:flex-1 after:border-t after:border-gray-300 after:mt-0.5"
                            ]
                            []
                        , {- Email input -}
                          div
                            [ class "mb-6"
                            ]
                            [ input
                                [ type_ "text"
                                , class "form-control block w-full px-4 py-2 text-xl font-normal text-gray-700 bg-white bg-clip-padding border border-solid border-gray-300 rounded transition ease-in-out m-0 focus:text-gray-700 focus:bg-white focus:border-blue-600 focus:outline-none"
                                , id "exampleFormControlInput2"
                                , placeholder "Adresse email"
                                , onInput UpdateLoginFormEmail
                                ]
                                []
                            ]
                        , div
                            [ class "text-center lg:text-left"
                            ]
                            [ button
                                [ onClick SubmitLoginForm
                                , type_ "button"
                                , class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                                ]
                                [ text "Login" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


renderReservationTable : Model -> Html Msg
renderReservationTable model =
    let
        headerLine header =
            th [ class ((header |> String.toLower) ++ "px-5 py-3 border-b-2 border-gray-200 bg-gray-100 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider") ] [ header |> text ]

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
            div []
                [ p [ class "p-8" ]
                    [ "Voici l'ensemble des réservations de fûts faites jusqu'ici !" |> text ]
                , table
                    [ class "min-w-full leading-normal" ]
                    [ thead []
                        [ tr []
                            (List.concat
                                [ [ "", "Type", "Date", "Nom" ] |> List.map headerLine
                                , model.beers |> List.map (\beer -> th [ class "px-5 py-3 border-b-2 border-gray-200 bg-gray-100 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider", colspan 2 ] [ beer ++ " (dispo)" |> text ])
                                , [ "Notes", "Gobelets" ] |> List.map headerLine
                                , [ renderLinesToDisplay model ]
                                ]
                            )
                        ]
                    , Keyed.node "tbody" [] (linesWithTotals |> List.map (viewLine model))
                    ]
                ]


renderLinesToDisplay : Model -> Html Msg
renderLinesToDisplay model =
    let
        number =
            model.pastWeeksToDisplay |> String.fromInt
    in
    th [ class "more px-5 py-3 border-b-2 border-gray-200 bg-gray-100 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider" ] [ a [ onClick IncreaseDisplayedPastWeeks ] [ "Voir plus (" ++ number ++ ")" |> text ] ]


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


renderTotalCell : Int -> Html msg
renderTotalCell total =
    let
        polarity =
            if total >= 0 then
                "positive"

            else
                "negative"
    in
    td [ class ("total " ++ polarity) ] [ "(" ++ String.fromInt total ++ ")" |> text ]


renderActions : Time.Posix -> Html Msg
renderActions posix =
    td [ class "actions px-5 py-5 border-b border-gray-200 bg-white text-sm" ]
        [ ul [ class "creation-links" ]
            [ li [] [ a [ onClick (CreateBrew posix) ] [ "+ enfûtage" |> text ] ]
            , li [] [ a [ onClick (CreateInventory posix) ] [ "= inventaire" |> text ] ]
            , li [] [ a [ onClick (CreateReservation posix) ] [ "- résa" |> text ] ]
            ]
        ]


renderLineType : String -> Html msg
renderLineType type_ =
    let
        text_color =
            case type_ of
                "Mis en fûts" ->
                    "text-green-900"

                "Inventaire" ->
                    "text-orange-700"

                "Réservation" ->
                    "text-yellow-900"

                _ ->
                    "text-green-900"

        bg_color =
            case type_ of
                "Mis en fûts" ->
                    "bg-green-200"

                -- class="bg-orange-200 text-orange-900"
                "Inventaire" ->
                    "bg-orange-200"

                "Réservation" ->
                    "bg-yellow-200"

                _ ->
                    "bg-green-200"
    in
    span [ class ("relative inline-block px-3 py-1 font-semibold " ++ text_color ++ " leading-tight") ]
        [ span [ attribute "aria-hidden" "", class ("absolute inset-0 " ++ bg_color ++ " opacity-50 rounded-full") ]
            []
        , span [ class "relative" ]
            [ text type_ ]
        ]


viewInventoryLine : Model -> Inventory -> Dict String Int -> Uuid.Uuid -> Html Msg
viewInventoryLine model inventory totals uuid =
    tr
        [ class "inventaire px-5 py-5 border-b border-gray-200 bg-white text-sm" ]
        (List.concat
            [ [ renderActions inventory.date
              , td [] [ renderLineType "Inventaire" ]
              , td [] [ renderDateInput inventory.date (InventoryUpdateDate uuid) uuid ]
              , td [] []
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
                        [ td [ class "move text-right" ]
                            [ input
                                [ type_ "text"
                                , value displayableValue
                                , onChange (InventoryUpdateQuantity uuid beer)
                                , class "text-right"
                                ]
                                []
                            ]
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
    tr [ class "mise-en-futs px-5 py-5 border-b border-gray-200 bg-white text-sm" ]
        (List.concat
            [ [ renderActions brew.date
              , td [] [ renderLineType "Enfûtage" ]
              , td [] [ renderDateInput brew.date (BrewUpdateDate uuid) uuid ]
              , td [ class "text-left" ]
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
                        [ td [ class "move text-right" ]
                            [ if beer == brew.beer then
                                input
                                    [ type_ "text"
                                    , value ("+" ++ String.fromInt quantity)
                                    , onChange (BrewUpdateQuantity uuid)
                                    , class "text-right"
                                    ]
                                    []

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
                "empty "

            else
                ""
    in
    tr [ id (uuid |> Uuid.toString), class (class_ ++ "px-5 py-5 border-b border-gray-200 bg-white text-sm") ]
        (List.concat
            [ [ renderActions reservation.date
              , td [] [ renderLineType "Réservation" ]
              , td [] [ renderDateInput reservation.date (ReservationUpdateDate uuid) uuid ]
              , td [ class "text-left" ] [ input [ id ((uuid |> Uuid.toString) ++ "-name"), type_ "text", value reservation.name, onChange (ReservationUpdateName uuid) ] [] ]
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
                        [ td [ class "text-right" ] [ input [ class "text-right", type_ "text", value stringValue, onChange (ReservationUpdateQuantity uuid beer) ] [] ]
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


renderDeleteLink : msg -> Html msg
renderDeleteLink event =
    a [ class "delete", onClick event ] [ text "✘" ]


getTime : Cmd Msg
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
subscriptions _ =
    Sub.batch
        [ replaceReservations ReplaceReservations
        , replaceInventories ReplaceInventories
        , replaceBrews ReplaceBrews
        , loginSuccess LoginSuccess
        ]



-- Ports


port storeData : Json.Encode.Value -> Cmd msg


port fetchData : String -> Cmd msg


port startLogin : String -> Cmd msg


port loginSuccess : (String -> msg) -> Sub msg


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
