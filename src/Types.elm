module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Json.Encode.Extra
import Random exposing (Seed)
import ScrollTo
import Time
import Uuid


type alias Model =
    { reservations : List Reservation
    , brews : List Brew
    , beers : List String
    , inventories : List Inventory
    , currentSeed : Seed
    , currentUuid : Maybe Uuid.Uuid
    , displayNewLineSelect : Bool
    , now : Time.Posix
    , scrollTo : ScrollTo.State
    , pastWeeksToDisplay : Int
    , userEmail : String
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
    { id : Maybe Uuid.Uuid
    , date : Time.Posix
    , stock : Dict String Int
    }


type alias Reservation =
    { id : Maybe Uuid.Uuid
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
    { id : Maybe Uuid.Uuid
    , date : Time.Posix
    , beer : String
    , quantity : Int
    }


type Line
    = BrewWrapper Brew
    | ReservationWrapper Reservation
    | InventoryWrapper Inventory


type alias Flags =
    { reservations : String
    , inventories : String
    , brews : String
    , seed : Int
    }


encodeLines : Model -> Json.Encode.Value
encodeLines model =
    Json.Encode.object
        [ ( "reservations", model.reservations |> encodeReservations )
        , ( "inventories", model.inventories |> encodeInventories )
        , ( "brews", model.brews |> encodeBrews )
        ]


encodeReservations : List Reservation -> Json.Encode.Value
encodeReservations reservations =
    reservations
        |> Json.Encode.list
            (\reservation ->
                Json.Encode.object
                    [ ( "id", reservation.id |> Json.Encode.Extra.maybe Uuid.encode )
                    , ( "date", reservation.date |> Time.posixToMillis |> Json.Encode.int )
                    , ( "name", reservation.name |> Json.Encode.string )
                    , ( "contact", reservation.contact |> Json.Encode.string )
                    , ( "order", Json.Encode.dict identity Json.Encode.int reservation.order )
                    , ( "tap", Json.Encode.bool reservation.tap )
                    , ( "notes", Json.Encode.string reservation.notes )
                    , ( "cups", Json.Encode.int reservation.cups )
                    , ( "done", Json.Encode.bool reservation.done )
                    ]
            )


reservationsDecoder : Json.Decode.Decoder (List Reservation)
reservationsDecoder =
    Json.Decode.list
        (Json.Decode.succeed Reservation
            |> optional "id" (Json.Decode.map Just Uuid.decoder) Nothing
            |> required "date" (Json.Decode.map Time.millisToPosix Json.Decode.int)
            |> required "name" Json.Decode.string
            |> required "contact" Json.Decode.string
            |> required "order" (Json.Decode.dict Json.Decode.int)
            |> required "tap" Json.Decode.bool
            |> required "notes" Json.Decode.string
            |> required "cups" Json.Decode.int
            |> required "done" Json.Decode.bool
        )


encodeInventories : List Inventory -> Json.Encode.Value
encodeInventories inventories =
    inventories
        |> Json.Encode.list
            (\inventory ->
                Json.Encode.object
                    [ ( "id", inventory.id |> Json.Encode.Extra.maybe Uuid.encode )
                    , ( "date", inventory.date |> Time.posixToMillis |> Json.Encode.int )
                    , ( "stock", Json.Encode.dict identity Json.Encode.int inventory.stock )
                    ]
            )


inventoriesDecoder : Json.Decode.Decoder (List Inventory)
inventoriesDecoder =
    Json.Decode.list
        (Json.Decode.succeed Inventory
            |> optional "id" (Json.Decode.map Just Uuid.decoder) Nothing
            |> required "date" (Json.Decode.map Time.millisToPosix Json.Decode.int)
            |> required "stock" (Json.Decode.dict Json.Decode.int)
        )


encodeBrews : List Brew -> Json.Encode.Value
encodeBrews brews =
    brews
        |> Json.Encode.list
            (\brew ->
                Json.Encode.object
                    [ ( "id", brew.id |> Json.Encode.Extra.maybe Uuid.encode )
                    , ( "date", brew.date |> Time.posixToMillis |> Json.Encode.int )
                    , ( "beer", Json.Encode.string brew.beer )
                    , ( "quantity", Json.Encode.int brew.quantity )
                    ]
            )


brewsDecoder : Json.Decode.Decoder (List Brew)
brewsDecoder =
    Json.Decode.list
        (Json.Decode.succeed Brew
            |> optional "id" (Json.Decode.map Just Uuid.decoder) Nothing
            |> required "date" (Json.Decode.map Time.millisToPosix Json.Decode.int)
            |> required "beer" Json.Decode.string
            |> required "quantity" Json.Decode.int
        )
