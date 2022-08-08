module Utils exposing (..)

import Iso8601
import Time
import Time.Format
import Time.Format.Config.Config_fr_fr exposing (config)


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


formatDate : Time.Posix -> String
formatDate date =
    Time.Format.format config "%Y-%m-%d" Time.utc date


dateToPosix : String -> Time.Posix
dateToPosix input =
    input |> Iso8601.toTime |> Result.withDefault (Time.millisToPosix 0)
