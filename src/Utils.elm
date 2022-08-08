module Utils exposing (..)

import Time


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
