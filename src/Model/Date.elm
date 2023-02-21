module Model.Date exposing (Date, Month(..), compare, withDefaultV2,compareMonth, full, month, monthToString, monthsBetween, monthsBetweenMonths, offsetMonths, onlyYear, view, year)

--import Html exposing (Html, text, div)
import Html exposing (..)
import Model.Util exposing (chainCompare)


type Date
    = Date { year : Int, month : Maybe Month }


year : Date -> Int
year (Date d) =
    d.year


month : Date -> Maybe Month
month (Date d) =
    d.month


full : Int -> Month -> Date
full y m =
    Date { year = y, month = Just m }


onlyYear : Int -> Date
onlyYear y =
    Date { year = y, month = Nothing }


withDefaultV2 : Maybe Month -> Month
withDefaultV2 a =
    case a of
        Just x -> x
        Nothing -> Apr


{-| Given two `Date`s it returns the number of months between the two dates as an **absolute** value.
The month fields are handled as follows:

  - If both are present (`Just`), they are included normally in the calculation
  - If both are missing (`Nothing`), the number of years between the two dates is calculated
  - Otherwise the result is undefined (`Nothing`)

```
    monthsBetween (full 2020 Jan) (full 2020 Feb) --> Just 1
    monthsBetween (full 2020 Mar) (full 2020 Jan) --> Just 2
    monthsBetween (full 2020 Jan) (full 2020 Dec) --> Just 11

    monthsBetween (full 2020 Jan) (full 2021 Feb) --> Just 13
    monthsBetween (full 2021 Jan) (full 2020 Feb) --> Just 11

    monthsBetween (onlyYear 2020) (full 2021 Jan) --> Nothing
    monthsBetween (full 2020 Jan) (onlyYear 2021) --> Nothing

    monthsBetween (full 2020 Dec) (full 2021 Jan) --> Just 1

    monthsBetween (onlyYear 2020) (onlyYear 2021) --> Just 12
    monthsBetween (onlyYear 2020) (onlyYear 2022) --> Just 24
```

-}
monthsBetween : Date -> Date -> Maybe Int
monthsBetween d1 d2 =
    if month d1 /= Nothing && month d2 /= Nothing then
        if year d1 > year d2 then 
            Just (monthToInt(withDefaultV2(month d1)) + (monthToInt(Dec) - monthToInt(withDefaultV2(month d2))) + 1 + (year d1 - year d2 - 1 ) * 12)
        else if year d1 < year d2 then
            Just ((monthToInt(Dec) - monthToInt(withDefaultV2(month d1))) + monthToInt(withDefaultV2(month d2)) + 1 + (year d2 - year d1 - 1 ) * 12)
        else if monthToInt(withDefaultV2(month d1)) < monthToInt(withDefaultV2(month d2)) then
            Just (monthToInt(withDefaultV2(month d2)) - monthToInt(withDefaultV2(month d1)))
        else if monthToInt(withDefaultV2(month d1)) > monthToInt(withDefaultV2(month d2)) then
            Just (monthToInt(withDefaultV2(month d1)) - monthToInt(withDefaultV2(month d2)))
        else Just 0
    else if (month d1 == Nothing && month d2 /= Nothing) || (month d1 /= Nothing && month d2 == Nothing) then
            Nothing
        else if year d1 < year d2 then
                Just ((year d2 - year d1) * 12)
            else if year d1 > year d2 then
                Just ((year d1 - year d2) * 12)
            else Just 0
    


{-| Compares two dates.
First, dates are compared by the year field. If it's equal, the month fields are used as follows:

  - If both are present (`Just`), they are compared the result is returned
  - If both are missing (`Nothing`), the dates are equal
  - Otherwise the date without month is greater

```
    Model.Date.compare (full 2020 Jan) (full 2021 Jan) --> LT
    Model.Date.compare (full 2021 Dec) (full 2021 Jan) --> GT

    Model.Date.compare (full 2020 Jan) (full 2020 Dec) --> LT
    Model.Date.compare (onlyYear 2020) (onlyYear 2021) --> LT

    Model.Date.compare (onlyYear 2020) (full 2020 Dec) --> GT
    Model.Date.compare (onlyYear 2019) (full 2020 Dec) --> LT
```

-}
compare : Date -> Date -> Order
compare (Date d1) (Date d2) =
    if month (Date d1) /= Nothing && month (Date d2) /= Nothing then
        if year (Date d1) > year (Date d2) then 
            GT
        else if year (Date d1) < year (Date d2) then
            LT
        else if monthToInt(withDefaultV2(month (Date d1))) < monthToInt(withDefaultV2(month (Date d2))) then
            LT
        else if monthToInt(withDefaultV2(month (Date d1))) > monthToInt(withDefaultV2(month (Date d2))) then
            GT
        else EQ
    else if (month (Date d1) == Nothing && month (Date d2) /= Nothing) then
        if year (Date d1) < year (Date d2) then
                LT
            else GT
        else if (month (Date d1) /= Nothing && month (Date d2) == Nothing) then
            if year (Date d1) < year (Date d2) then
                GT
            else LT
        else if year (Date d1) < year (Date d2) then
                LT
            else if year (Date d1) > year (Date d2) then
                GT
            else EQ


{-| Given a current date and the number of months, it returns a new date with the given number of months passed.

-}
offsetMonths : Int -> Date -> Date
offsetMonths months (Date d) =
    let
        addMonths =
            modBy 12 months

        addedMonths =
            d.month
                |> Maybe.map monthToInt
                |> Maybe.map ((+) addMonths)

        newMonth =
            addedMonths
                |> Maybe.map (modBy 12)
                |> Maybe.andThen intToMonth

        addYears =
            months // 12

        extraYear =
            if Maybe.withDefault 0 addedMonths >= 12 then
                1

            else
                0
    in
    Date { year = d.year + addYears + extraYear, month = newMonth }

yearToString : Date -> String
yearToString date = 
    String.fromInt(year date)

view : Date -> Html msg
view (Date d) =
    div [] [
        text (yearToString (Date d))
        ,if month (Date d) == Nothing then
            text ""
        else text (monthToString(withDefaultV2(month(Date d))))
    ]


-- MONTH


type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


intToMonth : Int -> Maybe Month
intToMonth idx =
    case idx of
        0 ->
            Just Jan

        1 ->
            Just Feb

        2 ->
            Just Mar

        3 ->
            Just Apr

        4 ->
            Just May

        5 ->
            Just Jun

        6 ->
            Just Jul

        7 ->
            Just Aug

        8 ->
            Just Sep

        9 ->
            Just Oct

        10 ->
            Just Nov

        11 ->
            Just Dec

        _ ->
            Nothing


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


monthToString : Month -> String
monthToString m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


compareMonth : Month -> Month -> Order
compareMonth m1 m2 =
    Basics.compare (monthToInt m1) (monthToInt m2)


{-| Returns the number of months between two months as an **absolute** value.

    monthsBetweenMonths Jan Jan --> 0

    monthsBetweenMonths Jan Apr --> 3

    monthsBetweenMonths Apr Jan --> 3

-}
monthsBetweenMonths : Month -> Month -> Int
monthsBetweenMonths m1 m2 =
    if monthToInt m1 > monthToInt m2 then 
        monthToInt m1 - monthToInt m2
    else monthToInt m2 - monthToInt m1
    
