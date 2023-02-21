module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList,href)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)
import List


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"


sortByElement : (a -> Interval) -> (Interval -> Interval -> Order) -> List a -> List a
sortByElement accessor sortFunc list =
    List.sortWith (orderBy accessor sortFunc) list

orderBy : (a -> Interval) -> (Interval -> Interval -> Order) -> a -> a -> Order
orderBy accessor orderFunc a b =
        orderFunc (accessor a) (accessor b)

sortByInterval : List Event -> List Event
sortByInterval events =
    sortByElement .interval Interval.compare events

noMaybeFunction : Maybe String -> String
noMaybeFunction a =
    case a of
        Just x -> x
        Nothing -> ""

view : Event -> Html Never
view event =
    div [if event.important == True then
        class "event-important event"
        else class "event"] 
    [h1[class "event-title"][text event.title]
    ,h1[class "event-description"][event.description]
    ,h1[class "event-category"][categoryView event.category]
    ,h1[class "event-interval"][text ""]
    ,h1[class "event-url"][text (noMaybeFunction event.url)]
    --,h1[class "event-url"][a[href (noMaybeFunction event.url)][]]
    ]






