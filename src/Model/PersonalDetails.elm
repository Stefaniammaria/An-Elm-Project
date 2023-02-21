module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, href)
import List

type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }

detailToString : List DetailWithName -> List String
detailToString list = 
    case list of
    [] -> []
    x::xs -> (x.name ++ ": " ++ x.detail ):: detailToString xs

view : PersonalDetails -> Html msg
view details =
    div [] [
        h1[id "name"][text details.name]
        ,em[id "intro"][text details.intro]
        ,ul[class "contact-detail"]
        (List.map (\list->li [] [text list ]) (detailToString details.contacts))
        ,ul[class "social-link"]
        (List.map (\list->li [][a[href ""][text list ]]) (detailToString details.socials))
    ]

