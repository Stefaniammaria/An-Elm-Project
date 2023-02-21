module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De
import List


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }

noMaybeFunction : Maybe String -> String
noMaybeFunction a =
    case a of
        Just x -> x
        Nothing -> ""

view : Repo -> Html msg
view repo =
    div [class "repo"] [
        h1[class "repo-name"][text repo.name]
        ,h1[class "repo-description"][text (noMaybeFunction(repo.description))]
        ,h1[class "repo-url"][a[href repo.url][]]
        ,h1[class "repo-stars"][text (String.fromInt(repo.stars))]
    ]
    


compareInt : Int -> Int -> Order
compareInt a b =
    if a > b then GT
    else if a < b then LT
    else EQ

sortByElement : (a -> Int) -> (Int -> Int -> Order) -> List a -> List a
sortByElement accessor sortFunc list =
    List.sortWith (orderBy accessor sortFunc) list

orderBy : (a -> Int) -> (Int -> Int -> Order) -> a -> a -> Order
orderBy accessor orderFunc a b =
        orderFunc (accessor a) (accessor b)

sortByStars : List Repo -> List Repo
sortByStars repos =
    sortByElement .stars compareInt repos


{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : De.Decoder Repo
decodeRepo =
    De.map5 Repo
        (De.field "name" De.string)
        (De.maybe (De.field "description" De.string))
        (De.field "html_url" De.string)
        (De.field "pushed_at" De.string)
        (De.field "stargazers_count" De.int)
