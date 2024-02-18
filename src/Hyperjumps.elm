module Hyperjumps exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Tuple exposing (first, second, pair)

type Op
    = Add
    | Subtract
    | Multiply
    | Divide

do_op : Op -> Int -> Int -> Result String Int
do_op op a b = case op of
    Add -> Ok (a+b)
    Subtract -> if a>b then Ok (a-b) else Err "The result of subtraction must be positive"
    Multiply -> Ok (a*b)
    Divide -> if modBy b a == 0 then Ok (a // b) else Err ((String.fromInt b)++" does not divide "++(String.fromInt a))

type alias Game =
    { numbers : List (Int, Maybe Int)
    , planet_number : Int
    }

type alias Model =
    { game : Game
    , selected_number : Maybe Int
    }

type Msg
    = SelectNumber Int
    | ClickSequence Int

{- makes a list of:
    Nothing: not filled
    Just (number, maybe_from): filled with number, which came from:
        Nothing -> it's fixed
        Just j -> index j in the list of numbers
-}
make_sequence : Game -> List (Maybe (Int, Maybe Int))
make_sequence game = 
    let
        size = List.length game.numbers
    in
       List.range 0 (size - 1)
    |> List.indexedMap (\i pos -> 
           game.numbers {- List (Int, Maybe Int) -}
        |> List.indexedMap pair {- List (Int, (Int, Maybe Int)) -}
        |> List.filter (second >> second >> (==) (Just pos))
        |> List.head {- ((Int, Maybe Int), Int) -}
        |> Maybe.map (\(npos, (n,_)) -> (n, Just npos))
        )
    |> List.reverse |> (::) (Just (game.planet_number, Nothing)) |> List.reverse


main = Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init_model : Model
init_model = 
    { game = { numbers = [1,3,3,4,1,4,3,3] |> List.map (\n -> (n, Nothing)), planet_number = 6 }
    , selected_number = Nothing
    }

nocmd model = (model, Cmd.none)

fi = String.fromInt

init: () -> (Model, Cmd Msg)
init _ = (init_model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    SelectNumber i -> { model | selected_number = if model.selected_number == Just i then Nothing else Just i } |> nocmd

    ClickSequence i -> case Debug.log "hey" (model.selected_number, List.Extra.getAt i (make_sequence model.game) |> Maybe.andThen identity) of 
        {- nothing selected, nothing in the sequence at that point: do nothing -}
        (Nothing, Nothing) -> nocmd model

        {- no matter the selection, fixed number in the sequence at that point: do nothing -}
        (_, Just (_, Nothing)) -> nocmd model

        {- nothing selected, number from index j in the sequence at that point: remove that number from the sequence -}
        (Nothing, Just (_, Just j)) -> 
            let
                game = model.game
                ngame = { game | numbers = game.numbers |> List.Extra.updateAt j (\(n,_) -> (n, Nothing)) }
            in
                {model | game = ngame, selected_number = Just j } |> nocmd

        {- number from index j selected, nothing in the sequence at that point: place that number in the sequence -}
        (Just j, Nothing) -> 
            let
                game = model.game
                ngame = { game | numbers = game.numbers |> List.Extra.updateAt j (\(n,_) -> (n, Just i)) }
            in
                {model | game = ngame, selected_number = Nothing } |> nocmd

        {- number from index j selected, number from index k in the sequence at that point: remove k, insert j -}
        (Just j, Just (k,Just _)) -> nocmd model


verify_triples : ((a, a, a) -> Maybe b) -> List a -> List (Maybe b)
verify_triples fn sequence = 
    List.foldr
        (\c (ma,mb,prev) -> 
            let
                res = case (ma, mb) of
                    (Just a, Just b) -> fn (c, b, a)
                    _ -> Nothing
            in
                (mb, Just c, res::prev)
        )
        (Nothing, Nothing, [])
        sequence
    |> \(_,_,out) -> out

isOk : Result a b -> Bool
isOk r = case r of
    Ok _ -> True
    Err _ -> False

valid_triple : (Int, Int, Int) -> Maybe (Op, (Int,Int,Int))
valid_triple (a, b, c) =
       [Add, Subtract, Multiply, Divide]
    |> List.filterMap ((\op -> case do_op op a b of
            Ok d -> if c == modBy 10 d then Just (op,(a,b,d)) else Nothing
            Err _ -> Nothing))
    |> List.head

filled_triple : (Maybe Int, Maybe Int,  Maybe Int) -> Maybe (Int, Int, Int)
filled_triple (ma, mb, mc) = case (ma, mb, mc) of
    (Just a, Just b, Just c) -> Just (a,b,c)
    _ -> Nothing

verify_hyperjumps : List (Maybe Int) -> List (Maybe (Op, (Int,Int,Int)))
verify_hyperjumps = verify_triples (filled_triple >> Maybe.andThen valid_triple)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Browser.Document Msg
view model = 
    let
        game = model.game

        size = List.length game.numbers

        sequence = make_sequence game

        view_number i (n,pos) = 
            H.li
                []
                [ H.button
                    [ HE.onClick <| SelectNumber i
                    , HA.classList
                        [ ("selected", model.selected_number == Just i)
                        , ("used", pos /= Nothing)
                        ]
                    ]
                    [H.text <| fi n]
                ]

        view_sequence_item i mn =
            H.li
                []
                [ H.button
                    [ HE.onClick <| ClickSequence i ]
                    [ H.text <| case mn of
                        Just (n,from) -> fi n
                        Nothing -> "empty"
                    ]
                ]
    in
        { title = "Hyperjumps"
        , body = 
            [ H.ul
                [ HA.id "numbers"]
                (List.indexedMap view_number game.numbers)
            , H.ul
                [ HA.id "sequence" ]
                (List.indexedMap view_sequence_item sequence)
            , H.pre
                []
                [ H.text <| Debug.toString <| verify_hyperjumps <| List.map (Maybe.map first) sequence ]
            ]
        }
