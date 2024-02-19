module Hyperjumps exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import List.Extra
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
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

type alias NumberInfo = (Int, Bool)

type alias Game =
    { numbers : Array NumberInfo -- available numbers, and whether they must be the end of the sequence
    , sequence: List Int -- List of indices into the numbers
    }

type alias Model =
    { game : Game
    , found_sequences : Set (List Int)
    }

type Msg
    = ClickNumber Int -- click the number at given index
    | ClickSequence Int -- click the item in the sequence at given index
    | PickNumber Int -- Pick the first available instance of this number
    | Backspace
    | SaveSequence

{- makes a list of:
    Nothing: not filled
    Just (number, maybe_from): filled with number, which came from:
        Nothing -> it's fixed
        Just j -> index j in the list of numbers
-}
make_sequence : Game -> List Int
make_sequence game = 
       game.sequence
    |> List.filterMap (\i -> Array.get i game.numbers)
    |> List.map first
    |> List.reverse

main = Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init_game : Game
init_game = 
    { numbers = [6,2,5,5,7,5,5,7] |> List.map (\n -> (n, False)) |> (::) (1, True) |> Array.fromList
    , sequence = []
    }

init_model : Model
init_model = 
    { game = init_game
    , found_sequences = Set.empty
    }

nocmd model = (model, Cmd.none)

fi = String.fromInt
ff = String.fromFloat
tf = toFloat

pairs : List a -> List (a,a)
pairs l = List.map2 pair l (List.drop 1 l)

init: () -> (Model, Cmd Msg)
init _ = (init_model, Cmd.none)
used_in_sequence game i = List.member i game.sequence

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    let
        sequence = make_sequence model.game
        game = model.game

        noop = nocmd model
    in
        case (Debug.log "msg" msg) of
            ClickNumber i -> { model | game = use_number game i } |> nocmd

            ClickSequence i -> 
                { model | game = { game | sequence = game.sequence |> List.reverse |> List.take (i + 1) |> List.reverse } } |> nocmd

            PickNumber n -> 
                let
                    available_numbers =
                        game.numbers
                        |> Array.toList
                        |> List.indexedMap pair
                        |> List.filter (first >> (\i -> List.member i game.sequence) >> not)
                        |> List.filter (second >> first >> (==) n)

                in
                    case available_numbers |> List.head |> Maybe.map first of
                        Just i -> { model | game = use_number game i } |> nocmd
                        Nothing -> noop

            Backspace ->
                { model | game = { game | sequence = List.drop 1 game.sequence } } |> nocmd

            SaveSequence ->
                if valid_sequence game then
                    { model | found_sequences = Set.insert sequence model.found_sequences, game = { game | sequence = [] } } |> nocmd
                else
                    noop

use_number game i = if used_in_sequence game i then game else { game | sequence = i :: game.sequence }

verify_triples : ((a, a, a) -> b) -> List a -> List (Maybe b)
verify_triples fn sequence = 
    List.foldl
        (\c (ma,mb,prev) -> 
            let
                res = case (ma, mb) of
                    (Just a, Just b) -> Just (fn (a, b, c))
                    _ -> Nothing
            in
                (mb, Just c, prev ++ [res])
        )
        (Nothing, Nothing, [])
        sequence
    |> \(_,_,out) -> out

isOk : Result a b -> Bool
isOk r = case r of
    Ok _ -> True
    Err _ -> False

valid_triple : (Int, Int, Int) -> Result String (Op, (Int,Int,Int))
valid_triple (a,b,c) = 
       [Add, Subtract, Multiply, Divide]
    |> List.filterMap ((\op -> case do_op op a b of
            Ok d -> if c == modBy 10 d then Just (op,(a,b,d)) else Nothing
            Err _ -> Nothing))
    |> List.head
    |> \mres -> case mres of
        Just res -> Ok res
        Nothing -> Err "No sums work"

filled_triple : (Maybe Int, Maybe Int,  Maybe Int) -> Maybe (Int, Int, Int)
filled_triple (ma, mb, mc) = case (ma, mb, mc) of
    (Just a, Just b, Just c) -> Just (a,b,c)
    _ -> Nothing

--verify_hyperjumps : List (Maybe Int) -> List (Maybe (Op, (Int,Int,Int)))
verify_hyperjumps = verify_triples valid_triple

subscriptions : Model -> Sub Msg
subscriptions model =
       JD.field "key" JD.string
    |> JD.andThen (\k -> JD.oneOf
        [ String.toInt k |> Maybe.map (PickNumber >> JD.succeed) |> Maybe.withDefault (JD.fail "not a number")
        , case k of
            "Backspace" -> JD.succeed Backspace
            "Enter" -> JD.succeed SaveSequence
            _ -> JD.fail "Unrecognised key"
        ]
        )
    |> Browser.Events.onKeyUp

valid_sequence : Game -> Bool
valid_sequence game =
       (game_finished game)
    && (   make_sequence game
        |> verify_hyperjumps
        |> List.all (\v -> case v of
            Just (Err _) -> False
            _ -> True
           )
       )

game_finished : Game -> Bool
game_finished game = 
    game.sequence
    |> List.head
    |> Maybe.andThen (\i -> Array.get i game.numbers)
    |> Maybe.map second
    |> Maybe.withDefault False


view : Model -> Browser.Document Msg
view model = 
    let
        game = model.game

        sequence = make_sequence game

        verifications = verify_hyperjumps sequence

        finished = game_finished game

        last_two : Maybe (Int, Int)
        last_two = 
               sequence
            |> List.reverse
            |> List.take 2
            |> \m -> case m of
                b::a::_ -> Just (a,b)
                _ -> Nothing

        can_be_next : Int -> Bool
        can_be_next c =
            case last_two of
                Nothing -> False
                Just (a,b) -> isOk (valid_triple (a,b,c))

        view_number i (n, must_end) = 
            let
                possible = (last_two == Nothing || can_be_next n) && not finished
                used = used_in_sequence game i
            in
                H.li
                    []
                    [ H.button
                        [ HE.onClick <| ClickNumber i
                        , HA.classList
                            [ ("used", used )
                            , ("possible", possible)
                            , ("must-end", must_end)
                            ]
                        , HA.disabled <| used
                        ]
                        [H.text <| fi n]
                    ]

        view_sequence_item : Int -> (Int, Maybe (Result String (Op, (Int, Int, Int)))) -> Html Msg
        view_sequence_item i (n, verity) =
            H.li
                []
                [ H.button
                    [ HE.onClick <| ClickSequence i ]
                    [ H.text <| fi n
                    ]
                , H.div
                    [ HA.classList
                        [ ("explanation", True)
                        ]
                    ]
                    [ case verity of
                        Nothing -> H.text ""
                        Just (Ok operation) -> H.span [ HA.class "explanation" ] (describe_op operation)
                        Just (Err msg) -> H.span [ HA.class "error" ] [ H.text msg ]
                    ]
                ]

        gap_radiuses = 2

        big_r = 100

        num_planets = 1 + (Array.length game.numbers)

        home_index = num_planets - 1

        planets : List (Int, Maybe NumberInfo)
        planets = (home_index, Nothing)::(game.numbers |> Array.toList |> List.map Just |> List.indexedMap pair)

        radius = big_r * (sin (pi / (tf num_planets))) / (1 + gap_radiuses)

        coords_for i = 
            let
                angle = ((tf i) + 1) / (tf num_planets) * 2 * pi
            in  
                (big_r * (sin angle), big_r * (cos angle))

        view_planet : (Int, Maybe NumberInfo) -> Svg Msg
        view_planet (i,minfo) =
            let
                n = minfo |> Maybe.map first |> Maybe.withDefault -1
                must_end = minfo |> Maybe.map second |> Maybe.withDefault False
                used = used_in_sequence game i
                possible = minfo /= Nothing && (last_two == Nothing || can_be_next n) && (not used) && not finished && (not must_end || (List.length sequence) >= 2)
                is_home = minfo == Nothing
                (cx, cy) = coords_for i
                position_in_sequence = List.Extra.elemIndex i (List.reverse game.sequence)
            in
                Svg.g
                    [ SA.transform <| "translate(" ++ (ff cx)++", "++(ff cy)++")"
                    , SE.onClick <| if is_home then ClickSequence (-1) else case position_in_sequence of
                        Just j -> ClickSequence j
                        Nothing -> ClickNumber i
                    , SA.class <| String.join " " <| List.map first <| List.filter second <|
                        [ ("planet", True)
                        , ("possible", possible)
                        , ("used", used)
                        , ("must-end", must_end)
                        ]
                    ]
                    [ Svg.circle
                        [ SA.r <| ff radius
                        ]
                        []
                    , Svg.text_
                        [ SA.fill "white"
                        , SA.fontSize "6"
                        , SA.textAnchor "middle"
                        , SA.dominantBaseline "middle"
                        ]
                        [ Svg.text <| case minfo of
                            Nothing -> "Home"
                            Just (nn,_) -> fi n
                        ]
                    ]

        view_jump (from, to) =
            let
                (x1,y1) = coords_for from
                (x2,y2) = coords_for to
            in
                Svg.line
                    [ SA.x1 <| ff x1
                    , SA.y1 <| ff y1
                    , SA.x2 <| ff x2
                    , SA.y2 <| ff y2
                    , SA.stroke "white"
                    , SA.class "jump"
                    ]
                    []

        view_diagram =
            Svg.svg
                [ HA.attribute "viewBox" <| String.join " " (List.map ((*) (1.2 * big_r) >> String.fromFloat) [-1, -1, 2, 2])
                , SA.id "diagram"
                ]
                [ Svg.circle
                    [ SA.cx "0"
                    , SA.cy "0"
                    , SA.r <| ff big_r
                    , SA.fill "none"
                    , SA.stroke "grey"
                    ]
                    []

                , Svg.g
                    []
                    (home_index::(List.reverse game.sequence) |> pairs |> List.map view_jump)

                , Svg.g
                    []
                    (List.map view_planet planets)
                ]
    in
        { title = "Hyperjumps"
        , body = 
            [ H.section
                []
                [ view_diagram ]

            , H.section
                []
                
                [ H.ul
                    [ HA.id "sequence"
                    , HA.class "number-list"
                    ]
                    (List.indexedMap view_sequence_item <| List.map2 pair sequence verifications)

                , H.button
                    [ HE.onClick Backspace
                    , HA.disabled <| 0 == (List.length game.sequence)
                    ]
                    [ H.text "Undo" ]

                , H.button
                    [ HE.onClick SaveSequence
                    , HA.disabled <| not <| valid_sequence <| game
                    ]
                    [ H.text "Launch" ]
                ]

            , H.section
                []
                [ H.h2 [] [H.text "Found sequences" ]
                , H.ul
                    [ HA.id "found-sequences" ]
                    (List.map (\l ->
                        H.li
                            []
                            [ H.ul
                                [ HA.class "number-list" ]
                                (List.map (\n -> H.li [] [H.text <| fi n]) l)
                            ]
                        )
                        (model.found_sequences |> Set.toList |> List.sortBy (\s -> (List.length s, s)))
                    )
                ]
            ]
        }

describe_op : (Op, (Int,Int,Int)) -> List (Html Msg)
describe_op (op, (a,b,c)) =
    let
        op_symbol = case op of
            Add -> "+"
            Subtract -> "-"
            Multiply -> "×"
            Divide -> "÷"

        important_digits n = H.strong [] [ H.text <| fi n ]
        unimportant_digits n = H.small [] [ H.text <| fi n ]
        operator s = H.code [] [ H.text s ]

        space = H.text " "
    in
        [ important_digits a
        , space
        , operator op_symbol
        , space
        , important_digits b
        , space
        , operator "="
        , space
        , if (c//10 == 0) then H.text "" else unimportant_digits (c//10)
        , important_digits (modBy 10 c)
        ]

