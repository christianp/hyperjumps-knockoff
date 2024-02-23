module Hyperjumps exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import List.Extra
import Random as R
import Random.List
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Tuple exposing (first, second, pair)

generate_game : R.Generator Game
generate_game =
    let
        generate_next prev (a,b) = 
            let
                outputs : List Int
                outputs = 
                       [Add, Subtract, Multiply, Divide]
                    |> List.filterMap ((\op -> case do_op op a b of
                        Ok d -> 
                            let
                                c = modBy 10 d
                            in
                                if c /=0 then Just c else Nothing
                        Err _ -> Nothing))
                    |> Debug.log ("outputs" ++ (Debug.toString (a,b)))

                rc : R.Generator Int
                rc = case outputs of
                    q::rest -> R.uniform q rest
                    [] -> R.uniform 0 []
            in
                if List.length prev < 9 - 3 then
                    R.andThen (\c -> generate_next (a::prev) (b,c)) rc
                else
                    R.map (\c -> c::b::a::prev) rc
    in
       R.pair (R.int 1 9) (R.int 1 9)
    |> R.andThen (generate_next [])
    |> R.andThen (\l -> case l of
        a::rest -> Random.List.shuffle rest |> R.map (\srest -> (a,True)::(List.map (\x -> (x,False)) srest))
        [] -> R.uniform [] []
       )
    |> R.map Array.fromList
    |> R.map (\numbers -> { init_game | numbers = numbers })

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

type Screen
    = RulesScreen
    | GameScreen


type alias Model =
    { game : Game
    , found_sequences : Set (List Int)
    , screen : Screen
    }

type Msg
    = ClickNumber Int -- click the number at given index
    | ClickSequence Int -- click the item in the sequence at given index
    | PickNumber Int -- Pick the first available instance of this number
    | Backspace
    | SaveSequence
    | SetGame Game
    | SetScreen Screen

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
    , screen = RulesScreen
    }

nocmd model = (model, Cmd.none)

fi = String.fromInt
ff = String.fromFloat
tf = toFloat

pairs : List a -> List (a,a)
pairs l = List.map2 pair l (List.drop 1 l)

init: () -> (Model, Cmd Msg)
init _ = (init_model, R.generate SetGame generate_game)
used_in_sequence game i = List.member i game.sequence

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    let
        sequence = make_sequence model.game
        game = model.game

        noop = nocmd model
    in
        case (Debug.log "msg" msg) of
            ClickNumber i -> case List.head game.sequence of
                Just 0 -> model |> nocmd
                _ -> { model | game = use_number game i } |> nocmd

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

            SetGame ngame -> { model | game = ngame } |> nocmd

            SetScreen screen -> { model | screen = screen } |> nocmd

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

type alias IntTriple = (Int,Int,Int)
type alias OpError = (String, IntTriple)
type alias OpResult = (Op, IntTriple)

valid_triple : (Int, Int, Int) -> Result OpError OpResult
valid_triple (a,b,c) = 
       [Add, Subtract, Multiply, Divide]
    |> List.filterMap ((\op -> case do_op op a b of
            Ok d -> if c == modBy 10 d then Just (op,(a,b,d)) else Nothing
            Err _ -> Nothing))
    |> List.head
    |> \mres -> case mres of
        Just res -> Ok res
        Nothing -> Err ("No sums work", (a,b,c))

filled_triple : (Maybe Int, Maybe Int,  Maybe Int) -> Maybe (Int, Int, Int)
filled_triple (ma, mb, mc) = case (ma, mb, mc) of
    (Just a, Just b, Just c) -> Just (a,b,c)
    _ -> Nothing

--verify_hyperjumps : List (Maybe Int) -> List (Maybe (Op, IntTriple))
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
    && (List.length game.sequence >= 3)
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
    { title = "Hyperjumps"
    , body = [ case model.screen of
        RulesScreen -> H.main_ [ HA.id "rules" ] (view_rules model)
        GameScreen -> H.main_ [ HA.id "game" ] (view_game model)
        ]
    }

para text = H.p [] [ H.text text ]

view_rules : Model -> List (Html Msg)
view_rules model =
    [ H.h1 [] [ H.text "Hyperjumps" ]
    , para "Jump between the planets and try to end up at the destination planet."
    , para "Your first two jumps are free, but after that, your jumps must follow this rule:"
    , para "Combine the number of the previous planet and the one you're currently on, with either addition, subtraction, multiplication or division, to make a positive whole number."
    , para "You can jump to planets whose number is the same as the last digit as the result of the combination you choose."
    , H.p [] [H.text "For example, if you last visited ", important_digits 6, H.text " and then ", important_digits 3, H.text", you can combine them in the following ways:" ]
    , H.ul
        []
        [ H.li [] (describe_op (Add, (6,3,9)))
        , H.li [] (describe_op (Subtract, (6,3,3)))
        , H.li [] (describe_op (Multiply, (6,3,18)))
        , H.li [] (describe_op (Divide, (6,3,2)))
        ]
    , H.p [] [H.text "So you can visit planets numbered ", important_digits 9, H.text ", ", important_digits 3, H.text ", ", important_digits 8, H.text " or ", important_digits 2, H.text "."]
    , H.p [] [H.text "Once you've reached the destination planet, click the ", H.strong [] [H.text "launch"], H.text " button to finish the sequence."]
    , para "Try to find as many different sequences as you can."
    , H.button 
        [ HE.onClick (SetScreen GameScreen)
        ]
        [ H.text "Play" ]
    ]

view_game : Model -> List (Html Msg)
view_game model = 
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

        gap_radiuses = 2

        big_r = 100

        num_planets = 1 + (Array.length game.numbers)

        home_index = num_planets - 1

        planets : List (Int, Maybe NumberInfo)
        planets = (home_index, Nothing)::(game.numbers |> Array.toList |> List.map Just |> List.indexedMap pair)

        radius = big_r * (sin (pi / (tf num_planets))) / (1 + gap_radiuses)

        angle_for i = ((tf i) + 1) / (tf num_planets) * 2 * pi

        coords_for i = 
            let
                angle = angle_for i
            in  
                (big_r * (cos angle), big_r * (sin angle))

        rocket_planet = game.sequence |> List.head |> Maybe.withDefault home_index

        (rocket_x, rocket_y) = coords_for rocket_planet

        rocket_angle = (angle_for rocket_planet) * 180 / pi - 90

        at_destination = rocket_planet == 0

        last_move = verifications |> List.reverse |> List.head

        last_move_is_wrong = 
               case last_move of
                   Just (Just (Err _)) -> True
                   _ -> False

        ended_too_soon = at_destination && List.length game.sequence < 3

        hint = 
            if last_move_is_wrong || ended_too_soon then
                "Click on a previous planet."
            else
                case (List.head game.sequence, List.reverse sequence) of
                    (Just i, a::b::_) -> 
                        if i == 0 then 
                            "You've reached the destination. Launch!"
                        else
                            "Find a planet you can get to by combining "++(fi b)++" and "++(fi a)++"."

                    _ -> "Pick any planet to jump to."

        last_move_desc = 
            last_move |> Maybe.map (\ld -> 
                if ended_too_soon then
                    [ H.text "You jumped to the destination too soon. You must visit at least 3 planets." ]
                else
                    case ld of
                        Nothing -> 
                            let
                                n =    Array.get rocket_planet game.numbers
                                    |> Maybe.map (first >> fi)
                                    |> Maybe.withDefault "Home"
                            in
                                [ H.text <| "Jumped to "++n ]

                        Just (Ok operation) -> describe_op operation

                        Just (Err err) -> describe_error err
            )

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
                diff = (tf <| (from - to)) / (tf num_planets) |> (\x -> if x>0.5 then x-1 else x) |> (\y -> if y < -0.5 then y+1 else y)
                angle = diff * pi |> abs
                d = big_r * (tan angle)
                sd = d |> abs |> ff
                sweep = if diff >0 then "1" else "0"
            in
                if abs diff == 0.5 then
                    Svg.line
                        [ SA.x1 <| ff x1
                        , SA.y1 <| ff y1
                        , SA.x2 <| ff x2
                        , SA.y2 <| ff y2
                        , SA.class "jump"
                        ]
                        []
                else
                    Svg.path
                        [ SA.d <| String.join " " ["M",ff x1,ff y1,"A", sd, sd, "0", "0", sweep, ff x2, ff y2]
                        , SA.class "jump"
                        , HA.attribute "data-d" <| ff d
                        , HA.attribute "data-diff" <| ff <| Debug.log "diff" diff
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

                , Svg.image
                    [ SA.xlinkHref "rocket.svg#rocket"
                    , SA.width "10"
                    , SA.height "18.990"
                    , SA.transform <| "translate("++(ff rocket_x)++" "++(ff rocket_y)++") rotate("++(ff rocket_angle)++") translate(0 "++(ff <| -radius)++") translate(-5 -15.7)"
                    ]
                    []

                ]
    in
        [ H.section
            [ HA.id "hint" ]
            [ case last_move_desc of
                Nothing -> H.text ""
                Just desc -> H.p [] ([H.strong [] [H.text "Last move:"], H.text " "]++desc)

            , H.p [] [ H.strong [] [H.text "Next move:"], H.text " ", H.text hint ]
            ]

        , H.button
            [ HE.onClick (SetScreen RulesScreen)
            , HA.id "how-to-play"
            ]
            [ H.text "How to play" ]

        , view_diagram

        , H.section
            [ HA.id "controls" ]
            [ H.button
                [ HE.onClick SaveSequence
                , HA.disabled <| not <| valid_sequence <| game
                , HA.id "launch"
                ]
                [ H.text "Launch" ]
            ]

        , H.section
            [ HA.id "found-sequences" ]
            [ H.h2 [] [H.text "Found sequences" ]
            , H.ul
                []
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

space = H.text " "
important_digits n = H.strong [] [ H.text <| fi n ]
unimportant_digits n = H.small [] [ H.text <| fi n ]
operator s = H.code [] [ H.text s ]

describe_op : (Op, IntTriple) -> List (Html Msg)
describe_op (op, (a,b,c)) =
    let
        op_symbol = case op of
            Add -> "+"
            Subtract -> "-"
            Multiply -> "×"
            Divide -> "÷"

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

describe_error : (String, IntTriple) -> List (Html Msg)
describe_error (msg, (a,b,c)) =
    [ H.text msg
    , H.text ":"
    , space
    , important_digits a
    , space
    , operator "·"
    , space
    , important_digits b
    , space
    , operator "≠"
    , space
    , important_digits c
    , space
    ]
