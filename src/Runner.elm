module Runner exposing (..)

import Array exposing (Array)
import Lexer exposing (Command, CommandT(Command), HeartTree(FilledHeart, EmptyHeart, NoHeart, Compare, Equal))
import Tokenizer exposing (Inst(Push, Add, Mul, Neg, Inv, Swi))
import Stack exposing (Stack)
import Dict exposing (Dict)
import String.Extra as S exposing (fromCodePoints, toCodePoints)
import Ratio exposing (Rational, over)
import Ratio.Infix exposing ((|<|), (|==))
import Parser exposing (parse)
import List
import Maybe exposing (withDefault)
import String

type alias RegisterKey = (Int {- number -}, Int {- heart -})
type RunState = Edit | Run | ExitNormal | ExitAbnormal
type Output = Stdout String | Stderr String

type alias Model =
    { commands : Array Command
    , nextCommandIndex : Int -- 다음에 실행할 명령어
    , lastCommandIndex : Int -- 실행된 명령어
    , stacks : (Int {- current index -}, Dict Int (Stack Rational))
    , register : Dict RegisterKey Int
    , registerJumpedIndex : Maybe Int -- 등록된 명령어로 넘어가게 한 명령어 중 가장 최근에 실행된 명령어
    , registerLastCalled : Maybe RegisterKey
    , stdin : String
    , output : Array Output
    , state : RunState
    , rawCode : String
    , autoRun : Bool
    }

nan : Rational
nan = over 0 0

sum : List Rational -> Rational
sum = List.foldl Ratio.add (over 0 1)

neg : Rational -> Rational
neg = Ratio.negate

inv : Rational -> Rational
inv r =
    let (a, b) = Ratio.split r
    in if b == 0 then nan else over b a

product : List Rational -> Rational
product = List.foldl Ratio.multiply (Ratio.fromInt 1)

strRatio : Rational -> String
strRatio r =
    let (a, b) = Ratio.split r
        strInt x = if x < 0 then S.fromInt (-x) else fromCodePoints [x]
    in if b == 0 then "너무 커엇..."
    else if b == 1 then strInt a
    else strInt (floor (toFloat a / toFloat b))

showRatio : Rational -> String
showRatio r =
    let (a, b) = Ratio.split r
    in if b == 0 then "NaN"
    else if b == 1 then S.fromInt a
    else S.fromInt a ++ "/" ++ S.fromInt b

initModel : Model
initModel =
    { commands = Array.empty
    , nextCommandIndex = 0
    , lastCommandIndex = -1
    , stacks = (3, Dict.empty)
    , register = Dict.empty
    , registerJumpedIndex = Nothing
    , registerLastCalled = Nothing
    , stdin = ""
    , output = Array.empty
    , state = Edit
    , rawCode
        =  "혀어어어어어어어엉........ 핫. 혀엉..... 흑... 하앗... 흐윽... 형.  하앙.\n"
        ++ "혀엉.... 하앙... 흐윽... 항. 항. 형... 하앙. 흐으윽... 형... 흡... 혀엉..\n"
        ++ "하아아앗. 혀엉.. 흡... 흐읍... 형.. 하앗. 하아앙... 형... 하앙... 흐윽...\n"
        ++ "혀어어엉.. 하앙. 항. 형... 하앙. 혀엉.... 하앙. 흑... 항. 형... 흡  하앗.\n"
        ++ "혀엉..... 흑. 흣"
    , autoRun = False
    }

resetRunner : Model -> Model
resetRunner model =
    {model|
      commands = Array.fromList (parse model.rawCode)
    , nextCommandIndex = 0
    , lastCommandIndex = -1
    , stacks = (3, Dict.empty)
    , register = Dict.empty
    , registerJumpedIndex = Nothing
    , registerLastCalled = Nothing
    , output = Array.empty
    , autoRun = False
    }

putOutput : Output -> Model -> Model
putOutput x model =
    let output = model.output
        last = Array.length output - 1
        merge a b = case (a, b) of
            (Stdout c, Stdout d) -> Just (Stdout (c ++ d))
            (Stderr c, Stderr d) -> Just (Stderr (c ++ d))
            _ -> Nothing
        setOutput o = {model| output = o}

    in setOutput <| case Array.get last model.output of
        Nothing -> Array.push x output
        Just s -> case merge s x of
            Just z -> Array.set last z output
            Nothing -> Array.push x output

modifyStack : Int -> Model -> (Stack Rational -> Stack Rational) -> Model
modifyStack cursor model f =
    let (curr, stacks) = model.stacks
        modified = f <| withDefault Stack.initialise <| Dict.get cursor stacks
    in {model| stacks = (curr, Dict.insert cursor modified stacks)}

modifyStackWith : Int -> Model -> (Stack Rational -> (a, Stack Rational)) -> (a, Model)
modifyStackWith cursor model f =
    let (curr, stacks) = model.stacks
        (res, modified) = f <| withDefault Stack.initialise <| Dict.get cursor stacks
    in (res, {model| stacks = (curr, Dict.insert cursor modified stacks)})

push : Int -> Rational -> Model -> Model
push cursor r model = if model.state /= Run then model else case cursor of
    1 -> putOutput (Stdout (strRatio r)) model
    2 -> putOutput (Stderr (strRatio r)) model

    _ -> modifyStack cursor model (Stack.push r)

pushCurrent : Rational -> Model -> Model
pushCurrent r model =
    let (current, _) = model.stacks
    in push current r model

-- left-first push
pushMany : Int -> List Rational -> Model -> Model
pushMany cursor rs model = if model.state /= Run then model else case cursor of
    1 -> putOutput (Stdout (String.concat (List.map strRatio rs))) model
    2 -> putOutput (Stderr (String.concat (List.map strRatio rs))) model

    _ -> modifyStack cursor model <| \stack -> List.foldl Stack.push stack rs

pushManyCurrent : List Rational -> Model -> Model
pushManyCurrent rs model =
    let (current, _) = model.stacks
    in pushMany current rs model

-- right-first pop
popManyCurrent : Int -> Model -> (List Rational, Model)
popManyCurrent count model = if model.state /= Run then ([], model) else case Tuple.first model.stacks of
    1 -> ([], {model| state = ExitNormal})
    2 -> ([], {model| state = ExitAbnormal})

    cursor ->
        let func _ (xs, stack) =
                let (r, newStack) = Stack.pop stack
                in (withDefault nan r :: xs, newStack)
        in modifyStackWith cursor model <| \stack ->
            List.foldr func ([], stack) (List.repeat count ())

popCurrent : Model -> (Rational, Model)
popCurrent model = if model.state /= Run then (nan, model) else case Tuple.first model.stacks of
    1 -> (nan, {model| state = ExitNormal})
    2 -> (nan, {model| state = ExitAbnormal})

    cursor -> modifyStackWith cursor model <| \stack ->
        let (r, newStack) = Stack.pop stack
        in (withDefault nan r, newStack)

move : Int -> Model -> Model
move n model = if model.state /= Run then model else
    let (_, stacks) = model.stacks
    in {model| stacks = (n, stacks)}

goNext : Model -> Model
goNext model = if model.state /= Run then model else
    let nextIndex = model.nextCommandIndex + 1
        size = Array.length model.commands
    in if nextIndex >= size then
        {model| nextCommandIndex = 0}
    else
        {model| nextCommandIndex = model.nextCommandIndex + 1}

runHeart : Int -> HeartTree -> Model -> Model
runHeart chardot hearttree model = if model.state /= Run then model else case hearttree of
    NoHeart -> goNext model
    FilledHeart heart -> case Dict.get (chardot, heart) model.register of
        Just index ->
            if model.nextCommandIndex == index then
                goNext model
            else {model|
                -- jump to index
                registerJumpedIndex = Just model.nextCommandIndex,
                registerLastCalled = Just (chardot, heart),
                nextCommandIndex = index
            }

        Nothing -> goNext {model|
            -- register a index and go next
            register = Dict.insert (chardot, heart) model.nextCommandIndex model.register
        }

    EmptyHeart -> case model.registerJumpedIndex of
        Just index -> {model| nextCommandIndex = index}
        Nothing -> goNext model

    Compare left right ->
        let (x, newModel) = popCurrent model
            isNan = Ratio.denominator x == 0
            leftOrRight =
                if isNan then right
                else if x |<| over chardot 1 then left -- x < chardot/1
                else right
        in runHeart chardot leftOrRight newModel

    Equal left right ->
        let (x, newModel) = popCurrent model
            isNan = Ratio.denominator x == 0
            leftOrRight =
                if x |== chardot then left -- x == chardot/1
                else right
        in runHeart chardot leftOrRight newModel

next : Model -> Model
next model = case model.state of
    ExitNormal -> model
    ExitAbnormal -> model
    Edit -> {model| state = Run}
        |> resetRunner
        |> pushMany 0 (List.map Ratio.fromInt (toCodePoints model.stdin))
        |> next
    Run -> case Array.get model.nextCommandIndex model.commands of
        Nothing ->
            if model.nextCommandIndex == 0 then
                model -- infinite loop
            else
                {model| nextCommandIndex = 0} -- rotate
        Just (Command inst charlen dotlen hearttree) ->
            let instModel = case inst of
                    Push -> pushCurrent (over (charlen * dotlen) 1) model
                    Add ->
                        let (res, newModel) = popManyCurrent charlen model
                        in push dotlen (sum res) newModel
                    Mul ->
                        let (res, newModel) = popManyCurrent charlen model
                        in push dotlen (product res) newModel
                    Neg ->
                        let (res, newModel) = popManyCurrent charlen model
                            negList = List.map neg res
                        in newModel
                            |> pushManyCurrent negList
                            |> push dotlen (sum negList)
                    Inv ->
                        let (res, newModel) = popManyCurrent charlen model
                            invList = List.map inv res
                        in newModel
                            |> pushManyCurrent invList
                            |> push dotlen (product invList)
                    Swi ->
                        let (res, newModel) = popCurrent model
                        in newModel
                            |> pushMany dotlen (List.repeat charlen res)
                            |> pushCurrent res
                            |> move dotlen
                heartModel = runHeart (charlen * dotlen) hearttree instModel
            in {heartModel| lastCommandIndex = model.nextCommandIndex}
