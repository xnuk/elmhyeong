module Tokenizer exposing (tokenize, showInst, Inst(..), InstStart(..), Heart(..), Token(..))

--import String
import String.Extra exposing (toCodePoints, fromCodePoints)
import List
import Set exposing (Set)
import Dict
import Maybe exposing (withDefault)

type Inst
    = Push -- 형, 엉
    | Add  -- 항, 앙
    | Mul  -- 핫, 앗
    | Neg  -- 흣, 읏
    | Inv  -- 흡, 읍
    | Swi  -- 흑, 윽

type InstStart
    = MaybePush -- 혀
    | AddMul -- 하
    | NegInvSwi -- 흐

type Heart
    = FilledHeart Int -- ♥
    | EmptyHeart -- ♡
    | Question -- ?
    | Exclamation -- !

type Token = Hangul | Single Inst | Start InstStart | End Inst | Dot Int | Heart Heart

showInst : Inst -> String
showInst i = case i of
    Push -> "형"
    Add  -> "항"
    Mul  -> "핫"
    Neg  -> "흣"
    Inv  -> "흡"
    Swi  -> "흑"

u가 = 0xAC00
u힣 = 0xD7A3

u형 = 0xD615
u항 = 0xD56D
u핫 = 0xD56B
u흣 = 0xD763
u흡 = 0xD761
u흑 = 0xD751

u엉 = 0xC5C9
u앙 = 0xC559
u앗 = 0xC557
u읏 = 0xC74F
u읍 = 0xC74D
u윽 = 0xC73D

u혀 = 0xD600
u하 = 0xD558
u흐 = 0xD750

emptyHeart = 0x2661 -- ♡

threeDot1 = 0x2026 -- …
threeDot2 = 0x22EF -- ⋯
threeDot3 = 0x22EE -- ⋮
oneDot = 0x2E -- .

questionMark = 0x3F -- ?
exclamationMark = 0x21 -- !

hangulDict = Dict.fromList
    [ (u형, Single Push)
    , (u항, Single Add)
    , (u핫, Single Mul)
    , (u흣, Single Neg)
    , (u흡, Single Inv)
    , (u흑, Single Swi)

    , (u엉, End Push)
    , (u앙, End Add)
    , (u앗, End Mul)
    , (u읏, End Neg)
    , (u읍, End Inv)
    , (u윽, End Swi)

    , (u혀, Start MaybePush)
    , (u하, Start AddMul)
    , (u흐, Start NegInvSwi)
    ]

miscDict = Dict.fromList
    [ (emptyHeart,      Heart EmptyHeart)
    , (questionMark,    Heart Question)
    , (exclamationMark, Heart Exclamation)

    , (oneDot,    Dot 1)
    , (threeDot1, Dot 3)
    , (threeDot2, Dot 3)
    , (threeDot3, Dot 3)
    ]

filledHearts : Set Int
filledHearts = Set.fromList (toCodePoints "♥❤💕💖💗💘💙💚💛💜💝")

tokenize : String -> List Token
tokenize str =
    let func c =
            if u가 <= c && c <= u힣 then
                Just <| withDefault Hangul (Dict.get c hangulDict)
            else if Set.member c filledHearts then
                Just <| Heart (FilledHeart c)
            else
                Dict.get c miscDict
    in List.filterMap func (toCodePoints str)
