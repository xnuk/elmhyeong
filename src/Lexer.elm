module Lexer exposing (HeartTree(..), CommandT(Command), Command, commands, showHeartTreePrefix, showCommandPrefix, showHeartTreeInfix, showCommandInfix)

import Tokenizer exposing (Token(Start, End, Single, Dot, Hangul, Heart), Heart(Question, Exclamation), Inst(Push, Add, Mul, Neg, Inv, Swi), InstStart(MaybePush, AddMul, NegInvSwi), showInst)
import List
import List.Extra exposing (break)
import Maybe exposing (withDefault)
import String
import String.Extra exposing (fromCodePoints)

type HeartTree = FilledHeart Int | EmptyHeart | NoHeart | Compare HeartTree HeartTree | Equal HeartTree HeartTree
type SemiCmd = SemiInst (Inst, Int) | SemiDot Int | SemiHeart Heart
type CommandT a = Command Inst Int Int a
type alias Command = CommandT HeartTree

type ParseState = ParseDot | ParseHeart
type St = St ParseState (Maybe (CommandT (List Heart)))

showHeartTreePrefix : HeartTree -> String
showHeartTreePrefix tree = case tree of
    FilledHeart c -> fromCodePoints [c]
    EmptyHeart -> "♡"
    NoHeart -> "_"
    Compare x y -> "?" ++ showHeartTreePrefix x ++ showHeartTreePrefix y
    Equal   x y -> "!" ++ showHeartTreePrefix x ++ showHeartTreePrefix y

showHeartTreeInfix : HeartTree -> String
showHeartTreeInfix hearttree = case hearttree of
    FilledHeart c -> fromCodePoints [c]
    EmptyHeart -> "♡"
    NoHeart -> ""
    Compare left right -> showHeartTreeInfix left ++ "?" ++ showHeartTreeInfix right
    Equal   left right -> showHeartTreeInfix left ++ "!" ++ showHeartTreeInfix right

showCommand : (HeartTree -> String) -> Command -> String
showCommand showHeartTree (Command i charlen dotlen heartTree) =
    let xs = [showInst i, toString charlen, toString dotlen]
        heart = showHeartTree heartTree
        result = if heart == "_" then xs else xs ++ [heart]
    in String.concat (List.intersperse " " result)

showCommandPrefix : Command -> String
showCommandPrefix = showCommand showHeartTreePrefix

showCommandInfix : Command -> String
showCommandInfix = showCommand showHeartTreeInfix

hangulLength : List Token -> Int
hangulLength =
    let f x = case x of
            Single _ -> True
            Start _ -> True
            End _ -> True
            Hangul -> True
            _ -> False
    in List.length << List.filter f

isEnd : InstStart -> Token -> Bool
isEnd z = case z of
    MaybePush -> \x -> x == End Push
    AddMul    -> \x -> x == End Add || x == End Mul
    NegInvSwi -> \x -> x == End Neg || x == End Inv || x == End Swi

listTail : List a -> List a
listTail = withDefault [] << List.tail

-- 한글 글자 해석 단계를 가장 먼저 순회할 수 있습니다.
-- - 말줄임표 해석 단계에선 `.`과 세 개짜리 말줄임표 문자들만 소모합니다.
-- - 하트 구역 해석 단계에선 `!`, `?`와 하트 문자들만 소모합니다.
-- - 두 해석 단계 모두 한글 글자 해석 단계에서 해석이 가능하다면 해석 단계를 중단합니다.
instStep : List Token -> (Maybe SemiCmd, List Token)
instStep tokens =
    let next = listTail tokens
    in case List.head tokens of
        Nothing -> (Nothing, [])
        Just (Single a) -> (Just <| SemiInst (a, 1), next)
        Just (Start a) ->
            let (left, right) = break (isEnd a) next -- End 토큰이 있기 전까지 잡아 left에 저장. End 토큰 이후로는 right에 저장.
                newNext = listTail right
            in case List.head right of
                Nothing -> (Nothing, next) -- End 토큰이 없어서 left == tokens인 경우
                Just (End end) ->
                    let len = hangulLength left + 2 -- left에는 Start와 End 토큰이 없기 때문에 2를 더함.
                    in (Just <| SemiInst (end, len), newNext)

                _ -> (Nothing, next)

        Just (Dot a) -> (Just <| SemiDot a, next)
        Just (Heart a) -> (Just <| SemiHeart a, next)
        Just _ -> (Nothing, next)

inst : List Token -> List SemiCmd
inst =
    let f cmds tokens =
            if List.isEmpty tokens then
                cmds
            else case instStep tokens of
                (Nothing, next) -> f cmds next
                (Just a, next) -> f (cmds ++ [a]) next
    in f []

addDot : Int -> Maybe (CommandT a) -> Maybe (CommandT a)
addDot n cmd =
    let func (Command i len dot heart) = Command i len (dot+n) heart
    in Maybe.map func cmd

addHeart : a -> Maybe (CommandT (List a)) -> Maybe (CommandT (List a))
addHeart h cmd =
    let func (Command i len dot heart) = Command i len dot (heart ++ [h])
    in Maybe.map func cmd

-- Inst가 이미 해석된 단계입니다.
-- Dot만 해석하고, Heart는 모아두기만 합니다.
dots : St -> List SemiCmd -> List (CommandT (List Heart))
dots (St parseState workingCmd) semicmds =
    let next = listTail semicmds
    in case List.head semicmds of
        Nothing -> case workingCmd of
            Nothing -> []
            Just a -> [a]
        Just z -> case z of
            SemiInst (a, len) ->
                let nextList _ = dots (St ParseDot <| Just (Command a len 0 [])) next -- lazy. lazy하지 않으면 아마 TCO가 안 될 것.
                in case workingCmd of
                    Nothing -> nextList ()
                    Just cmd -> cmd :: nextList ()
            SemiDot n ->
                if parseState == ParseDot then
                    dots (St ParseDot (addDot n workingCmd)) next
                else
                    dots (St parseState workingCmd) next
            SemiHeart h -> dots (St ParseHeart (addHeart h workingCmd)) next

treeCompare : List Heart -> HeartTree
treeCompare hearts = if List.isEmpty hearts then NoHeart else
    let (left, right) = break ((==) Question) hearts
    in case List.head right of
        Nothing -> treeEqual hearts
        Just _ -> Compare (treeEqual left) (treeCompare (listTail right))

treeEqual : List Heart -> HeartTree
treeEqual hearts = if List.isEmpty hearts then NoHeart else
    let (left, right) = break ((==) Exclamation) hearts
    in case List.head right of
        Nothing -> treeHeart hearts
        Just _ -> Equal (treeHeart left) (treeEqual (listTail right))

treeHeart : List Heart -> HeartTree
treeHeart hearts = case List.head hearts of
    Just (Tokenizer.FilledHeart c) -> FilledHeart c
    Just (Tokenizer.EmptyHeart) -> EmptyHeart
    _ -> NoHeart

commands : List Token -> List Command
commands = inst >> dots (St ParseHeart Nothing) >> List.map (\(Command a charlen dotlen heart) -> Command a charlen dotlen (treeCompare heart))
