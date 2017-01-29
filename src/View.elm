module View exposing (view, Msg(..))

import Html exposing (Html, text, textarea, button, div, p, pre, li, strong, span, main_)
import Html.Attributes exposing (disabled, id, placeholder, class, classList)
import Html.Events exposing (onInput, onClick)
import Html.Keyed exposing (ul)

import Maybe exposing (withDefault)
import Maybe.Extra exposing (values)

import Runner exposing (Model, RunState(Edit, ExitNormal, ExitAbnormal), showRatio, Output(Stdout, Stderr))
import Lexer exposing (showCommandInfix, Command)

import Dict
import Stack
import String.Extra exposing (fromCodePoints)

import Array

type Msg = MsgInput String | MsgCode String | MsgNext | MsgReset | MsgAuto

textCode : Model -> Html Msg
textCode model = textarea
    [ onInput MsgCode
    , placeholder "코드"
    , id "textCode"
    ]
    [ text model.rawCode ]

textStdin : Model -> Html Msg
textStdin model = textarea
    [ onInput MsgInput
    , placeholder "입력"
    , id "textStdin"
    ]
    [ text model.stdin ]

textOutput : Model -> Html a
textOutput {output} =
    let toHtml x = case x of
            Stdout a -> pre [ class "stdout" ] [ text a ]
            Stderr a -> pre [ class "stderr" ] [ text a ]
    in div [ id "output" ] <| Array.toList <| Array.map toHtml output

btnNext : Model -> Html Msg
btnNext _ = button [ onClick MsgNext, id "btnNext" ] [ text "다음▶" ]

btnReset : Model -> Html Msg
btnReset _ = button [ onClick MsgReset, id "btnReset" ] [ text "초기화" ]

btnAuto : Model -> Html Msg
btnAuto {autoRun} = button [ onClick MsgAuto, id "btnAuto" ] [ text <| if autoRun then "중단" else "실행" ]

stacks : Model -> Html a
stacks model =
    let (cursor, stacks) = model.stacks
        hash (key, stack) =
            let keyStr = toString key
                stackList = Stack.toList stack
                len = List.length stackList
                elem i r = (toString (len - i), li [] [text (showRatio r)])
                inner = List.indexedMap elem
                    <| if model.autoRun then
                            List.take 40 stackList -- for perfomance
                            -- TODO: make customizable
                       else stackList
                prefix = strong [class "stackNumber"] [text keyStr]
            in (keyStr,
                li [
                    classList [("selected", cursor == key)]
                ] [prefix, ul [class "stack"] inner]
            )
    in div [id "stacksDiv"]
        [ p [] [ text ("현재 스택: " ++ toString cursor) ]
        , ul [id "stacksList"] (List.map hash (Dict.toList stacks))
        ]

command : Int -> Command -> Html a
command index cmd = span [class "command"]
    [ span [class "cmdIndex"] [text (toString index)]
    , text (showCommandInfix cmd)
    ]

register : Model -> Html a
register {commands, registerLastCalled, register} =
    let hash ((num, heart), index) =
            let strNum = toString num
                strHeart = fromCodePoints [heart]
                keyNode =
                    [ text strNum
                    , span [class "comma"] [text ","]
                    , text strHeart
                    ]
                cmdStr = case Array.get index commands of
                    Just cmd -> showCommandInfix cmd
                    Nothing -> ""
            in (strNum ++ strHeart,
                li [
                    classList [("selected", registerLastCalled == Just (num, heart))]
                ]
                [ strong [class "registerKey"] keyNode
                , case Array.get index commands of
                    Just cmd -> command index cmd
                    Nothing -> text ""
                ]
            )
    in div [id "register"]
        [ p [] [text "등록된 명령어"]
        , ul [id "registerList"] (List.map hash (Dict.toList register))
        ]

lastCommand : Model -> Html a
lastCommand {commands, lastCommandIndex} = div [id "lastCommand"] <|
    case Array.get lastCommandIndex commands of
        Nothing -> []
        Just cmd -> [ text "실행된 명령어: ", command lastCommandIndex cmd ]

nextCommand : Model -> Html a
nextCommand {commands, nextCommandIndex, state} = div [id "nextCommand"] <|
    case Array.get nextCommandIndex commands of
        Nothing -> []
        Just cmd ->
            [ text "다음 명령어: "
            , case state of
                ExitNormal   -> text "정상적으로 종료"
                ExitAbnormal -> text "비정상적으로 종료"
                _ -> command nextCommandIndex cmd
            ]

each : List (a -> b) -> a -> List b
each funcs x = List.map ((|>) x) funcs

view : Model -> Html Msg
view model =
    let notEdit a = if model.state == Edit then Nothing else Just a
        isEdit a = if model.state == Edit then Just a else Nothing
        editArea = each <| values
            [ Just textCode
            , Just btnNext
            , notEdit btnAuto
            , notEdit btnReset
            , isEdit textStdin
            , notEdit textOutput
            ]
        runArea = each
            [ lastCommand
            , nextCommand
            , stacks
            , register
            ]
        area = each <| values 
            [ Just <| div [id "editArea"] << editArea
            , notEdit <| div [id "runArea"] << runArea
            ]
    in main_ [id "main"] (area model)
