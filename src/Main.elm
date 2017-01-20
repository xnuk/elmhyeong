module Main exposing (main)

import Html exposing (program)
import Platform exposing (Program)

import View exposing (view, Msg(..))
import Runner exposing (Model, initModel, next, resetRunner, RunState(Edit, Run))

import Platform.Cmd as Cmd exposing (Cmd)
import Platform.Sub as Sub exposing (Sub)

import Time exposing (every, millisecond)

main : Program Never Model Msg
main = program { init = (initModel, Cmd.none), view = view, update = update, subscriptions = subscriptions}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (\x -> (x, Cmd.none)) <| case msg of
    MsgInput a -> {model| stdin = a}
    MsgCode a -> {model| rawCode = a}
    MsgNext -> next model
    MsgReset ->
        let newModel = resetRunner model
        in {newModel| state = Edit}
    MsgAuto -> {model| autoRun = model.state == Run && not model.autoRun}

subscriptions : Model -> Sub Msg
subscriptions {autoRun} =
    if autoRun then
        every (30 * millisecond) (always MsgNext)
    else
        Sub.none
