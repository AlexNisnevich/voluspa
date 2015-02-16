module Deserialize where

import Dict
import Json.Decode (..)

import Helpers (..)
import GameTypes (..)
import Player

action : Decoder Action
action =
  ("action" := string) `andThen` actionInfo

actionInfo : String -> Decoder Action
actionInfo actionType =
  case actionType of
    "PickUpPiece" ->
      object2 PickUpPiece
        ("player" := player)
        ("idx" := int)
    "PlacePiece" ->
      object2 PlacePiece
        ("mousePos" := mousePos)
        ("dims" := windowDims)
    "StartGame" ->
      object3 StartGame
        (succeed HumanVsHumanRemote)
        ("deck" := deck)
        ("player" := player)
    "GameStarted" ->
      object2 GameStarted
        ("deck" := deck)
        ("player" := player)
    "Pass" ->
      null Pass
    "NoAction" ->
      null NoAction
    _ ->
      fail (actionType ++ " is not a recognized type of action")

deck : Decoder Deck
deck = list string

player : Decoder Player
player = map Player.fromString string

mousePos : Decoder MousePos
mousePos = tuple2 (,) int int

windowDims : Decoder WindowDims
windowDims = tuple2 (,) int int