module Kalevala where

import Color
import Dict
import Graphics.Element exposing (Element)
import Graphics.Input.Field as Field
import Json.Decode
import Json.Encode
import Mouse
import Random
import Signal exposing (..)
import Time exposing (Time)
import Window

import Deprecated.WebSocket as WebSocket

import Helpers exposing (..)
import GameTypes exposing (..)
import Log
import State exposing (isGameOver, mustPass)
import Display exposing (..)
import Display.Board exposing (mouseToBoardPosition)
import Game
import Player
import Serialize
import Deserialize

import Debug

isRemoteSignal : Signal Bool
isRemoteSignal =
  let getIsRemoteFromClickSignal event =
    case event of
    StartSinglePlayer -> Just False
    StartTwoPlayerHotseat -> Just False
    StartTwoPlayerOnline -> Just True
    otherwise -> Nothing
  in
    filterMap getIsRemoteFromClickSignal False clickMailbox.signal

{- Perform an action on the current state and return the resulting state. -}
performAction : Action -> State -> State
performAction action state =
  let newState =
        case action of
          PickUpPiece player idx -> Game.tryToPickUpPiece player idx state
          PlacePiece mousePos dims -> Game.tryMove (mouseToBoardPosition mousePos state dims) state
          StartGame gameType deck player playerName -> Game.startGame gameType deck player playerName
          StartNewGame deck player playerName -> Game.startGame state.gameType deck player playerName
          GameStarted deck startPlayer localPlayer opponentName -> Game.gameStarted deck startPlayer localPlayer opponentName state
          Pass -> Game.pass state
          Switch -> { state | turn <- Player.next state.turn }
          MoveToMainMenu -> Game.startState
          MoveToRemoteGameMenu -> { state | gameType <- HumanVsHumanRemote, gameState <- NotStarted }
          OpponentDisconnected -> { state | gameState <- Disconnected
                                          , log <- Log.addSystemMsg "Opponent disconnected." state.log }
          CpuAction -> Game.tryAIMove state
          NoAction -> state
          ParseError e -> state
  in
    if | isGameOver newState -> { newState | gameState <- GameOver
                                           , log <- Log.addSystemMsg ("Game over!" ++ State.endStateMsg newState) newState.log }
       | otherwise -> newState

{- Turn a ClickEvent into an Action -}
constructAction : ClickEvent -> Random.Seed -> MousePos -> WindowDims -> Field.Content -> Action
constructAction clickType seed mousePos dims playerName =
  let
    pos = Debug.watch "Mouse.position" mousePos
    click = Debug.watch "clickInput.signal" clickType
    deck = shuffle Game.deckContents seed
  in
    case clickType of
      StartSinglePlayer -> StartGame HumanVsCpu deck (Player.random seed) playerName.string
      StartTwoPlayerOnline -> StartGame HumanVsHumanRemote deck (Player.random seed) playerName.string
      StartTwoPlayerHotseat -> StartGame HumanVsHumanLocal deck (Player.random seed) playerName.string
      StartNewGameButton -> StartNewGame deck (Player.random seed) playerName.string
      BoardClick -> PlacePiece mousePos dims
      PieceInHand player idx -> PickUpPiece player idx
      PassButton -> Pass
      SwitchButton -> Switch
      MainMenuButton -> MoveToMainMenu
      StartRemoteGameButton -> MoveToRemoteGameMenu
      None -> NoAction

{- Turn a signal of ClickEvents into a signal of Actions -}
processClick : Signal ClickEvent -> Signal Action
processClick signal =
  let seedSignal = (Random.initialSeed << round << fst) <~ Time.timestamp signal
      sampledMouse = sampleOn signal Mouse.position
      sampledPlayerName = sampleOn signal <| playerNameSignal
  in
    constructAction <~ signal ~ seedSignal ~ sampledMouse ~ Window.dimensions ~ sampledPlayerName

{- Path to a Voluspa game server
   (see https://github.com/neunenak/voluspa-server) -}
server : String
server = "ws://209.38.174.55:22000"

{- Encodes an Action as a JSON string -}
encode : Action -> String
encode action = Json.Encode.encode 0 (Serialize.action action)

{- Decodes an Action from a JSON string -}
decode : String -> Action
decode actionJson =
  case Json.Decode.decodeString Deserialize.action actionJson of
    Ok action -> action
    Err e -> ParseError e

getRemoteResponse : Signal Action -> Signal Action
getRemoteResponse playerAction =
  let actionForRemote = filterOn playerAction isRemoteSignal NoAction
      request = Debug.watch "request" <~ (encode <~ actionForRemote)
      response = Debug.watch "response" <~ WebSocket.connect server request
  in
    Debug.watch "deserialized" <~ (decode <~ response)

getCpuResponse : Signal Action -> Time -> Signal Action
getCpuResponse playerAction delayTime =
  let processCpuResponse a =
    case a of
      PlacePiece mp wd -> CpuAction
      Pass -> CpuAction
      otherwise -> NoAction
  in
    Time.delay delayTime (processCpuResponse <~ playerAction)

{- The main game loop.
   The state is folded over time over a stream of Actions, coming from the client and from the server (for an online game.)
   Actions from the client are constructed by processing a signal of ClickEvents from clickMailbox.
   In the case the the game type is HumanVsHumanRemote, Actions are also sent to the server over a websocket,
   and the Actions that are received from the server are merged into the signal of client Actions.
   For each Action in this resulting signal, (performAction action state) returns the new state. -}
main : Signal Element
main =
  let
    playerAction = processClick clickMailbox.signal
    remoteAction = getRemoteResponse playerAction
    cpuAction = getCpuResponse playerAction Time.second

    state = foldp performAction Game.startState (mergeMany [playerAction, remoteAction, cpuAction])
  in
    render <~ state ~ Window.dimensions ~ playerNameSignal