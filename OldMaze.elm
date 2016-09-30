module OldMaze exposing (Model)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard exposing (KeyCode)
import Keyboard
import Html.App as App
import Html exposing (Html)
import Debug exposing (log)


main =
  App.program
    { init = init
    , view = render
    , update = update
    , subscriptions = subscriptions
    }



-- MODELS
type alias Model =
  { player : Player
  , maze : Maze
  , grid : Grid
  }

type alias Maze =
  {
    walls : List (List Point)
  }

type alias Point =
  {  x : Int
  ,  y : Int
  }

type alias Grid =
  {  width : Int
  ,  height : Int
  }

type alias Player =
  {  x : Int
  ,  y : Int
  }

defaultGrid : Grid
defaultGrid =
  {  width = 20
  ,  height = 20
  }




maze1 : Maze
maze1 =
  { walls =
      [ [{x=1,y=1}, {x=10,y=1}, {x=10,y=12}]
      , [{x=5,y=10}, {x=15,y=10}]
      ]

  }

-- Grid Maze Helpers
toSvgPoints : List Point -> String
toSvgPoints list =
    case list of
      [] -> ""
      head :: rest ->
        (toString (head.x * 20)) ++ "," ++ (toString (head.y * 20)) ++ " " ++ (toSvgPoints rest)


init : (Model, Cmd Msg)
init =
  ( { player =
        {x = 0 -- Player Starting Position
        , y = 0
        }
    , maze = maze1
    , grid = defaultGrid
    }
  , (Cmd.none))



-- UPDATES

type Msg =
  KeyEvent KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyEvent k ->
      case k of
        40 -> (movedown model, Cmd.none) -- MoveDown
        38 -> (moveup model, Cmd.none)   -- MoveUp
        37 -> (moveleft model, Cmd.none) -- MoveLeft
        39 -> (moveright model, Cmd.none)-- MoveRight
        _ -> (model, Cmd.none)           -- DoNothing


-- BALL UPDATE ACTIONS

type Direction = X | Y

movedown : Model -> Model
movedown model =
  {model |player = updatePlayer model.player Y 1}

moveup : Model -> Model
moveup model =
  {model |player = updatePlayer model.player Y -1}


moveleft : Model -> Model
moveleft model =
  {model |player = updatePlayer model.player X -1}

moveright : Model -> Model
moveright model =
  {model |player = updatePlayer model.player X 1}

updatePlayer : Player -> Direction -> Int -> Player
updatePlayer player dir diff =
  case dir of
    X -> {player | x = player.x + diff}
    Y -> {player | y = player.y + diff}

-- SUBSCRIBE
subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.downs KeyEvent


-- VIEW

render : Model -> Html Msg
render model =
  svg
    [ width "400", height "400", viewBox "0 0 400 400" ]
    ([
      circle [ cx (toString ((model.player.x * 20)+10)), cy (toString ((model.player.y * 20)+10)), r "10" ] [],
      rect [x "0", y "0", width "400", height "400", fillOpacity "0", stroke "black"] []

    ] ++ (buildPolylines model.maze.walls))



buildPolylines : List (List Point) -> List (Svg a)
buildPolylines list =
  case list of
    [] -> []
    head :: rest ->
      [polyline [points (toSvgPoints head), fill "none", stroke "black"][]] ++ (buildPolylines rest)
