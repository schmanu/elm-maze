module MazeGen exposing (..)

import Array exposing (Array)
import Random
import Set exposing (Set)

type CellEnds = Dead | Open Int

north : Int
north = 1
south : Int
south = 2
east : Int
east = 4
west : Int
west = 8

dx : Int -> Int
dx dir =
  case dir of
    4 -> 1 -- east
    8 -> -1 -- west
    _ -> 0


type alias Cell =
  {  ends : CellEnds
  ,  x : Int
  ,  y : Int
  ,  mark: Bool
  }

type alias Model =
  {  maze : Maze
  ,  seed : Random.Seed
  }


init : Model
init =
  {  maze = emptyMaze
  ,  seed = Random.initialSeed 31415
  }

toArrayIdx : Int -> Int -> Int
toArrayIdx x y =
  (y * 20) + x

createEmptyCell : Int -> Cell
createEmptyCell arrIdx =
  { ends = Dead
  , x = arrIdx % 20
  , y = arrIdx // 20
  , mark = False
  }

-- Our Maze is simply a one dimensional Array, which represents a two dimensional matrix.
type alias Maze = Array Cell


emptyMaze : Maze
emptyMaze =
  Array.initialize 400 createEmptyCell

openStartPosition : Model -> Model
openStartPosition model =
  let
    (rndX, seed1) = Random.step (Random.int 0 19) model.seed
    (rndY, newSeed) = Random.step (Random.int 0 19) seed1
    maybeCell = Array.get (toArrayIdx rndX rndY) model.maze
    maybeCell' =
      case maybeCell of
        Nothing -> Nothing
        Just cell -> Just {cell | mark = True}
  in
    case maybeCell' of
      Nothing -> model
      Just cell ->
        {  maze = Array.set (toArrayIdx rndX rndY) cell model.maze
        ,  seed = newSeed
        }
