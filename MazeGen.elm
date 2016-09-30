module MazeGen exposing (..)

import Array exposing (Array)
import Random
import List
import Bitwise

type CellEnds = Dead | Open Int

mergeEnds : CellEnds -> CellEnds -> CellEnds
mergeEnds ends1 ends2 =
  case ends1 of
    Dead -> ends2
    Open dir ->
      case ends2 of
        Dead -> ends1
        Open dir2 ->
          Open (Bitwise.or dir dir2)

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

dy : Int -> Int
dy dir =
  case dir of
    1 -> 1 -- north
    2 -> -1 -- south
    _ -> 0

oppositeDir : Int -> Int
oppositeDir dir =
  case dir of
    1 -> 2
    2 -> 1
    4 -> 8
    8 -> 4
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
  ,  visited : List (Int, Int)
  }


init : Model
init =
  {  maze = emptyMaze
  ,  seed = Random.initialSeed 31415
  ,  visited = []
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

openCell : Int -> Int -> CellEnds ->  Model -> Model
openCell x y ends model =
  let
    maybeCell = Array.get (toArrayIdx x y) model.maze
    maybeCell' =
      case maybeCell of
        Nothing -> Nothing
        Just cell ->
          Just {cell
                  | mark = True
                  , ends = mergeEnds ends cell.ends
               }
  in
    case maybeCell' of
      Nothing -> model
      Just cell ->
        {  maze = Array.set (toArrayIdx x y) cell model.maze
        ,  seed = model.seed
        ,  visited = (x, y) :: model.visited
        }

openStartPosition : Model -> Model
openStartPosition model =
  let
    (rndX, seed1) = Random.step (Random.int 0 19) model.seed
    (rndY, newSeed) = Random.step (Random.int 0 19) seed1
    openModel = openCell rndX rndY Dead model
  in
    {  maze = openModel.maze
    ,  seed = newSeed
    ,  visited = openModel.visited
    }

rndDirection : Cell -> Random.Seed -> (Int, Random.Seed)
rndDirection cell seed =
  let
    (rndDir, seed1) = Random.step (Random.int 0 3) seed
    dir = 2 ^ rndDir
  in
    case cell.ends of
      Dead -> (dir, seed1)
      Open state ->
        if Bitwise.and state dir > 0 then -- Weg ist bereits frei
          rndDirection cell seed1 -- Nochmal versuchen
        else
          (dir, seed1)

carveNextCell : Model -> Model
carveNextCell model =
  let
    (x,y) =
      case model.visited of
        [] -> (-1, -1)
        (vX,vY) :: list -> (vX,vY)
    maybeCell = Array.get (toArrayIdx x y) model.maze

    (dir, newSeed) =
      case maybeCell of
        Nothing -> (0, Random.initialSeed 1)
        Just cell -> rndDirection cell model.seed

    updatedCell =
      case maybeCell of
        Nothing -> Nothing
        Just cell -> Just {cell | ends = (mergeEnds cell.ends (Open dir))}

    -- Coordinates of Cell where Path leads to.
    otherX = x + (dx dir)
    otherY = y + (dy dir)

    openModel = openCell otherX otherY (Open (oppositeDir dir)) model


  in
    case updatedCell of
      Nothing -> model
      Just cell ->
        if otherX > 19 || otherX < 0 || otherY > 19 || otherY < 0 then
          { maze = model.maze
          , seed = newSeed
          , visited = model.visited
          }
        else
          {  maze = Array.set (toArrayIdx x y) cell openModel.maze
          ,  seed = newSeed
          ,  visited = openModel.visited
          }

