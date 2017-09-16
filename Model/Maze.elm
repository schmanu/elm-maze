module Model.Maze exposing (..)

import Array exposing (Array)
import Bitwise
import String
import Model.Direction as Direction exposing (Direction) 
-- Our Maze is simply a one dimensional Array, which represents a two dimensional matrix.
type alias Maze = Array Cell

-- Cells have 2-dimensional coordinates, can be marked and have CellEnds, describing their Open Walls.
type alias Cell =
  {  ends : CellEnds
  ,  x : Int
  ,  y : Int
  ,  mark: Bool
  }

-- CellEnds describes the four Walls of a Cell. The Cell can still be Dead and Closed or Opened in multiple Directions encoded 
-- as Direction Code (Int)
type CellEnds = Dead | Open Int

-- Generates 20x20 empty Maze
emptyMaze : Maze
emptyMaze =
  Array.initialize 400 createEmptyCell

-- Creates a Cell which is still Dead. The Coordinates are derived from a single integer describing the x,y Position of the Cell.
createEmptyCell : Int -> Cell
createEmptyCell arrIdx =
  { ends = Dead
  , x = arrIdx % 20
  , y = arrIdx // 20
  , mark = False
  }

-- Merges newly Carved Openings with the Current state of the Cell using Bitwise or.
mergeEnds : CellEnds -> CellEnds -> CellEnds
mergeEnds ends1 ends2 =
  case ends1 of
    Dead -> ends2
    Open dir ->
      case ends2 of
        Dead -> ends1
        Open dir2 ->
          Open (Bitwise.or dir dir2)

toMazeArrayIdx : Int -> Int -> Int
toMazeArrayIdx x y =
  (y * 20) + x

getCell : (Int, Int) -> Maze -> Maybe Cell
getCell (x, y) maze =
    if x > 19 || y > 19 || x < 0 || y < 0 then
      Nothing
    else
      Array.get (toMazeArrayIdx x y) maze

setCell : (Int, Int) -> Cell -> Maze -> Maze
setCell (x, y) cell maze =
    Array.set (toMazeArrayIdx x y) cell maze

getNeighbor : Cell -> Direction -> Maze -> Maybe Cell
getNeighbor cell direction maze =
  getCell (cell.x + Direction.dx direction, cell.y + Direction.dy direction) maze

getAllNeighbors : Cell -> Maze -> List (Maybe Cell)
getAllNeighbors cell maze =
  getCell (cell.x - 1, cell.y) maze :: getCell (cell.x + 1, cell.y) maze :: getCell (cell.x, cell.y - 1) maze :: getCell (cell.x, cell.y + 1) maze :: []



cellToString : Cell -> String
cellToString cell =
    case cell.ends of
        Dead -> "X"
        Open _ -> " "
mazeToString : Maze -> String
mazeToString maze =
    String.concat (Array.toList (Array.map cellToString maze))


