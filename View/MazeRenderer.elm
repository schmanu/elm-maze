module View.MazeRenderer exposing (..)
import Model.Maze exposing (Maze, Cell, CellEnds)
import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (Array)
import Time exposing (Time)

render : Maze -> Html Msg
render maze = 
  div [] 
  [
  table []
  (renderRows maze)
  ]


type Msg = Tick Time

renderRows : Maze -> List (Html Msg)
renderRows maze =
  if Array.length maze == 20 then
    renderRow (Array.toList maze)
  else 
    renderRow (Array.toList (Array.slice 0 20 maze)) ++ renderRows (Array.slice 20 (Array.length maze) maze) 


renderRow : List Cell -> List (Html Msg)
renderRow mazeRow =
  [ tr []
     (renderCells mazeRow)
  ]

renderCells : List Cell -> List (Html Msg)
renderCells mazeRow =
  case mazeRow of
    [] -> []
    head::tail ->
      [td [style (List.append (setBordersForCell head) [("width", "15px"), ("height", "15px")]) ][]
      ] ++ renderCells tail

setBordersForCell : Cell -> List (String, String)
setBordersForCell cell =
  case cell.ends of
    Model.Maze.Dead -> [("border-style", "solid"), ("background", "black")]
    Model.Maze.Open 1 -> [("border-left-style", "solid"), ("border-top-style", "solid"), ("border-right-style", "solid")]
    Model.Maze.Open 2 -> [("border-left-style", "solid"), ("border-top-style", "solid"), ("border-bottom-style", "solid")]
    Model.Maze.Open 3 -> [("border-left-style", "solid"), ("border-top-style", "solid")]
    Model.Maze.Open 4 -> [("border-left-style", "solid"), ("border-bottom-style", "solid"), ("border-right-style", "solid")]
    Model.Maze.Open 5 -> [("border-left-style", "solid"), ("border-right-style", "solid")]
    Model.Maze.Open 6 -> [("border-left-style", "solid"), ("border-bottom-style", "solid")]
    Model.Maze.Open 7 -> [("border-left-style", "solid")]
    Model.Maze.Open 8 -> [("border-top-style", "solid"), ("border-bottom-style", "solid"), ("border-right-style", "solid")]
    Model.Maze.Open 9 -> [("border-top-style", "solid"), ("border-right-style", "solid")]
    Model.Maze.Open 10 -> [("border-top-style", "solid"), ("border-bottom-style", "solid")]
    Model.Maze.Open 11 -> [("border-top-style", "solid")]
    Model.Maze.Open 12 -> [("border-bottom-style", "solid"), ("border-right-style", "solid")]
    Model.Maze.Open 13 -> [("border-right-style", "solid")]
    Model.Maze.Open 14 -> [("border-bottom-style", "solid")]
    _ -> [("border-style", "none")]

