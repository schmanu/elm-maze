module View.MazeRenderer exposing (..)
import Model.Maze exposing (Maze, Cell, CellEnds)
import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (Array)

render : Maze -> Html Msg
render maze = 
  div [] 
  [
  table []
  (renderRows maze)
  ]


type Msg = Empty

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
      [td (setBordersForCell head)
        [
        text (Model.Maze.cellToString head)
        ]] ++ renderCells tail

setBordersForCell : Cell -> List (Html.Attribute Msg)
setBordersForCell cell =
  case cell.ends of
    Model.Maze.Dead -> [style [("border-style", "solid")]]
    Model.Maze.Open 1 -> [style [("border-left-style", "solid"), ("border-top-style", "solid"), ("border-right-style", "solid")]]
    Model.Maze.Open 2 -> [style [("border-left-style", "solid"), ("border-top-style", "solid"), ("border-bottom-style", "solid")]]
    Model.Maze.Open 3 -> [style [("border-left-style", "solid"), ("border-top-style", "solid")]]
    Model.Maze.Open 4 -> [style [("border-left-style", "solid"), ("border-bottom-style", "solid"), ("border-right-style", "solid")]]
    Model.Maze.Open 5 -> [style [("border-left-style", "solid"), ("border-right-style", "solid")]]
    Model.Maze.Open 6 -> [style [("border-left-style", "solid"), ("border-bottom-style", "solid")]]
    Model.Maze.Open 7 -> [style [("border-left-style", "solid")]]
    Model.Maze.Open 8 -> [style [("border-top-style", "solid"), ("border-bottom-style", "solid"), ("border-right-style", "solid")]]
    Model.Maze.Open 9 -> [style [("border-top-style", "solid"), ("border-right-style", "solid")]]
    Model.Maze.Open 10 -> [style [("border-top-style", "solid"), ("border-bottom-style", "solid")]]
    Model.Maze.Open 11 -> [style [("border-top-style", "solid")]]
    Model.Maze.Open 12 -> [style [("border-bottom-style", "solid"), ("border-right-style", "solid")]]
    Model.Maze.Open 13 -> [style [("border-right-style", "solid")]]
    Model.Maze.Open 14 -> [style [("border-bottom-style", "solid")]]
    _ -> [style [("border-style", "none")]]

