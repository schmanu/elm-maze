module MazeGenerator exposing (..)

import Random exposing (Generator)
import List
import Bitwise
import Model.Direction
import Model.Maze
import Debug

type alias Model =
  {  maze : Model.Maze.Maze
  ,  seed : Random.Seed
  ,  visited : List (Int, Int)
  }

generateMaze : Model
generateMaze =
  carveMaze (openStartPosition init)

carveMaze : Model -> Model
carveMaze model =
  if List.length model.visited < 40 then
    carveMaze (carveNextCell model)
  else
    model


init : Model
init =
  {  maze = Model.Maze.emptyMaze
  ,  seed = Random.initialSeed 31415
  ,  visited = []
  }


openCell : Int -> Int -> Model.Maze.CellEnds ->  Model -> Model
openCell x y ends model =
  let
    maybeCell = Model.Maze.getCell x y model.maze
    maybeCell' =
      case maybeCell of
        Nothing -> Nothing
        Just cell ->
          Just {cell
                  | mark = True
                  , ends = Model.Maze.mergeEnds ends cell.ends
               }
  in
    case maybeCell' of
      Nothing -> model
      Just cell ->
        {  maze = Model.Maze.setCell x y cell model.maze
        ,  seed = model.seed
        ,  visited = (x, y) :: model.visited
        }

openStartPosition : Model -> Model
openStartPosition model =
  let
    (rndX, seed1) = Random.step (Random.int 0 19) model.seed
    (rndY, newSeed) = Random.step (Random.int 0 19) seed1
    openModel = openCell rndX rndY Model.Maze.Dead model
  in
    {  maze = openModel.maze
    ,  seed = newSeed
    ,  visited = openModel.visited
    }

rndDirection : Model.Maze.Cell -> Random.Seed -> (Model.Direction.Direction, Random.Seed)
rndDirection cell seed =
  let
    (rndDir, seed1) = Random.step (Model.Direction.dirGenerator) seed
    dirCode = Model.Direction.toInt(rndDir)
  in
    case cell.ends of
      Model.Maze.Dead -> (rndDir, seed1)
      Model.Maze.Open state ->
        if Bitwise.and state dirCode > 0 then -- Weg ist bereits frei
          rndDirection cell seed1 -- Nochmal versuchen
        else
          (rndDir, seed1)

selectValidStartingCell : List (Int, Int) -> Model -> Maybe Model.Maze.Cell
selectValidStartingCell listOfVisited model =
  let 
    (x, y, list) =
      case listOfVisited of
        [] -> (-1, -1, [])
        (vX,vY) :: list -> (vX,vY, list)
    maybeCell = Model.Maze.getCell x y model.maze
  in 
    case maybeCell of
      Nothing -> Nothing
      Just cell ->
        case cell.ends of 
          Model.Maze.Dead -> Just cell
          Model.Maze.Open 15 -> selectValidStartingCell list model
          Model.Maze.Open _ -> Just cell

carveNextCell : Model -> Model
carveNextCell model =
  let
    maybeCell = selectValidStartingCell model.visited model
    (dir, newSeed) =
      case maybeCell of
        Nothing -> (Model.Direction.None, model.seed)
        Just cell -> rndDirection cell model.seed

    updatedCell =
      case maybeCell of
        Nothing -> Nothing
        Just cell -> Just {cell | ends = (Model.Maze.mergeEnds cell.ends (Model.Maze.Open ((Debug.log "Direction Carved: " (Model.Direction.toInt dir)))))}

    (x, y) = case maybeCell of
      Nothing -> (-1, -1)
      Just cell -> (cell.x, cell.y)

    -- Coordinates of Cell where Path leads to.
    otherX = x + (Model.Direction.dx dir)
    otherY = y + (Model.Direction.dy dir)

    openModel = openCell otherX otherY (Model.Maze.Open (Model.Direction.toInt(Model.Direction.oppositeDirection dir))) model


  in
    case updatedCell of
      Nothing -> model
      Just cell ->
        if otherX > 19 || otherX < 0 || otherY > 19 || otherY < 0 then
          { maze = model.maze
          , seed = newSeed
          , visited = List.append (List.drop 1 model.visited)  [(x, y)]
          }
        else
          {  maze = Model.Maze.setCell x y cell openModel.maze
          ,  seed = newSeed
          ,  visited = openModel.visited
          }

