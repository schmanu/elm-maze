module Direction exposing (..)
import Random exposing (Generator)

type Direction = North | East | South | West | None

fromInt : Int ->  Direction
fromInt code =
    case code of
        1 -> North
        2 -> East
        4 -> South
        8 -> West
        _ -> None

toInt : Direction -> Int
toInt dir =
    case dir of
        North -> 1
        East -> 2
        South -> 4
        West -> 8
        None -> 0


dx : Direction -> Int
dx dir =
  case dir of
    East -> 1 -- east
    West -> -1 -- west
    _ -> 0

dy : Direction -> Int
dy dir =
  case dir of
    North -> 1 -- north
    South -> -1 -- south
    _ -> 0

oppositeDirection : Direction -> Direction
oppositeDirection dir =
  case dir of
    North -> South
    East -> West
    South -> North
    West -> East
    None -> None

dirGenerator : Generator Direction
dirGenerator =
    Random.map(\n -> fromInt(2 ^ n)) (Random.int 0 3)