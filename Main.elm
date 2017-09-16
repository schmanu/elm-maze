import Html exposing (..)
import Html.App as App
import MazeGenerator
import View.MazeRenderer

main : Program Never
main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

model : MazeGenerator.Model
model =
    MazeGenerator.generateMaze

-- UPDATE

update : View.MazeRenderer.Msg -> MazeGenerator.Model -> MazeGenerator.Model
update msg model =
  model


-- VIEW

view : MazeGenerator.Model -> Html View.MazeRenderer.Msg
view model =
  View.MazeRenderer.render model.maze
