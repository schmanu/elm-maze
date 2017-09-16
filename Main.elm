import Html exposing (..)
import Html.App as App
import MazeGenerator
import View.MazeRenderer
import Time exposing (Time, millisecond)

type alias Model =
  {  mazeModel : MazeGenerator.Model
  ,  tick : Int
  }

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd View.MazeRenderer.Msg)
init =
  ( { mazeModel = MazeGenerator.openStartPosition MazeGenerator.init
    , tick = 0
    }
  , Cmd.none
  )

-- UPDATE
update : View.MazeRenderer.Msg -> Model -> (Model, Cmd View.MazeRenderer.Msg)
update msg model =
  case msg of
    View.MazeRenderer.Tick newTime ->
       if model.tick < 300 then
        ( {model | 
            mazeModel = MazeGenerator.carveNextCell model.mazeModel
          , tick = model.tick + 1
          }
        , Cmd.none
        )
      else
        (model, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub View.MazeRenderer.Msg
subscriptions model =
  Time.every (50 * millisecond) View.MazeRenderer.Tick


-- VIEW

view : Model -> Html View.MazeRenderer.Msg
view model =
  div [] 
    [(View.MazeRenderer.render model.mazeModel.maze)
    , text ("Tick: " ++ (toString model.tick))]

