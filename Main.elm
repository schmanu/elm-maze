import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
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
       if model.tick < 350 then
        ( {model | 
            mazeModel = MazeGenerator.carveNextCell model.mazeModel
          , tick = model.tick + 1
          }
        , Cmd.none
        )
      else
        (model, Cmd.none)
    View.MazeRenderer.Reset ->
      (reset model, Cmd.none)

reset : Model -> Model
reset model =
  let
    newMaze = (MazeGenerator.init)
    seededMaze = {newMaze | seed = model.mazeModel.seed}
    
  in
      
  {model | mazeModel = (MazeGenerator.openStartPosition seededMaze)
  , tick = 0
  } 


-- SUBSCRIPTIONS
subscriptions : Model -> Sub View.MazeRenderer.Msg
subscriptions model =
  Time.every (50 * millisecond) View.MazeRenderer.Tick


-- VIEW
view : Model -> Html View.MazeRenderer.Msg
view model =
  div [style [("width", "500px")]] [h2 [] [ a [href "https://github.com/schmanu/elm-maze"] [ text("Elm-Maze")]
      , text (" - Maze Generator written in Elm")]
    , h5 [style [("text-align", "right")]] [text("by schmanu")]
    ,(View.MazeRenderer.render model.mazeModel.maze)
    , span [class "tickCounter"] [text ("Tick: " ++ (toString model.tick))]
    , button [class "newMazeBtn", onClick View.MazeRenderer.Reset ][text("New Maze")]]