module Main exposing (..)


import Browser
import Html
import Html.Attributes as HA


import List exposing (map)
import String exposing (fromInt, fromFloat)


import Html.Events.Extra.Mouse as Mouse
import Svg exposing (Svg)
import Svg.Attributes as SA
import Matrix exposing (Matrix)


import Tiles exposing (Tile(..), Loc(..), Quad(..), Selection(..), td)




type alias Model =
    { pos : (Float, Float)
    , mt : Matrix Tile
    , dragLoc : Loc
    , dragState : Bool
    }

init : Model
init = 
    { pos = (0,0)
    , mt = Matrix.repeat 1 1 T0 -- width and height
    , dragLoc = LOut
    , dragState = False
    }



type Msg = MoveMsg (Float, Float) | UpMsg (Float, Float) | DownMsg (Float, Float)

update : Msg -> Model -> Model
update msg model =
  case msg of 
    MoveMsg (x, y) -> handleMove (x, y) model
    DownMsg (x, y) -> handleDown model 
    UpMsg (x, y)   -> handleUp   model


handleMove : (Float, Float) -> Model -> Model
handleMove (x, y) ({pos, mt, dragLoc, dragState} as model) =
  {model | pos = (x, y)}


handleDown : Model -> Model
handleDown ({pos, mt, dragLoc, dragState} as model) =
  let
    loc = Tiles.posToLoc mt pos
    wm = Matrix.width mt
    hm = Matrix.height mt
  in
    case loc of
      LTile t   -> {model | dragState = True, dragLoc = loc}
      LAR       -> {model | mt = Tiles.addRow mt, dragState = False}
      LAC       -> {model | mt = Tiles.addCol mt, dragState = False}
      LRR       -> {model | mt = Tiles.remRow mt, dragState = False}
      LRC       -> {model | mt = Tiles.remCol mt, dragState = False}
      LMat i j q t -> {model | mt = Tiles.selectAndApply loc mt, dragState = False}
      _         -> model


handleUp : Model -> Model
handleUp ({pos, mt, dragLoc, dragState} as model) =
  let
    loc = Tiles.posToLoc mt pos
    
    nt = case dragLoc of
             LTile t -> t
             _ -> T0
  in
    case loc of
      LMat i j q t -> if dragState then
                          {model | mt = Matrix.set j i nt mt, dragState = False }
                      else
                          {model | dragState = False}
      _ -> { model | dragState = False}






renderView : Model -> Svg msg
renderView ({pos, mt, dragLoc, dragState} as model) =
  let
    loc = Tiles.posToLoc mt pos

    wm = Matrix.width mt
    hm = Matrix.height mt

    -- tileset
    ts = Tiles.matTilesToSvg Tiles.tSet
    
    -- tileset hover
    tshov = case loc of
                LTile t -> [Tiles.overlayRect (td * Tiles.tileToInt t) 0 td td]
                _ -> []

    ms = Tiles.translate td (2*td) <| Tiles.matTilesToSvg mt

    -- drag tile
    (x, y) = pos
    dt = case dragState of
            False -> []
            True -> case dragLoc of
                        LTile t -> [Tiles.translate (round x) (round y) (Tiles.tileToSvgE t)]
                        _ -> []

    -- hover
    ht = case dragState of
            True  -> case loc of
                        LTile t -> [Tiles.overlayRect (td * Tiles.tileToInt t) 0 td td]
                        LMat i j q t ->  [Tiles.overlayRect ((j + 1) * td) ((i + 2) * td) td td]
                        _ -> []
            False -> case (Tiles.selection mt loc) of
                       Inn i j -> [Tiles.overlayRect ((j + 1) * td) ((i + 2) * td) (2 * td) (2 * td)]
                       BU j    -> [Tiles.overlayRect ((j + 1) * td) (2 * td) (2 * td) td]
                       BD j    -> [Tiles.overlayRect ((j + 1) * td) ((hm + 1) * td) (2 * td) td]
                       MS i j  -> [Tiles.overlayRect ((j + 1) * td) ((i + 2 - 1) * td) (2 * td) (3 * td)]
                       Mor i j -> [Tiles.overlayRect ((j + 1) * td) ((i + 2) * td) ((Tiles.spanOfT9 j i mt) * td) (1 * td)]
                       _       -> []

    adC = [Tiles.translate ((wm + 2) * td) (2 * td) Tiles.tAddC]
    adR = [Tiles.translate td ((hm + 3) * td) Tiles.tAddR]

    rmC = [Tiles.translate ((wm + 2) * td) (3 * td) Tiles.tRmC]
    rmR = [Tiles.translate (2 * td) ((hm + 3) * td) Tiles.tRmR]
    
    wt = (Matrix.width Tiles.tSet) -- tileSet width
    wtot = fromInt <| max (wt * td) ((wm + 3) * td)
    htot = fromInt <| ((hm + 4) * td)
  in
    Svg.svg [SA.width wtot, SA.height htot]
        ([ts, ms] ++ dt ++ adC ++ adR ++ rmC ++ rmR ++ ht ++ tshov)
 



view : Model -> Html.Html Msg
view ({pos, mt, dragLoc, dragState} as model) = 
    let
        (x, y) = pos
        
        mtsvg = renderView model

    in
        Html.div [ HA.style "margin" "auto"
                 , HA.style "width" "50%"
                 , HA.style "height" "100%"
                 , HA.style "padding-top" "50px"
                 , HA.style "padding-bottom" "50px"
                 , HA.style "padding-left" "50px"
                 , HA.style "padding-right" "50px"
                 , HA.style "background-color" "#659dbd"
                 ]
                 [ Html.div 
                   [ Mouse.onMove (\event -> MoveMsg event.offsetPos)
                   , Mouse.onUp (\event -> UpMsg event.offsetPos)
                   , Mouse.onDown (\event -> DownMsg event.offsetPos)
                   ] 
                   [ mtsvg ]
                 
                 , Html.div
                   [] 
                   [ 
                   --  Html.br [] []
                   --, Html.text <| fromFloat x
                   --, Html.br [] []
                   --, Html.text <| fromFloat y
                   --, Html.br [] []
                     Html.text <| Tiles.locToStr <| Tiles.posToLoc mt pos
                   , Html.br [] []
                   , Html.text <| ("isWellConnected: " ++ (if Tiles.isWellConnected mt then "True" else "False")) 
                   ]
                 ]

main : Program () Model Msg
main = Browser.sandbox { init = init
                       , update = update
                       , view = view
                       }