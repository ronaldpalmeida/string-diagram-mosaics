module Tiles exposing (..)

import String exposing (fromInt, fromFloat)
import Array

import Svg exposing (Svg, g, svg, rect, circle, line, polygon)
import Svg.Attributes as A

import Matrix exposing (Matrix)
import Neighbours

import Rules exposing (innR)

type Tile = T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10

tileToStr : Tile -> String
tileToStr t =
    case t of
      T0 -> "T0"
      T1 -> "T1"
      T2 -> "T2"
      T3 -> "T3"
      T4 -> "T4"
      T5 -> "T5"
      T6 -> "T6"
      T7 -> "T7"
      T8 -> "T8"
      T9 -> "T9"
      T10 -> "T10"

tileToInt : Tile -> Int
tileToInt t =
    case t of
      T0 -> 0
      T1 -> 1
      T2 -> 2
      T3 -> 3
      T4 -> 4
      T5 -> 5
      T6 -> 6
      T7 -> 7
      T8 -> 8
      T9 -> 9
      T10 -> 10

intToTile : Int -> Tile
intToTile n =
    case n of
      0 -> T0
      1 -> T1
      2 -> T2
      3 -> T3
      4 -> T4
      5 -> T5
      6 -> T6
      7 -> T7
      8 -> T8
      9 -> T9
      10 -> T10
      _ -> T0

tileToSvgE : Tile -> Svg msg
tileToSvgE t =
    case t of
      T0 -> t0
      T1 -> t1
      T2 -> t2
      T3 -> t3
      T4 -> t4
      T5 -> t5
      T6 -> t6
      T7 -> t7
      T8 -> t8
      T9 -> t9
      T10 -> t10

-- Empty, String, Morphism
type Conn = E | S | M

-- arities

au : Tile -> Conn
au t = 
    case t of
      T0 -> E
      T1 -> S
      T2 -> E
      T3 -> S
      T4 -> S
      T5 -> E
      T6 -> E
      T7 -> S
      T8 -> E
      T9 -> E
      T10 -> E

ad : Tile -> Conn
ad t = 
    case t of
      T0 -> E
      T1 -> S
      T2 -> E
      T3 -> E
      T4 -> E
      T5 -> S
      T6 -> S
      T7 -> E
      T8 -> S
      T9 -> E
      T10 -> E

al : Tile -> Conn
al t = 
    case t of
      T0 -> E
      T1 -> E
      T2 -> S
      T3 -> S
      T4 -> E
      T5 -> E
      T6 -> S
      T7 -> M
      T8 -> M
      T9 -> E
      T10 -> M

ar : Tile -> Conn
ar t = 
    case t of
      T0 -> E
      T1 -> E
      T2 -> S
      T3 -> E
      T4 -> S
      T5 -> S
      T6 -> E
      T7 -> M
      T8 -> M
      T9 -> M
      T10 -> E

-- reflections

hrefl : Tile -> Tile
hrefl t =
    case t of
      T0 -> T0
      T1 -> T1
      T2 -> T2
      T3 -> T6
      T4 -> T5
      T5 -> T4
      T6 -> T3
      T7 -> T8
      T8 -> T7
      T9 -> T9
      T10 -> T10

vrefl : Tile -> Tile
vrefl t =
    case t of
      T0 -> T0
      T1 -> T1
      T2 -> T2
      T3 -> T4
      T4 -> T3
      T5 -> T6
      T6 -> T5
      T7 -> T7
      T8 -> T8
      T9 -> T10
      T10 -> T9



-------------------------------------------------------------------
-- background: #659dbd
tCol = "#fbeec1" -- tile background color
tBordCol = "#659dbd"  -- tile border color
tStrCol = "#bc986a" -- string color 
tMorCol = "#8d8741" -- morphism color
tBordW = "2"      -- border width
tStrW = "2"       -- string width
td = 50
tdh = fromFloat <| (toFloat td) / 2 -- string half td

t0 : Svg msg
t0 = let 
          rt = rect [ A.width (fromInt td)
                    , A.height (fromInt td)
                    , A.fill tCol
                    , A.stroke tBordCol
                    , A.strokeWidth tBordW
                    ] []
        in
          g [] [rt]


t1 : Svg msg
t1 = let
          ln = line [ A.x1 tdh
                    , A.y1 "0"
                    , A.x2 tdh
                    , A.y2 <| fromInt td
                    , A.stroke tStrCol
                    , A.strokeWidth tStrW
                    ] []
        in
          g [] [t0, ln]


t2 : Svg msg
t2 =
  g [A.transform ("rotate(90,"++ tdh ++ "," ++ tdh ++ ")")] [t1]


t3 : Svg msg
t3 = let
          crc = circle [ A.cx "0"
                       , A.cy "0"
                       , A.r tdh
                       , A.stroke tStrCol
                       , A.strokeWidth tStrW
                       , A.fill "none"
                       ] []
        in
          g [] [svg [A.width (fromInt td), A.height (fromInt td)] [t0, crc]]


t4 : Svg msg
t4 =
  g [A.transform ("rotate(90,"++ tdh ++ "," ++ tdh ++ ")")] [t3]


t5 : Svg msg
t5 =
  g [A.transform ("rotate(180,"++ tdh ++ "," ++ tdh ++ ")")] [t3]


t6 : Svg msg
t6 =
  g [A.transform ("rotate(270,"++ tdh ++ "," ++ tdh ++ ")")] [t3]


t7 : Svg msg
t7 = let
          ln = line [ A.x1 tdh
                    , A.y1 "0"
                    , A.x2 tdh
                    , A.y2 tdh
                    , A.stroke tStrCol
                    , A.strokeWidth tStrW
                    ] []
          br = rect [ A.x "0"
                    , A.y <| fromFloat (0.4 * toFloat td)
                    , A.width <| fromInt td
                    , A.height <| fromFloat (0.2 * toFloat td)
                    , A.fill tMorCol
                    ][]
        in
          g [] [t0, ln, br]


t8 : Svg msg
t8 =
  g [A.transform ("rotate(180,"++ tdh ++ "," ++ tdh ++ ")")] [t7]


t9 : Svg msg
t9 = let
          br = rect [ A.x tdh
                    , A.y <| fromFloat (0.4 * toFloat td)
                    , A.width tdh
                    , A.height <| fromFloat (0.2 * toFloat td)
                    , A.fill tMorCol
                    ][]
        in
          g [] [t0, br]


t10 : Svg msg
t10 =
  g [A.transform ("rotate(180,"++ tdh ++ "," ++ tdh ++ ")")] [t9]



tAddR : Svg msg
tAddR = 
  let 
    rt = rect [ A.width (fromInt td)
              , A.height (fromInt td)
              , A.fill tCol
              , A.stroke tBordCol
              , A.strokeWidth tBordW
              ] []
    arr = polygon [ A.points ("0,0 " ++ tdh ++ "," ++ tdh ++ " " ++ (fromInt td) ++ "," ++ "0")
                  , A.fill tMorCol
                  ] []
  in
    g [] [rt, arr]


tAddC : Svg msg
tAddC = 
  g [A.transform ("rotate(270,"++ tdh ++ "," ++ tdh ++ ")")] [tAddR]

tRmC : Svg msg
tRmC = 
  g [A.transform ("rotate(180,"++ tdh ++ "," ++ tdh ++ ")")] [tAddC]

tRmR : Svg msg
tRmR = 
  g [A.transform ("rotate(180,"++ tdh ++ "," ++ tdh ++ ")")] [tAddR]




translate x y el = g [A.transform ("translate(" ++ (fromInt x) ++ "," ++ (fromInt y) ++ ")")] [el]

overlayRect : Int -> Int -> Int -> Int -> Svg msg
overlayRect x y w h = 
    rect [ A.width (fromInt w)
         , A.height (fromInt h)
         , A.x (fromInt x)
         , A.y (fromInt y)
         , A.fill "none"
         , A.stroke "white"
         , A.strokeWidth "2"
         ] []



----------------------------------------------------------------------------------
-- matrix stuff

tSet : Matrix Tile
tSet = 
    let
        gf i j = case i of
            0 -> T0
            1 -> T1
            2 -> T2
            3 -> T3
            4 -> T4
            5 -> T5
            6 -> T6
            7 -> T7
            8 -> T8
            9 -> T9
            10 -> T10
            _  -> T0
    in
        Matrix.generate 11 1 gf


matTilesToSvg : Matrix Tile -> Svg msg
matTilesToSvg mt =
    let
        ws = fromInt <| td * Matrix.width mt
        hs = fromInt <| td * Matrix.height mt
        rm = Matrix.indexedMap (\i j t -> translate (i * td) (j * td) (tileToSvgE t)) mt
        lm = Matrix.foldr (::) [] rm 
    in 
        svg [A.width ws, A.height hs] lm


type Quad = NE | NW | SE | SW


-- add row remove row add col remove col
type Loc = LTile Tile | LMat Int Int Quad Tile | LOut | LAR | LAC | LRR | LRC


quadToStr q = 
  case q of
    NE -> "NE"
    NW -> "NW"
    SE -> "SE"
    SW -> "SW"

-- rendered matrix, position -> location
posToLoc : Matrix Tile -> (Float, Float) -> Loc
posToLoc mt (xp, yp) = 
  let
    mj = Matrix.width mt
    mi = Matrix.height mt
    -- swapped
    ix = yp / (toFloat td)
    jx = xp / (toFloat td)
    
    i = floor ix
    j = floor jx
    
    iq = ix - toFloat i
    jq = jx - toFloat j
    
    q = if (iq <= 0.5) && (jq <= 0.5) then
          NW
        else if (iq <= 0.5) && (jq > 0.5) then
          NE
        else if (iq > 0.5) && (jq > 0.5) then
          SE
        else
          SW
  in
    if (i == 0) && (j < 11) then
        case (Matrix.get j 0 tSet) of
            Ok t -> LTile t
            Err _ -> LTile T0
    else if (i >= 2) && (i < mi+2) && (j >= 1) && (j < (mj+1)) then
        case (Matrix.get (j-1) (i-2) mt) of
            Ok t -> LMat (i-2) (j-1) q t
            Err _ -> LMat (i-2) (j-1) q T0
    else if (i == (mi + 3)) && (j == 1) then
      LAR
    else if (i == 2) && (j == (mj + 2)) then
      LAC
    else if (i == (mi + 3)) && (j == 2) then
      LRR
    else if (i == 3) && (j == (mj + 2)) then
      LRC
    else
        LOut


locToStr : Loc -> String
locToStr loc = 
  case loc of
    LOut         -> "Outside"
    LAC          -> "Add Column"
    LAR          -> "Add Row"
    LRC          -> "Remove Column"
    LRR          -> "Remove Row"
    LTile t      -> "TileSet: " ++ (tileToStr t)
    LMat i j q t -> "Matrix " 
                    ++ fromInt i
                    ++ ", "
                    ++ fromInt j
                    ++ "; " 
                    ++ quadToStr q
                    ++ ": "
                    ++ tileToStr t

------------------------------------------------------





-- matrix, x, y, width, height 
getSubMatrix : Matrix Tile -> Int -> Int -> Int -> Int -> Matrix Tile
getSubMatrix mt x y w h =
  let
    wm = Matrix.width mt
    hm = Matrix.height mt
    
    idxm = Matrix.generate w h (\xx yy -> (x + xx, y + yy))

    gt xx yy = Result.withDefault T0 <| Matrix.get xx yy mt
  in
    Matrix.map (\(xx, yy) -> gt xx yy) idxm



-- matrix, smaller matrix, x, y 
setSubMatrix : Matrix Tile -> Matrix Tile -> Int -> Int -> Matrix Tile
setSubMatrix mt smt x y =
  let
    ismt = Matrix.toArray <| Matrix.indexedMap (\xi yj t -> (xi + x, yj + y, t)) smt
    fsetA = Array.map (\(xi, yj, t) -> Matrix.set xi yj t) ismt

  in
    Array.foldr (<|) mt fsetA




addRow : Matrix Tile -> Matrix Tile
addRow mt =
  let
    wm = Matrix.width mt
    hm = Matrix.height mt

    lrow = getSubMatrix mt 0 (hm - 1) wm 1 -- get last row

    lcon = Matrix.map (\t -> if (ad t == S) then T1 else T0) lrow
  in
    Result.withDefault mt <| Matrix.concatVertical mt lcon


remRow : Matrix Tile -> Matrix Tile
remRow mt =
  let
    wm = Matrix.width mt
    hm = Matrix.height mt

    lrow = getSubMatrix mt 0 (hm - 1) wm 1 -- get last row
    restmt = getSubMatrix mt 0 0 wm (hm - 1)

    lcon = Matrix.foldr (&&) True <| Matrix.map (\t -> if ((t == T0) || (t == T1)) then True else False) lrow
  in
    if lcon  && (hm > 1) then restmt else mt


addCol : Matrix Tile -> Matrix Tile
addCol mt =
  let
    wm = Matrix.width mt
    hm = Matrix.height mt

    lcol = getSubMatrix mt (wm - 1) 0 1 hm -- get last row

    lcon = Matrix.map (\t -> if (ar t == S) then T2 else (if (ar t == M) then T10 else T0)) lcol
  in
    Result.withDefault mt <| Matrix.concatHorizontal mt lcon


remCol : Matrix Tile -> Matrix Tile
remCol mt =
  let
    wm = Matrix.width mt
    hm = Matrix.height mt

    lcol = getSubMatrix mt (wm - 1) 0 1 hm -- get last row
    restmt = getSubMatrix mt 0 0 (wm - 1) hm

    lcon = Matrix.foldr (&&) True <| Matrix.map (\t -> if ((t == T0) || (t == T2)) then True else False) lcol
  in
    if lcon && (wm > 1) then restmt else mt



------------------------------------------------------------------------------------------------

-- inner, morphism switch, morphism move up down, morphism move left right, boundary up, boundary down
-- i j
type Selection = Inn Int Int | BU Int | BD Int | MS Int Int | Mor Int Int | NoSel

selection : Matrix Tile -> Loc -> Selection
selection mt loc =
  let
    wm = Matrix.width mt
    hm = Matrix.height mt
  in
    -- handel 2 x 2 case
    case loc of
         LMat i j q t -> if (i == 0) && (j <= (wm - 2)) then -- upper boundary
                             BU j
                         else if (i == (hm - 1)) && (j <= (wm - 2)) then -- lower boundary
                             BD j
                         else if ((j <= (wm - 2)) && (i <= (hm - 2)) && (j /= 0) && (i /= 0)) then
                            case t of
                              T7  -> MS i j 
                              T8  -> MS i j
                              T9  -> Mor i j
                              T10 -> NoSel
                              _   -> case q of
                                        NW -> Inn (i-1) (j-1)
                                        NE -> Inn (i-1) j
                                        SE -> Inn i j
                                        SW -> Inn i (j-1)
                         else
                             NoSel
         _           -> NoSel
    

applyRule : Matrix Tile -> Selection -> Matrix Tile
applyRule mt sel = 
  let
    wm = Matrix.width mt
    hm = Matrix.height mt
  in
    case sel of
      Inn i j -> setSubMatrix mt (innRule (getSubMatrix mt j i 2 2)) j i
      BU j    -> setSubMatrix mt (buRule (getSubMatrix mt j 0 2 1)) j 0
      BD j    -> setSubMatrix mt (bdRule (getSubMatrix mt j (hm - 1) 2 1)) j (hm - 1) -- try hm
      MS i j -> setSubMatrix mt (msRule (getSubMatrix mt j (i-1) 2 3)) j (i-1)
      _       -> mt

----------------------------------------------------------------------------------------


selectAndApply : Loc -> Matrix Tile -> Matrix Tile
selectAndApply loc mt = applyRule mt <| selection mt loc








-- must be a 2 x 2 matrix
innRule : Matrix Tile -> Matrix Tile
innRule mt = 
  let
    tis = [ Result.withDefault T0 <| Matrix.get 0 0 mt
          , Result.withDefault T0 <| Matrix.get 1 0 mt
          , Result.withDefault T0 <| Matrix.get 1 1 mt
          , Result.withDefault T0 <| Matrix.get 0 1 mt
          ]
    ntis = List.map intToTile <| Rules.innR <| List.map tileToInt tis
    ftls = List.indexedMap (\i t -> case i of
                                      0 -> Matrix.set 0 0 t
                                      1 -> Matrix.set 1 0 t
                                      2 -> Matrix.set 1 1 t
                                      3 -> Matrix.set 0 1 t
                                      _ -> Matrix.set 0 0 t
                           ) ntis
    emt = Matrix.repeat 2 2 T0
  in
    List.foldr (<|) emt ftls


-- must be a 2 x 1 matrix
buRule : Matrix Tile -> Matrix Tile
buRule mt = 
  let
    tis = [ Result.withDefault T0 <| Matrix.get 0 0 mt
          , Result.withDefault T0 <| Matrix.get 1 0 mt
          ]
    ntis = List.map intToTile <| Rules.buR <| List.map tileToInt tis
    ftls = List.indexedMap (\i t -> case i of
                                      0 -> Matrix.set 0 0 t
                                      1 -> Matrix.set 1 0 t
                                      _ -> Matrix.set 0 0 t
                           ) ntis
    emt = Matrix.repeat 2 1 T0
  in
    List.foldr (<|) emt ftls


-- must be a 2 x 1 matrix
bdRule : Matrix Tile -> Matrix Tile
bdRule mt = 
  let
    tis = [ Result.withDefault T0 <| Matrix.get 0 0 mt
          , Result.withDefault T0 <| Matrix.get 1 0 mt
          ]
    ntis = List.map intToTile <| Rules.bdR <| List.map tileToInt tis
    ftls = List.indexedMap (\i t -> case i of
                                      0 -> Matrix.set 0 0 t
                                      1 -> Matrix.set 1 0 t
                                      _ -> Matrix.set 0 0 t
                           ) ntis
    emt = Matrix.repeat 2 1 T0
  in
    List.foldr (<|) emt ftls


-- must be 2 x 3 matrix
msRule : Matrix Tile -> Matrix Tile
msRule mt =
  let
    tis = [ Result.withDefault T0 <| Matrix.get 0 0 mt
          , Result.withDefault T0 <| Matrix.get 1 0 mt
          , Result.withDefault T0 <| Matrix.get 0 1 mt
          , Result.withDefault T0 <| Matrix.get 1 1 mt
          , Result.withDefault T0 <| Matrix.get 0 2 mt
          , Result.withDefault T0 <| Matrix.get 1 2 mt
          ]
    ntis = List.map intToTile <| Rules.msR <| List.map tileToInt tis
    ftls = List.indexedMap (\i t -> case i of
                                      0 -> Matrix.set 0 0 t
                                      1 -> Matrix.set 1 0 t
                                      2 -> Matrix.set 0 1 t
                                      3 -> Matrix.set 1 1 t
                                      4 -> Matrix.set 0 2 t
                                      5 -> Matrix.set 1 2 t
                                      _ -> Matrix.set 0 0 t
                           ) ntis
    emt = Matrix.repeat 2 3 T0
  in
    List.foldr (<|) emt ftls






isWellConnected : Matrix Tile -> Bool
isWellConnected mt =
  let
    wm = Matrix.width mt
    hm = Matrix.height mt

    gt x y = Result.withDefault T0 <| Matrix.get x y mt
    
    dChk = Matrix.foldl (&&) True <| Matrix.indexedMap (\x y t -> (ad t) == (au (gt x (y + 1)))) <| getSubMatrix mt 0 0 wm (hm - 1)
    uChk = Matrix.foldl (&&) True <| Matrix.indexedMap (\x y t -> (au t) == (ad (gt x y))) <| getSubMatrix mt 0 1 wm (hm - 1)
    rChk = Matrix.foldl (&&) True <| Matrix.indexedMap (\x y t -> (ar t) == (al (gt (x + 1) y))) <| getSubMatrix mt 0 0 (wm - 1) hm
    lChk = Matrix.foldl (&&) True <| Matrix.indexedMap (\x y t -> (al t) == (ar (gt x y))) <| getSubMatrix mt 1 0 (wm - 1) hm
  in
    dChk && uChk && rChk && lChk


-- i j of T9, wellconnected mt, -> number of 
spanOfT9 : Int -> Int -> Matrix Tile -> Int
spanOfT9 x y mt =
  let
    wm = Matrix.width mt
    hm = Matrix.height mt
  
    --gt x y = Result.withDefault T0 <| Matrix.get x y mt

    rowls = Matrix.foldr (::) [] (getSubMatrix mt x y (wm - x) 1)
    rowb = List.filter (\(xi, b) -> b)  <| List.indexedMap (\xi t -> (xi, (t == T10))) rowls

    (xl, _) = Maybe.withDefault (0, False) <| List.head rowb
    
  in
    xl + 1


-- no pts on boundary and if mij = T5 and mi j+k = T6 then there exists l between j and j + k such that m il = T3, likewise for T4 and T3 and T6
-- no cups and caps
isProgressive : Matrix Tile -> Bool
isProgressive mt =
  let
    wm = Matrix.width mt
    hm = Matrix.height mt

    gt x y = Result.withDefault T0 <| Matrix.get x y mt
    
    --ws = List.map (\i -> getSubMatrix 0 i wm 1) <| range 0 (hm - 1)
  in
    True


