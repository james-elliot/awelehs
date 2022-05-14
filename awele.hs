import Graphics.UI.SDL as SDL
-- import Graphics.UI.SDL.Color as SDLC
import Graphics.UI.SDL.Primitives as SDLP
import Graphics.UI.SDL.TTF as SDLT
import Text.Printf
import Data.Array.Unboxed
--import Data.Array
import Debug.Trace
import Data.Maybe
-- import Data.Typeable
import GHC.Int
import Control.Monad
import System.CPUTime as Cputime

black = SDL.Pixel 0x000000FF
red   = SDL.Pixel 0xFF0000FF
green = SDL.Pixel 0x00FF00FF
blue  = SDL.Pixel 0x0000FFFF
grey  = SDL.Pixel 0x808080FF
white = SDL.Pixel 0xFFFFFFFF
width = 900::Int16
height = 340::Int16
xybase = 100
xyinc = 140
hrad = 35
rad = 60
smallr = 4
nbmax = 8
ecart = 10
fontname = "/usr/share/fonts/truetype/freefont/FreeMono.ttf"
fontsize = 18
fontcolor = (Color 0xff 0xff 0xff)
fontpos = (Just (Rect 400 0 0 0))

init_screen = do
  SDL.init [SDL.InitVideo,InitEventthread]
  screen <- SDL.setVideoMode (fromIntegral width) (fromIntegral height) 32 
            [SDL.SWSurface,SDL.DoubleBuf]
  SDLT.init
  font <- openFont fontname fontsize
  SDL.enableEvent SDL.SDLKeyUp False
  SDL.enableEvent SDL.SDLKeyDown False
  SDL.enableEvent SDL.SDLMouseButtonDown False
  SDL.enableEvent SDL.SDLMouseButtonUp False
  SDL.enableEvent SDL.SDLMouseMotion False
  SDL.enableEvent SDL.SDLVideoExpose False
  SDL.enableEvent SDL.SDLSysWMEvent False
  SDL.enableEvent SDL.SDLActiveEvent False
  return (screen,font)

circles = [(xybase+(mod i 6)*xyinc,xybase+(quot i 6)*xyinc)|i<-[0..11]]

draw_circles (screen,font) pos val =
    let text = if (abs val) <25 then show val else "Fin de partie:"++(show val)
        f (x,y) = SDLP.filledCircle screen (fromIntegral x) (fromIntegral y) rad grey
        f2 (x,y) = SDLP.filledCircle screen (fromIntegral x) (fromIntegral y) smallr white
        pawns =
            concatMap
            (\v -> 
                 let (i,nb)= v
                     qt = quot i 6
                     xbase = if qt==0 then xybase+(mod i 6)*xyinc-hrad 
                             else xybase+(5-(mod i 6))*xyinc-hrad
                     ybase = xybase+qt*xyinc-hrad
                     f3 (-1) = [] 
                     f3 j = (xbase+(mod j nbmax)*ecart,ybase+(quot j nbmax)*ecart):(f3 (j-1)) in
                 f3 (nb-1))
            (assocs pos) in
  do
    SDLP.filledPolygon screen [(0,0),(0,height),(width,height),(width,0)] black
    forM_ circles f
    forM_ pawns f2
    surf <- SDLT.renderTextSolid font (show val) fontcolor
    SDL.blitSurface surf Nothing screen fontpos
    SDL.flip screen

get_circle p2 = do
  SDL.enableEvent SDL.SDLMouseButtonDown True
  event <- SDL.waitEvent
  SDL.enableEvent SDL.SDLMouseButtonDown False
  let MouseButtonDown a b c = event
      (af,bf) = (fromIntegral a,fromIntegral b)
      (imin,_) = 
          foldl (\(imin,icurr) (x,y) -> 
                  let (xf,yf) = (fromIntegral x,fromIntegral y)
                      d = sqrt ((xf-af)*(xf-af)+(yf-bf)*(yf-bf)) in
                  if d<(fromIntegral rad) && (p2 ! (17-icurr)) /=0 then (icurr,icurr+1) 
                  else (imin,icurr+1))
                (0,6) (drop 6 circles) in
    if imin /= 0 then return (17-imin) else get_circle p2
  
do_move::(Array Int Int, Int) -> Int -> (Array Int Int, Int)
do_move (ar,v) i = 
  let f x c (l,acc) = (x:l,c+acc)
      n = ar ! i ; tm = mod n 11 ; tq = quot n 11
      (inc,nb) = if tm==0 then (11,tq) else (tm,tq+1)
      last = mod (i + inc) 12
      go1 curr p  = 
        if curr==i then ([(i,0)],0) else
          let c = (ar ! curr)+1 in
          if p && (c>=2)&&(c<=3) && ((quot i 6)/=(quot curr 6)) then 
            f (curr,0) c (go1 (mod (curr-1) 12) p) else
            f (curr,c) 0 (go1 (mod (curr-1) 12) False)
      go2 curr p b
        | (curr == i) = f (i, 0) 0 (go2 (mod (curr - 1) 12) False (b - 1))
        | (curr == last) && (nb /= b) = ([], 0)
        | otherwise =
          let c = (ar ! curr) + b in
          if p && (c >= 2) && (c <= 3) && ((quot i 6) /= (quot curr 6)) then
            f (curr, 0) c (go2 (mod (curr - 1) 12) p b) else
            f (curr, c) 0 (go2 (mod (curr - 1) 12) False b)
      (res,acc) = if nb==1 then go1 last True else go2 last True nb in
  if i<6 then (ar//res,v+acc) else (ar//res,v-acc)

gen_pos (ar,v) low high =  go low
    where go i
              | i > high = []
              | otherwise = if (ar ! i) == 0 then (go (i + 1)) 
                            else (do_move (ar,v) i):(go (i + 1))

eval_pos (pos,v) = v  

alpha_beta :: Int -> Int -> Int -> Int -> (Array Int Int, Int) -> 
              (Int, Maybe (Array Int Int, Int))
alpha_beta alpha beta rem prof pos =
  let do_max (alpha,p) m = case m of
        [] -> (alpha,p)
        (x:xs) -> let (r,_) = (alpha_beta alpha beta (rem-1) (prof+1) x) in
          if r>=beta then (r,Just x) else
            if r>alpha then do_max (r,Just x) xs else do_max (alpha,p) xs
      do_min (beta,p) m = case m of
        [] -> (beta,p)
        (x:xs) -> let (r,_) = (alpha_beta alpha beta (rem-1) (prof+1) x) in
          if r<=alpha then (r,Just x) else
            if r<beta then do_min (r,Just x) xs else do_min (beta,p) xs in
  if rem == 0 then (eval_pos pos,Nothing) else
    if mod prof 2 == 0 then 
      let moves = (gen_pos pos 0 5) in
      if moves==[] then (eval_pos pos,Nothing) else do_max (alpha,Nothing) moves 
    else
      let moves = (gen_pos pos 6 11) in
      if moves==[] then (eval_pos pos,Nothing) else do_min (beta,Nothing) moves
  
play context pos = do
  cputime <- Cputime.getCPUTime
  let compute depth = 
        let (res,p)=alpha_beta (-32767) 32767 depth 0 pos in
        do
          printf "depth=%d res=%d\n" depth res
          cpu <- res `seq` Cputime.getCPUTime -- To force computation of res and p
          if ((cpu-cputime) < 1000000000000) && (res < 25) && (res > -25) then
            compute (depth+1) 
            else
            return (res,p,depth) in
    do
      (res,p,depth)  <- compute 1
      let (p2,v2) = fromJust p in 
--        traceShow (depth,res) $
        do
          draw_circles context p2 v2  
          if v2 >=25 then return v2 else
            do
              imin <- get_circle p2
              let (p3,v3) = do_move (p2,v2) imin in
               do
                 draw_circles context p3 v3
                 if v3<= (-25) then return v3 else play context (p3,v3)
        
pos_init = array (0,11) [(i,4)|i<-[0..11]]
main = do
  context <- init_screen
  draw_circles context pos_init 0
  v <- play context (pos_init,0)
  printf "End=%s\n" (show v)
  delay 10000
  SDL.quit
