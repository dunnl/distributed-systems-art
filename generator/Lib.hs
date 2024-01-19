module Lib where

import Backend
import Diagrams.Prelude

myRed :: Colour Double
myRed = sRGB24 214 39 40

myBlue :: Colour Double
myBlue = sRGB24 31 119 180

myGreen :: Colour Double
myGreen = sRGB24 44 160 44

_WORLDLENGTH :: Double
_WORLDLENGTH = 350

mkProcLabel :: String -> Diagram B
mkProcLabel lbl =
  (text lbl # fontSizeL 5 # fc black <>
  rect 30 20 # lw 0 # lw 0) # translateX (-15)

-- Make a process timeline, a simple _WORLDLENGTH-length line with a label on the left
mkProcessLine :: String -> Diagram B
mkProcessLine name =
  mkProcLabel name ||| arrowBetween' arrowStyle start end # lineOpts
  where
    start = p2 (0, 0)
    end   = p2 (_WORLDLENGTH , 0)
    arrowStyle = with & arrowHead .~ tri & headLength .~ small
    lineOpts = lwL 1 . lc black

-- Create an label on an event
mkEventLabel :: String
             -> Diagram B
mkEventLabel label =
  circle 0.1 # lw 0 <>
  text label # fontSizeL 5 # translateY 5

mkEvent :: String -- ^ Name
        -> String -- ^ Label
        -- -> (Double, Double) -- ^ Label (x,y)-offset
        -> Double  -- ^ Start time
        -> Double  -- ^ Duration
        -> Diagram B
--mkEvent name label (lblx, lbly) start duration =
mkEvent name label start duration =
  (mkEventLabel label # showOrigin === (s <> t <> box # fc myBlue)) # translate offset
  where
    offset = (start + duration/2) ^& 3.4
    box = rect duration 3 # lw 2
    s = mempty # named (name .> "start") # translateX (-duration/2)
    t = mempty # named (name .> "stop") # translateX (duration/2)

mkPointLabel :: String -> Diagram B
mkPointLabel lbl =
  (text lbl # fontSizeL 5 # fc black <>
  rect 20 10 # lw 0 # fc white)

mkPoint :: Double -- ^ X location
        -> String -- ^ Name
        -> String -- ^ Label
        -> Maybe (Double, Double) -- ^ Label offset
        -> Diagram B -- ^ Marker
        -> Diagram B
mkPoint xloc nm lbl mlbloff marker =
  (marker <> label # named nm) # translateX xloc
  where
    label = mkPointLabel lbl # offset_label
    offset_label = maybe id (\offset -> translate (r2 offset)) mlbloff

mkCircle :: Diagram B
mkCircle  = circle 3 # fc black # lw 0

noCircle :: String -> Diagram B
noCircle lbl = mkPointLabel lbl # translateY (-10)

linearizationPoint :: String -> Diagram B
linearizationPoint _ =
  rect 5 20 # fc red # lw 0 # translateY 1.5

mkArrow :: ArrowOpts Double -> Bool -> Point V2 Double -> Point V2 Double ->  Diagram B
mkArrow opts dashed = arrowBetween' opts # lwL 1 # (if dashed then dashingN [0.01,0.01] 0 else id)

attach :: ArrowOpts Double -> Bool -> (Name, Name) -> Diagram B -> Diagram B
attach opts dashed (n1, n2) =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
       beneath (mkArrow opts dashed (location b1) (location b2))

attachAll :: ArrowOpts Double -> Bool -> [(String, String)] -> Diagram B -> Diagram B
attachAll opts dashed = appEndo . foldMap (Endo . attach opts dashed . toNames)
  where toNames (x, y) = (toName x, toName y)

mkWorldLine :: String  -- ^ Left-most label
            -> Double  -- ^ Y (vertical) coordinate of worldline
            -> [(String, String, Double, Double)] -- ^ Events
            -> [(Double, String, String, Maybe (Double, Double), Diagram B)] -- ^ Points with rendering function
            -> Diagram B
mkWorldLine name y es pts =
  (mconcat points <>
   mkProcessLine name <>
   mconcat events) # translateY y
                   # (.>>) name
  where
    events = (\(nm,lbl,s,dur) -> mkEvent nm lbl s dur) <$> es
    points = (\(xloc, nm, lbl, mlbloff, marker) -> mkPoint xloc nm lbl mlbloff marker) <$> pts

mkWorlds :: [(String, Double, [(String, String, Double, Double)], [(Double, String, String, Maybe (Double, Double), Diagram B)])]
         -> [(String, String)]
         -> ArrowOpts Double
         -> Bool
         -> Diagram B
mkWorlds worlds points opts dashed =
    attachAll opts dashed points ws
  where
  ws = foldMap (\(name, y, events, pts) -> mkWorldLine name y events pts) worlds
