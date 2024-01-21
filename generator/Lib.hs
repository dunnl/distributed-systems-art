module Lib where

import Backend
import Diagrams.Prelude

myRed :: Colour Double
myRed = sRGB24 214 39 40

myBlue :: Colour Double
myBlue = sRGB24 31 119 180

myGreen :: Colour Double
myGreen = sRGB24 44 160 44

myHiRed :: Colour Double
myHiRed = sRGB24 220 38 127

myHiBlue :: Colour Double
myHiBlue = sRGB24 100 143 255

myHiYellow :: Colour Double
myHiYellow = sRGB24 255 176 0

myBlueA :: AlphaColour Double
myBlueA = withOpacity myBlue 0.5

_WORLDLENGTH :: Double
_WORLDLENGTH = 350

_IN_NOTEBOOK :: Bool
_IN_NOTEBOOK = False

math :: String -> String
math str = if _IN_NOTEBOOK then str else "$" ++ str ++ "$"

mkProcLabel :: String -> Diagram B
mkProcLabel lbl =
  (text lbl # fontSizeL 5 # fc black <>
  rect 30 30 # lw 0 # lw 0) # translateX (-15)

-- Make a process timeline, a simple _WORLDLENGTH-length line with a label on the left
mkProcessLine :: String -> Diagram B
mkProcessLine name =
  mkProcLabel name ||| arrowBetween' arrowStyle start end # lineOpts
  where
    start = p2 (0, 0)
    end   = p2 (_WORLDLENGTH , 0)
    arrowStyle = with & arrowHead .~ tri & headLength .~ small
    lineOpts = lwL 1.5 . lc black

-- Create an label on an event
mkOperationLabel :: String
                 -> Diagram B
mkOperationLabel label =
  circle 0.1 # lw 0 <>
  text label # fontSizeL 5 # translateY 5

mkOperation :: String -- ^ Name
            -> String -- ^ Label
            -> Double -- ^ Start time
            -> Double -- ^ Duration
            -> Diagram B
mkOperation name labelstr start duration =
  (label === operation) # translate offset
  where
    label = mkOperationLabel labelstr
    offset = xoff ^& yoff
    yoff = 3.4
    xoff = start + (duration/2)
    box = rect duration 3 # lw 2
    operation = s <> t <> box # fc myHiBlue
    s = mempty # named (name .> "start") # translateX (-duration/2)
    t = mempty # named (name .> "stop") # translateX (duration/2)

type OperationSpec = (String, String, Double, Double)

mkEventLabel :: String -> Diagram B
mkEventLabel lbl =
  text lbl # fontSizeL 5 # fc black
  <>
  rect 25 20 # lw 0 # fc white

mkEvent :: String -- ^ Name
        -> String -- ^ Label
        -> Double -- ^ Start time
        -> Maybe (Double, Double) -- ^ Label offset
        -> Diagram B -- ^ Marker
        -> Diagram B
mkEvent name labelstr start mlbloff marker =
  (marker <> label) # named name # translateX start
  where
    label = if labelstr == "" then mempty else mkEventLabel labelstr # offset_label
    offset_label = maybe id (\offset -> translate (r2 offset)) mlbloff

type EventSpec = (String, String, Double, Maybe (Double, Double), Diagram B)

attach' :: (IsName n1, IsName n2) => ArrowOpts Double -> (n1, n2) -> Diagram B -> Diagram B
attach' opts (n1, n2) =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
       beneath (arrowBetween' opts (location b1) (location b2))

-- (arrow options, pairs of names (from, to) to draw, the diagram w/o arrows)
attachAll' :: (IsName n1, IsName n2) => ArrowOpts Double -> [(n1, n2)] -> Diagram B -> Diagram B
attachAll' opts = appEndo . foldMap (Endo . attach' opts)

-- (arrow options, pairs of names (from, to) to draw, the diagram w/o arrows)
attachAllOutside' :: (IsName n1, IsName n2) => ArrowOpts Double -> [(n1, n2)] -> Diagram B -> Diagram B
attachAllOutside' opts = appEndo . foldMap (Endo . uncurry (connectOutside' opts))

mkWorldLine :: String  -- ^ Left-most label
            -> Double  -- ^ Y (vertical) coordinate of worldline
            -> [(String, String, Double, Double)] -- ^ Operations (name of operation, label, start point, duration)
            -> [(String, String, Double, Maybe (Double, Double), Diagram B)] -- ^ Events (name of event, label, start point, maybe (x,y) offset for label, rendering function)
            -> Diagram B
mkWorldLine name y ops evs =
  (mconcat events <>
   mkProcessLine name <>
   mconcat operations) # translateY y
                       # (.>>) name
  where
    operations = (\(nm,lbl,s,dur) -> mkOperation nm lbl s dur) <$> ops
    events     = (\(nm,lbl,s,mlbloff,marker) -> mkEvent nm lbl s mlbloff marker) <$> evs

mkWorlds :: (IsName n1, IsName n2)
         => [(String, Double, [(String, String, Double, Double)], [(String, String, Double, Maybe (Double, Double), Diagram B)])]
         -> [(n1, n2)]
         -> ArrowOpts Double
         -> Diagram B
mkWorlds worlds arrowPoints opts =
    attachAll' opts arrowPoints ws
  where
  ws = foldMap (\(name, y, ops, evs) -> mkWorldLine name y ops evs) worlds
