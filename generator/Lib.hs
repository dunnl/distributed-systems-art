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

_PROCLINE_WIDTH :: Double
_PROCLINE_WIDTH = 1.5

math :: String -> String
math str = if _IN_NOTEBOOK then str else "$" ++ str ++ "$"

mkProcessLabel :: String -> Diagram B
mkProcessLabel lbl =
  (text lbl # fontSizeL 5 # fc black <>
  rect 30 30 # lw 0 # lw 0) # translateX (-15)

processLineArrowOps :: ArrowOpts Double
processLineArrowOps =
  with & arrowHead  .~ tri
       & headLength .~ small
       & shaftStyle %~ lwL _PROCLINE_WIDTH . lc black

-- Make a process timeline, a simple _WORLDLENGTH-length line with a label on the left
mkProcessLine :: String -> Diagram B
mkProcessLine name =
  label ||| line
  where
    start = p2 (0, 0)
    end   = p2 (_WORLDLENGTH , 0)
    line = arrowBetween' processLineArrowOps start end
    label = mkProcessLabel name

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
    borderwidth = 2
    boxheight = 3
    xoff = start + (duration/2)
    yoff = (boxheight + borderwidth)/2
    offset = xoff ^& yoff
    box = rect duration boxheight # lw borderwidth
    startEvent = mempty # named (name .> "start") # translateX (-duration/2)
    stopEvent  = mempty # named (name .> "stop")  # translateX (duration/2)
    operation = startEvent <> box <> stopEvent # fc myHiBlue

-- (marker, maybe an (x, y) offset for the label, maybe (x, y) size
-- for the label)
data EventSpec = EventSpec
  { marker :: Diagram B
  , eventName :: String
  , lbltext   :: String
  , lblstart  :: Double
  , lbloffset :: Maybe (Double, Double)
  , lblsize   :: Maybe (Double, Double)
  }

mkEventLabel :: EventSpec -> Diagram B
mkEventLabel (EventSpec _ _ lbl start moff msize) =
  (txt <> background) # offset_label
  where
    offset_label = maybe id (\offset -> translate (r2 offset)) moff
    (lblx, lbly) = maybe (10, 5) id msize
    background = rect lblx lbly # lw 0 # fc green
    txt = text lbl # fontSizeL 5 # fc black

mkEvent :: EventSpec
        -> Diagram B
mkEvent spec@(EventSpec marker name labelstr start moff msize) =
  (marker <> label) # named name # translateX start
  where
    prelabel = mkEventLabel spec
    label =if labelstr == "" then mempty else prelabel

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
            -> [EventSpec] -- ^ Events to draw
            -> Diagram B
mkWorldLine name y ops evs =
  (mconcat events <>
   mkProcessLine name <>
   mconcat operations) # translateY y
                       # (.>>) name
  where
    operations = (\(nm,lbl,s,dur) -> mkOperation nm lbl s dur) <$> ops
    events     = mkEvent <$> evs

mkWorlds :: (IsName n1, IsName n2)
         => [(String, Double, [(String, String, Double, Double)], [EventSpec])]
         -> [(n1, n2)]
         -> ArrowOpts Double
         -> Diagram B
mkWorlds worlds arrowPoints opts =
    attachAll' opts arrowPoints ws
  where
  ws = foldMap (\(name, y, ops, evs) -> mkWorldLine name y ops evs) worlds
