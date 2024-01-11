module Lib where

import Diagrams.Prelude
import Diagrams.Backend.PGF

mkProcLabel :: String -> Diagram B
mkProcLabel lbl =
  (text lbl # fontSizeL 5 # fc black <>
  rect 30 20 # lw 0 # lw 0) # translateX (-15)

mkProcLine :: String -> Diagram B
mkProcLine name =
  mkProcLabel name |||
  arrowBetween' (with & arrowHead .~ tri & headLength .~ small) (p2 (0, 0)) (p2 (400,0)) # lwL 1 # lc black

mkEventLabel :: String
             -> Diagram B
mkEventLabel label =
  circle 0.1 # lw 0 <>
  text label # fontSizeL 5 # translateY 5

mkEvent :: String -- ^ Name
        -> String -- ^ Label
        -> Double  -- ^ Start time
        -> Double  -- ^ Duration
        -> Diagram B
mkEvent name label start duration =
  (mkEventLabel label # showOrigin === (s <> t <> box)) # translate offset
  where
    offset = (start + duration/2) ^& 3.4
    box = rect duration 3 # lw 2
    s = mempty # named (name .> "start") # translateX (-duration/2)
    t = mempty # named (name .> "stop") # translateX (duration/2)

mkArrow :: ArrowOpts Double -> Bool -> Point V2 Double -> Point V2 Double ->  Diagram B
mkArrow opts dashed = arrowBetween' opts # lwL 1 # (if dashed then dashingN [0.01,0.01] 0 else id)

mkPointLabel :: String -> Diagram B
mkPointLabel lbl =
  text lbl # fontSizeL 5 # fc black <>
  square 20 # lw 0

mkPoint :: Double -- ^ X location
        -> String -- ^ Name
        -> String -- ^ Label
        -> (String -> Diagram B)
        -> Diagram B
mkPoint s nm lbl make =
  (make lbl # named nm) # translateX s

mkCircle :: String -> Diagram B
mkCircle lbl = (circle 3 # fc black # lw 0) === mkPointLabel lbl

noCircle :: String -> Diagram B
noCircle lbl = mkPointLabel lbl # translateY (-10)

mkLinearization :: String -> Diagram B
mkLinearization _ =
  rect 3 3 # fc red # lw 0 # translateY 1.5
  --rect 3 13 # fc red # lw 0 # translateY 2.5

attach :: ArrowOpts Double -> Bool -> (Name, Name) -> Diagram B -> Diagram B
attach opts dashed (n1, n2) =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
       atop (mkArrow opts dashed (location b1) (location b2))

attachAll :: ArrowOpts Double -> Bool -> [(Name, Name)] -> Diagram B -> Diagram B
attachAll opts dashed = appEndo . foldMap (Endo . attach opts dashed)

mkWorldLine :: String  -- ^ Label
            -> Double  -- ^ Y axis
            -> [(String, String, Double, Double)] -- ^ Events
            -> [(Double, String, String, String -> Diagram B)] -- ^ Points with rendering function
            -> Diagram B
mkWorldLine name y es pts =
  (mkProcLine name <>
   mconcat events <>
   mconcat points) # translateY y
                   # (.>>) name
  where
    events = (\(nm,lbl,s,dur) -> mkEvent nm lbl s dur) <$> es
    points = (\(s, nm, lbl, make) -> mkPoint s nm lbl make) <$> pts

mkWorlds :: [(String, Double, [(String, String, Double, Double)], [(Double, String, String, String -> Diagram B)])]
         -> [(Name, Name)]
         -> ArrowOpts Double
         -> Bool
         -> Diagram B
mkWorlds worlds points opts dashed =
    attachAll opts dashed points ws
  where
  ws = foldMap (\(name, y, events, pts) -> mkWorldLine name y events pts) worlds
