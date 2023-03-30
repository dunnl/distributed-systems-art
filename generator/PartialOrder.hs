module PartialOrder where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific

mkArrow :: ArrowOpts Double -> Bool -> Point V2 Double -> Point V2 Double ->  Diagram B
mkArrow opts dashed = arrowBetween' opts # lw 2

attach :: ArrowOpts Double -> Bool -> (Name, Name) -> Diagram B -> Diagram B
attach opts dashed (n1, n2) =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
       atop (mkArrow opts dashed (location b1) (location b2))

attachAll :: ArrowOpts Double -> Bool -> [(Name, Name)] -> Diagram B -> Diagram B
attachAll opts dashed = appEndo . foldMap (Endo . attach opts dashed)

opts = def & arrowHead  .~ dart
       & arrowTail .~ noTail
       & headLength .~ normal
       & tailLength .~ normal
       & tailGap    .~ large
       & headGap    .~ large

mkNode :: String -> Double -> Double -> Diagram B
mkNode name x y = (circle 1.5 <> text name) # named name # translate (r2 (x, y))

partialOrder = (mkNode "E1" 0 0 <> mkNode "E2" 10 7 <> mkNode "E3" 20 7 <> mkNode "E5" 20 0 <> mkNode "E4" 20 (-7) <> mkNode "E6" 30 7 <> mkNode "E7" 30 (-7) <> mkNode "E8" 40 (0))
  # attachAll opts True [(toName "E1", toName "E2"), (toName "E2", toName "E3"), (toName "E1", toName "E5"), (toName "E1", toName "E4"), (toName "E3", toName "E6"), (toName "E5", toName "E6")
  , (toName "E4", toName "E6"), (toName "E3", toName "E7"), (toName "E5", toName "E7"), (toName "E4", toName "E7"), (toName "E7", toName "E8")
  ]
