module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine (multiMain)
import Lib
import PartialOrder (partialOrder)

request = pad 1.1 $
     mkWorlds [ ("Process", 50, [("P1", "E", 70, 50)], [(70, "", "E.s", noCircle), (120, "", "E.t", noCircle)])
              , ("Client", 10, [], [(40, "S", "C.s", mkCircle), (150, "T", "C.t", mkCircle)])
              ]
              [ (toName "S", "P1" .> "start")
              , ("P1" .> "stop", toName "T")
              ]
              opts False
     where
       opts = def & arrowHead  .~ dart
         & arrowTail .~ noTail
         & headLength .~ normal
         & tailLength .~ normal
         & headGap    .~ small

externalorder = pad 1.1 $
     mkWorlds [ ("P3", 80, [("P31", "E2", 30, 20), ("P32", "E3", 55, 45), ("P33", "E6", 140, 83)], [])
              , ("P2", 40, [("P21", "E1", 10, 10), ("P22", "E5", 79, 24), ("P23", "E8", 200, 33)], [])
              , ("P1", 0,  [("P11", "E4", 72, 20), ("P12", "E7", 160, 23)], [])
              ]
              [ ("P31" .> "stop", "P22" .> "start")
              , ("P32" .> "stop", "P12" .> "start")
              , ("P21" .> "stop", "P11" .> "start")
              , ("P22" .> "stop", "P33" .> "start")
              , ("P22" .> "stop", "P12" .> "start")
              , ("P21" .> "stop", "P31" .> "start")
              , ("P12" .> "stop", "P23" .> "start")
              , ("P11" .> "stop", "P33" .> "start")
              ]
              (with & arrowHead .~ dart
                    & arrowTail .~ noTail
                    & headGap   .~ small) True
linear1 = pad 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(y, 1)", 40, 63), ("P23", "R(x,2)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(y,1)", 140, 83)], [])
              ]
              [
              ] def False
linear2 = pad 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 40, 63), ("P23", "R(x,2)", 200, 33)], [(45, "", "", mkLinearization), (215, "", "", mkLinearization)])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,2)", 140, 83)], [(57, "", "", mkLinearization), (180, "", "", mkLinearization)])
              ]
              [
              ]
              def False
linear3 = pad 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 40, 63), ("P23", "R(x,1)", 200, 33)], [(90, "", "", mkLinearization), (215, "", "", mkLinearization)])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,1)", 140, 83)], [(25, "", "", mkLinearization), (180, "", "", mkLinearization)])
              ]
              [
              ]
              def False

sequential1 = pad 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 40, 63), ("P23", "R(x,1)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,2)", 140, 83)], [])
              ]
              [
              ]
              def False

sequential2 = pad 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 140, 43), ("P23", "R(x,1)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,2)", 65, 63)], [])
              ]
              [
              ]
              def False

causal1 = pad 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 40, 63), ("P23", "R(x,2)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,1)", 140, 83)], [])
              ]
              [
              ]
              def False

main :: IO ()
main = multiMain $
  [ ("request", request)
  , ("externalorder", externalorder)
  , ("partialorder", partialOrder)
  , ("linear1", linear1)
  , ("linear2", linear2)
  , ("linear3", linear3)
  , ("sequential1", sequential1)
  , ("sequential2", sequential2)
  , ("causal1", causal1)
  ]
