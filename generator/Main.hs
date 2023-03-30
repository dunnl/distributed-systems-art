module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine (multiMain)
import Lib
import PartialOrder (partialOrder)

request = padY 1.1 $
     mkWorlds [ ("Process", 50, [("P1", "E", 90, 50)], [(70, "", "E.s", noCircle), (140, "", "E.t", noCircle)])
              , ("Client", 10, [], [(60, "S", "C.s", mkCircle), (170, "T", "C.t", mkCircle)])
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

opts = (with & arrowHead .~ dart
         & arrowTail .~ noTail
         & headGap   .~ small)

externalorder = padY 1.1 $
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
linear1 = padY 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(y, 1)", 40, 63), ("P23", "R(x,2)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(y,1)", 140, 83)], [])
              ]
              [
              ] opts False
linearTemplate = padY 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 40, 63), ("P23", "R(x,_)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,_)", 140, 83)], [])
              ]
              [
              ]
              opts False
linear2 = padY 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 40, 63), ("P23", "R(x,2)", 200, 33)], [(45, "", "", mkLinearization), (215, "", "", mkLinearization)])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,2)", 140, 83)], [(57, "", "", mkLinearization), (180, "", "", mkLinearization)])
              ]
              [
              ]
              opts False
linear3 = padY 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 40, 63), ("P23", "R(x,1)", 200, 33)], [(90, "", "", mkLinearization), (215, "", "", mkLinearization)])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,1)", 140, 83)], [(25, "", "", mkLinearization), (180, "", "", mkLinearization)])
              ]
              [
              ]
              opts False
nonlinear0 = padY 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(y, 1)", 40, 63), ("P23", "R(x,2)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(y,0)", 140, 83)], [])
              ]
              [
              ]
              opts False
nonlinear1 = padY 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 40, 63), ("P23", "R(x,1)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,2)", 140, 83)], [])
              ]
              [
              ]
              opts False
nonlinear2 = padY 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 40, 63), ("P23", "R(x,2)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,1)", 140, 83)], [])
              ]
              [
              ]
              opts False

sequential1 = nonlinear1

sequential2 = padY 1.1 $
     mkWorlds [ ("P2", 40, [("P21", "W(x, 1)", 140, 43), ("P23", "R(x,1)", 200, 33)], [])
              , ("P1", 0,  [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,2)", 65, 63)], [])
              ]
              [
              ]
              opts False

sequential3 = padY 1.1 $
     mkWorlds [ ("P", 40, [("P11", "W(x, 2)", 10, 52), ("P12", "R(x,2)", 65, 63), ("P21", "W(x, 1)", 140, 43), ("P23", "R(x,1)", 200, 33)], [])
              ]
              [
              ]
              opts False

nonsequential1 = padY 1.1 $
     mkWorlds [ ("P2", 50, [("P21", "R(y,1)", 35, 37), ("P22", "W(x,1)", 110, 18)], [])
              , ("P1", 10, [("P11", "R(x,1)", 20, 40), ("P12", "W(y,1)", 90, 30)], [])
              ]
              [ (toName "S", "P1" .> "start")
              , ("P1" .> "stop", toName "T")
              ]
              (with & arrowHead  .~ dart
                & arrowTail .~ noTail
                & headLength .~ normal
                & tailLength .~ normal
                & headGap    .~ small) False

nonsequential_x = padY 1.1 $
     mkWorlds [ ("P2", 50, [("P22", "W(x,1)", 110, 18)], [])
              , ("P1", 10, [("P11", "R(x,1)", 20, 40)], [])
              ]
              [ (toName "S", "P1" .> "start")
              , ("P1" .> "stop", toName "T")
              ] def False
nonsequential_y = padY 1.1 $
     mkWorlds [ ("P2", 50, [("P21", "R(y,1)", 35, 37)], [])
              , ("P1", 10, [("P12", "W(y,1)", 90, 30)], [])
              ]
              [ (toName "S", "P1" .> "start")
              , ("P1" .> "stop", toName "T")
              ] def False

causal1 = padY 1.1 $
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
  , ("linearTemplate", linearTemplate)
  , ("nonlinear0", nonlinear0)
  , ("nonlinear1", nonlinear1)
  , ("nonlinear2", nonlinear2)
  , ("sequential1", sequential1)
  , ("sequential2", sequential2)
  , ("sequential3", sequential3)
  , ("nonsequential1", nonsequential1)
  , ("nonsequential_x", nonsequential_x)
  , ("nonsequential_y", nonsequential_y)
  , ("causal1", causal1)
  ]
