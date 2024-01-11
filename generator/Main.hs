module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine (multiMain)
import Lib
import PartialOrder (partialOrder)

request = padY 1.1 $
     mkWorlds [ ("Process", 50, [("$P_1$", "E", 90, 50)], [(70, "", "E.s", noCircle), (140, "", "E.t", noCircle)])
              , ("Client", 10, [], [(60, "S", "C.s", mkCircle), (170, "T", "C.t", mkCircle)])
              ]
              [ (toName "S", "$P_1$" .> "start")
              , ("$P_1$" .> "stop", toName "T")
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
     mkWorlds [ ("$P_3$", 80, [("$P_3$1", "E2", 30, 20), ("$P_3$2", "E3", 55, 45), ("$P_3$3", "E6", 140, 83)], [])
              , ("$P_2$", 40, [("$P_2$1", "E1", 10, 10), ("$P_2$2", "E5", 79, 24), ("$P_2$3", "E8", 200, 33)], [])
              , ("$P_1$", 0,  [("$P_1$1", "E4", 72, 20), ("$P_1$2", "E7", 160, 23)], [])
              ]
              [ ("$P_3$1" .> "stop", "$P_2$2" .> "start")
              , ("$P_3$2" .> "stop", "$P_1$2" .> "start")
              , ("$P_2$1" .> "stop", "$P_1$1" .> "start")
              , ("$P_2$2" .> "stop", "$P_3$3" .> "start")
              , ("$P_2$2" .> "stop", "$P_1$2" .> "start")
              , ("$P_2$1" .> "stop", "$P_3$1" .> "start")
              , ("$P_1$2" .> "stop", "$P_2$3" .> "start")
              , ("$P_1$1" .> "stop", "$P_3$3" .> "start")
              ]
              (with & arrowHead .~ dart
                    & arrowTail .~ noTail
                    & headGap   .~ small) True
linear1 = padY 1.1 $
     mkWorlds [ ("$P_2$", 40, [("$P_2$1", "W(y, 1)", 40, 63), ("$P_2$3", "R(x,2)", 200, 33)], [])
              , ("$P_1$", 0,  [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(y,1)", 140, 83)], [])
              ]
              [
              ] opts False
linearTemplate = padY 1.1 $
     mkWorlds [ ("$P_2$", 40, [("$P_2$1", "W(x, 1)", 40, 63), ("$P_2$3", "R(x,_)", 200, 33)], [])
              , ("$P_1$", 0,  [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(x,_)", 140, 83)], [])
              ]
              [
              ]
              opts False
linear2 = padY 1.1 $
     mkWorlds [ ("$P_2$", 40, [("$P_2$1", "W(x, 1)", 40, 63), ("$P_2$3", "R(x,2)", 200, 33)], [(45, "", "", mkLinearization), (215, "", "", mkLinearization)])
              , ("$P_1$", 0,  [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(x,2)", 140, 83)], [(57, "", "", mkLinearization), (180, "", "", mkLinearization)])
              ]
              [
              ]
              opts False
linear3 = padY 1.1 $
     mkWorlds [ ("$P_2$", 40, [("$P_2$1", "W(x, 1)", 40, 63), ("$P_2$3", "R(x,1)", 200, 33)], [(90, "", "", mkLinearization), (215, "", "", mkLinearization)])
              , ("$P_1$", 0,  [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(x,1)", 140, 83)], [(25, "", "", mkLinearization), (180, "", "", mkLinearization)])
              ]
              [
              ]
              opts False
nonlinear0 = padY 1.1 $
     mkWorlds [ ("$P_2$", 40, [("$P_2$1", "W(y, 1)", 40, 63), ("$P_2$3", "R(x,2)", 200, 33)], [])
              , ("$P_1$", 0,  [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(y,0)", 140, 83)], [])
              ]
              [
              ]
              opts False
nonlinear1 = padY 1.1 $
     mkWorlds [ ("$P_2$", 40, [("$P_2$1", "W(x, 1)", 40, 63), ("$P_2$3", "R(x,1)", 200, 33)], [])
              , ("$P_1$", 0,  [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(x,2)", 140, 83)], [])
              ]
              [
              ]
              opts False
nonlinear2 = padY 1.1 $
     mkWorlds [ ("$P_2$", 40, [("$P_2$1", "W(x, 1)", 40, 63), ("$P_2$3", "R(x,2)", 200, 33)], [])
              , ("$P_1$", 0,  [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(x,1)", 140, 83)], [])
              ]
              [
              ]
              opts False

sequential1 = nonlinear1

sequential2 = padY 1.1 $
     mkWorlds [ ("$P_2$", 40, [("$P_2$1", "W(x, 1)", 140, 43), ("$P_2$3", "R(x,1)", 200, 33)], [])
              , ("$P_1$", 0,  [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(x,2)", 65, 63)], [])
              ]
              [
              ]
              opts False

sequential3 = padY 1.1 $
     mkWorlds [ ("P", 40, [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(x,2)", 65, 63), ("$P_2$1", "W(x, 1)", 140, 43), ("$P_2$3", "R(x,1)", 200, 33)], [])
              ]
              [
              ]
              opts False

nonsequential1 = padY 1.1 $
     mkWorlds [ ("$P_2$", 50, [("$P_2$1", "R(y,1)", 35, 37), ("$P_2$2", "W(x,1)", 110, 18)], [])
              , ("$P_1$", 10, [("$P_1$1", "R(x,1)", 20, 40), ("$P_1$2", "W(y,1)", 90, 30)], [])
              ]
              [ (toName "S", "$P_1$" .> "start")
              , ("$P_1$" .> "stop", toName "T")
              ]
              (with & arrowHead  .~ dart
                & arrowTail .~ noTail
                & headLength .~ normal
                & tailLength .~ normal
                & headGap    .~ small) False

nonsequential_x = padY 1.1 $
     mkWorlds [ ("$P_2$", 50, [("$P_2$2", "W(x,1)", 110, 18)], [])
              , ("$P_1$", 10, [("$P_1$1", "R(x,1)", 20, 40)], [])
              ]
              [ (toName "S", "$P_1$" .> "start")
              , ("$P_1$" .> "stop", toName "T")
              ] def False
nonsequential_y = padY 1.1 $
     mkWorlds [ ("$P_2$", 50, [("$P_2$1", "R(y,1)", 35, 37)], [])
              , ("$P_1$", 10, [("$P_1$2", "W(y,1)", 90, 30)], [])
              ]
              [ (toName "S", "$P_1$" .> "start")
              , ("$P_1$" .> "stop", toName "T")
              ] def False

causal1 = padY 1.1 $
     mkWorlds [ ("$P_2$", 40, [("$P_2$1", "W(x, 1)", 40, 63), ("$P_2$3", "R(x,2)", 200, 33)], [])
              , ("$P_1$", 0,  [("$P_1$1", "W(x, 2)", 10, 52), ("$P_1$2", "R(x,1)", 140, 83)], [])
              ]
              [
              ]
              def False

messages = pad 1.1 $
     mkWorlds [ ("$P_1$", 70, [], [(50, "alice", "m2.s", mkCircle), (80, "m11", "m1.r", mkCircle), (140, "m12", "m4.s", mkCircle), (315, "bob", "m3.r", mkCircle)])
              , ("$P_2$", 10, [], [(40, "m21", "m1.s", mkCircle), (170, "m22", "m4.r", mkCircle)])
              , ("$P_3$", -50, [], [(110, "p3_bob", "m3.s", mkCircle), (230, "alice2", "m2.r", mkCircle)])
              ]
              [ (toName "m21", toName "m11")
              , (toName "m12", toName "m22")
              , (toName "p3_bob", toName "bob")
              , (toName "alice", toName "alice2")
              ]
              opts False
  where
    opts = def & arrowHead  .~ dart
           & arrowTail .~ noTail
           & headLength .~ normal
           & tailLength .~ normal
           & headGap    .~ small

main :: IO ()
main = multiMain $
  [ ("request", request)
  , ("externalorder", externalorder)
  , ("messages", messages)
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
