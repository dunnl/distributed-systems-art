module Main (main) where

import Backend
import Lib
import PartialOrder (partialOrder)
import Diagrams.Prelude

messagePassingArrowOpts = with & arrowHead  .~ dart
       & arrowTail .~ noTail
       & headLength .~ normal
       & tailLength .~ normal
       & headGap    .~ small

messagePassingExample :: Diagram B
messagePassingExample = pad 1.1 $
     mkWorlds [ ("$P_1$", 70, [], [(50, "m2s", "$m_2^\\textrm{send}$", Just (0, 10), mkCircle), (95, "m1r", "$m_1^\\textrm{recv}$", Just (0, 10), mkCircle), (140, "m4s", "$m_4^\\textrm{send}$", Just (0, 10), mkCircle), (315, "m3r", "$m_3^\\textrm{recv}$", Just (0, 10), mkCircle)])
              , ("$P_2$", 10, [], [(40, "m1s", "$m_1^\\textrm{send}$", Just (0, -10), mkCircle), (255, "m5s", "$m_5^\\textrm{send}$", Just (5, 10), mkCircle), (195, "m4r", "$m_4^\\textrm{recv}$", Just (6, 10), mkCircle)])
              , ("$P_3$", -50, [], [(110, "m3s", "$m_3^\\textrm{send}$", Just (0, -10), mkCircle), (320, "m5r", "$m_5^\\textrm{recv}$", Just (0, -10), mkCircle), (260, "m2r", "$m_2^\\textrm{recv}$", Just (0, -10), mkCircle)])
              ]
              [ ("m1s", "m1r")
              , ("m4s", "m4r")
              , ("m3s", "m3r")
              , ("m5s", "m5r")
              , ("m2s", "m2r")
              ]
              messagePassingArrowOpts False

messagePassingExampleScalar :: Diagram B
messagePassingExampleScalar = pad 1.1 $
     mkWorlds [ ("$P_1$", 70, [], [(50, "m2s", "$1$", Just (0, 10), mkCircle), (95, "m1r", "$2$", Just (0, 10), mkCircle), (140, "m4s", "$3$", Just (0, 10), mkCircle), (315, "m3r", "$4$", Just (0, 10), mkCircle)])
              , ("$P_2$", 10, [], [(40, "m1s", "$1$", Just (0, -10), mkCircle), (255, "m5s", "$5$", Just (5, 10), mkCircle), (195, "m4r", "$4$", Just (6, 10), mkCircle)])
              , ("$P_3$", -50, [], [(110, "m3s", "$1$", Just (0, -10), mkCircle), (320, "m5r", "$6$", Just (0, -10), mkCircle), (260, "m2r", "$2$", Just (0, -10), mkCircle)])
              ]
              [ ("m1s", "m1r")
              , ("m4s", "m4r")
              , ("m3s", "m3r")
              , ("m5s", "m5r")
              , ("m2s", "m2r")
              ]
              messagePassingArrowOpts False

messagePassingExample2 :: Diagram B
messagePassingExample2 = pad 1.1 $
     mkWorlds [ ("$P_1$", 70, [], [ m2send, m3recv, m4send, m5recv, m6send, m1recv ])
              , ("$P_2$", 10, [], [ m2recv, m3send, m4recv, m5send, m6recv ])
              , ("$P_3$", -50, [], [ m1send ])
              ]
              [ ("m1send", "m1recv")
              , ("m2send", "m2recv")
              , ("m3send", "m3recv")
              , ("m4send", "m4recv")
              , ("m5send", "m5recv")
              , ("m6send", "m6recv")
              ]
              messagePassingArrowOpts False
  where
    m1send = (20, "m1send", "$m_1^\\textrm{send}$", Just (0, -10), mkCircle)
    m1recv = (310, "m1recv", "$m_1^\\textrm{recv}$", Just (0, 10), mkCircle)
    m2send = (40, "m2send", "$m_2^\\textrm{send}$", Just (0, 10), mkCircle)
    m2recv = (60, "m2recv", "$m_2^\\textrm{recv}$", Just (0, -10), mkCircle)
    m3send = (80, "m3send", "$m_3^\\textrm{send}$", Just (0, -10), mkCircle)
    m3recv = (100, "m3recv", "$m_3^\\textrm{recv}$", Just (0, 10), mkCircle)
    m4send = (120, "m4send", "$m_4^\\textrm{send}$", Just (0, 10), mkCircle)
    m4recv = (140, "m4recv", "$m_4^\\textrm{recv}$", Just (0, -10), mkCircle)
    m5send = (210, "m5send", "$m_5^\\textrm{send}$", Just (0, -10), mkCircle)
    m5recv = (230, "m5recv", "$m_5^\\textrm{recv}$", Just (0, 10), mkCircle)
    m6send = (250, "m6send", "$m_6^\\textrm{send}$", Just (0, 10), mkCircle)
    m6recv = (270, "m6recv", "$m_6^\\textrm{recv}$", Just (0, -10), mkCircle)

messagePassingExampleScalar2 :: Diagram B
messagePassingExampleScalar2 = pad 1.1 $
     mkWorlds [ ("$P_1$", 70, [], [ m2send, m3recv, m4send, m5recv, m6send, m1recv ])
              , ("$P_2$", 10, [], [ m2recv, m3send, m4recv, m5send, m6recv ])
              , ("$P_3$", -50, [], [ m1send ])
              ]
              [ ("m1send", "m1recv")
              , ("m2send", "m2recv")
              , ("m3send", "m3recv")
              , ("m4send", "m4recv")
              , ("m5send", "m5recv")
              , ("m6send", "m6recv")
              ]
              messagePassingArrowOpts False
  where
    m1send = (20, "m1send", "$1$", Just (0, -10), mkCircle)
    m1recv = (310, "m1recv", "$10$", Just (0, 10), mkCircle)
    m2send = (40, "m2send", "$1$", Just (0, 10), mkCircle)
    m2recv = (60, "m2recv", "$2$", Just (0, -10), mkCircle)
    m3send = (80, "m3send", "$3$", Just (0, -10), mkCircle)
    m3recv = (100, "m3recv", "$4$", Just (0, 10), mkCircle)
    m4send = (120, "m4send", "$5$", Just (0, 10), mkCircle)
    m4recv = (140, "m4recv", "$6$", Just (0, -10), mkCircle)
    m5send = (210, "m5send", "$7$", Just (0, -10), mkCircle)
    m5recv = (230, "m5recv", "$8$", Just (0, 10), mkCircle)
    m6send = (250, "m6send", "$9$", Just (0, 10), mkCircle)
    m6recv = (270, "m6recv", "$10$", Just (0, -10), mkCircle)

messagePassingExample3 :: Diagram B
messagePassingExample3 =
     mkWorlds [ ("$P_1$", 70, [], [ m1send, m2send, m3recv])
              , ("$P_2$", 10, [], [ m2recv, m3send, m4send])
              , ("$P_3$", -50, [], [ m4recv, m1recv ])
              ]
              [ ("m1send", "m1recv")
              , ("m2send", "m2recv")
              , ("m3send", "m3recv")
              , ("m4send", "m4recv")
              , ("m5send", "m5recv")
              , ("m6send", "m6recv")
              ]
              messagePassingArrowOpts False
  where
    m1send = (30, "m1send", "$m_1^\\textrm{send}$", Just (-5, 10), mkCircle)
    m1recv = (340, "m1recv", "$m_1^\\textrm{recv}$", Just (0, -10), mkCircle)
    m2send = (40, "m2send", "$m_2^\\textrm{send}$", Just (5, 10), mkCircle)
    m2recv = (60, "m2recv", "$m_2^\\textrm{recv}$", Just (0, -10), mkCircle)
    m3send = (105, "m3send", "$m_3^\\textrm{send}$", Just (-5, -10), mkCircle)
    m3recv = (135, "m3recv", "$m_3^\\textrm{recv}$", Just (0, 10), mkCircle)
    m4send = (115, "m4send", "$m_4^\\textrm{send}$", Just (10, 10), mkCircle)
    m4recv = (140, "m4recv", "$m_4^\\textrm{recv}$", Just (0, -10), mkCircle)

messagePassingExampleScalar3 :: Diagram B
messagePassingExampleScalar3 = pad 1.1 $
     mkWorlds [ ("$P_1$", 70, [], [ m1send, m2send, m3recv])
              , ("$P_2$", 10, [], [ m2recv, m3send, m4send])
              , ("$P_3$", -50, [], [ m4recv, m1recv ])
              ]
              [ ("m1send", "m1recv")
              , ("m2send", "m2recv")
              , ("m3send", "m3recv")
              , ("m4send", "m4recv")
              , ("m5send", "m5recv")
              , ("m6send", "m6recv")
              ]
              messagePassingArrowOpts False
  where
    m1send = (30, "m1send", "$1$", Just (-5, 10), mkCircle)
    m1recv = (340, "m1recv", "$7$", Just (0, -10), mkCircle)
    m2send = (40, "m2send", "$2$", Just (5, 10), mkCircle)
    m2recv = (60, "m2recv", "$3$", Just (0, -10), mkCircle)
    m3send = (105, "m3send", "$4$", Just (-5, -10), mkCircle)
    m3recv = (135, "m3recv", "$5$", Just (0, 10), mkCircle)
    m4send = (115, "m4send", "$5$", Just (10, 10), mkCircle)
    m4recv = (140, "m4recv", "$6$", Just (0, -10), mkCircle)


{-
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
-}


main :: IO ()
main = multiMain $
  [ ("messagePassingEx", messagePassingExample)
  , ("messagePassingExScalar", messagePassingExampleScalar)
  , ("messagePassingEx2", messagePassingExample2)
  , ("messagePassingExScalar2", messagePassingExampleScalar2)
  , ("messagePassingEx3", messagePassingExample3)
  , ("messagePassingExScalar3", messagePassingExampleScalar3)
  ]
  {-
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
-}
