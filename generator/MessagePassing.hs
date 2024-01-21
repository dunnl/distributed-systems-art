module MessagePassing
  ( mpEx1
  , mpEx1Sc
  , mpEx1Vec
  , mpEx2
  , mpEx2Sc
  , mpEx2Vec
  )
where

import Backend
import Lib
import Diagrams.Prelude

send :: String -> String
send str =
    if _IN_NOTEBOOK then str ++ "-snd"
    else str ++ "^\\textrm{send}"

recv :: String -> String
recv str =
    if _IN_NOTEBOOK then str ++ "-rcv"
    else str ++ "^\\textrm{recv}"

vector :: (Int, Int, Int) -> String
vector (a, b, c) = if _IN_NOTEBOOK then show (a, b, c) else
    "\\begin{bmatrix}" ++ show a ++ "\\\\" ++ show b ++ "\\\\" ++ show c ++ "\\end{bmatrix}"

littleup = Just (0, 10)
littledown = Just (0, -10)

scoot :: EventSpec -> EventSpec
scoot (a, b, c, Just (x, y), diagram) =
  if y > 0 then (a, b, c, Just (x, y + amt), diagram)
  else (a, b, c, Just (x, y - amt), diagram)
  where amt = 10

messagePassingArrowOpts :: ArrowOpts Double
messagePassingArrowOpts = with & arrowHead  .~ dart
       & arrowTail .~ noTail
       & headLength .~ normal
       & tailLength .~ normal
       & headGap    .~ small
       & shaftStyle %~ lwL 1

eventCircle :: Diagram B
eventCircle  = circle 3 # fc myBlue # lw 1 # lc black

_WORLDLINE_GAP :: Double
_WORLDLINE_GAP = 60

p1h = _WORLDLINE_GAP
p2h = 0 :: Double
p3h = - _WORLDLINE_GAP

---- Example 1
ex1m1send lbl = ("m1s", math lbl, 30, littledown, eventCircle)
ex1m2send lbl = ("m2s", math lbl, 50, littleup, eventCircle)
ex1m1recv lbl = ("m1r", math lbl, 95, littleup, eventCircle)
ex1m3send lbl = ("m3s", math lbl, 110, littledown, eventCircle)
ex1m4send lbl = ("m4s", math lbl, 140, littleup, eventCircle)
ex1m4recv lbl = ("m4r", math lbl, 195, Just (6, 10), eventCircle)
ex1m5send lbl = ("m5s", math lbl, 245, Just (5, 10), eventCircle)
ex1m2recv lbl = ("m2r", math lbl, 260, littledown, eventCircle)
ex1m3recv lbl = ("m3r", math lbl, 315, littleup, eventCircle)
ex1m5recv lbl = ("m5r", math lbl, 320, littledown, eventCircle)

ex1arrows =
    [ ("m1s", "m1r")
    , ("m4s", "m4r")
    , ("m3s", "m3r")
    , ("m5s", "m5r")
    , ("m2s", "m2r")
    ]

mpEx1 :: Diagram B
mpEx1 =
     mkWorlds [ ("$P_1$", p1h, [], [m2send, m1recv, m4send, m3recv])
              , ("$P_2$", p2h, [], [m1send, m4recv, m5send])
              , ("$P_3$", p3h, [], [m3send, m2recv, m5recv])
              ]
              ex1arrows
              messagePassingArrowOpts
  where
    m1send = ex1m1send $ send "m_1"
    m2send = ex1m2send $ send "m_2"
    m1recv = ex1m1recv $ recv "m_1"
    m3send = ex1m3send $ send "m_3"
    m4send = ex1m4send $ send "m_4"
    m4recv = ex1m4recv $ recv "m_4"
    m5send = ex1m5send $ send "m_5"
    m2recv = ex1m2recv $ recv "m_2"
    m3recv = ex1m3recv $ recv "m_3"
    m5recv = ex1m5recv $ recv "m_5"

mpEx1Sc :: Diagram B
mpEx1Sc =
     mkWorlds [ ("$P_1$", p1h, [], [m2send, m1recv, m4send, m3recv])
              , ("$P_2$", p2h, [], [m1send, m4recv, m5send])
              , ("$P_3$", p3h, [], [m3send, m2recv, m5recv])
              ]
              ex1arrows
              messagePassingArrowOpts
  where
    m1send = ex1m1send "1"
    m2send = ex1m2send "1"
    m1recv = ex1m1recv "2"
    m3send = ex1m3send "1"
    m4send = ex1m4send "3"
    m4recv = ex1m4recv "4"
    m5send = ex1m5send "5"
    m2recv = ex1m2recv "2"
    m3recv = ex1m3recv "4"
    m5recv = ex1m5recv "6"

mpEx1Vec :: Diagram B
mpEx1Vec =
     mkWorlds [ ("$P_1$", p1h, [], [m2send, m1recv, m4send, m3recv])
              , ("$P_2$", p2h, [], [m1send, m4recv, m5send])
              , ("$P_3$", p3h, [], [m3send, m2recv, m5recv])
              ]
              ex1arrows
              messagePassingArrowOpts
  where
    m1send = scoot . ex1m1send $ vector (0,1,0)
    m2send = scoot . ex1m2send $ vector (1,0,0)
    m1recv = scoot . ex1m1recv $ vector (2,1,0)
    m3send = scoot . ex1m3send $ vector (0,0,1)
    m4send = scoot . ex1m4send $ vector (3,1,0)
    m4recv = scoot . ex1m4recv $ vector (3,2,0)
    m5send = scoot . ex1m5send $ vector (3,3,0)
    m2recv = scoot . ex1m2recv $ vector (1,0,2)
    m3recv = scoot . ex1m3recv $ vector (4,1,1)
    m5recv = scoot . ex1m5recv $ vector (3,3,3)

---- Example 2
ex2m1send lbl = ("m1send", math lbl, 20, littledown, eventCircle)
ex2m2send lbl = ("m2send", math lbl, 40, littleup, eventCircle)
ex2m2recv lbl = ("m2recv", math lbl, 60, littledown, eventCircle)
ex2m3send lbl = ("m3send", math lbl, 80, littledown, eventCircle)
ex2m3recv lbl = ("m3recv", math lbl, 100, littleup, eventCircle)
ex2m4send lbl = ("m4send", math lbl, 120, littleup, eventCircle)
ex2m4recv lbl = ("m4recv", math lbl, 140, littledown, eventCircle)
ex2m5send lbl = ("m5send", math lbl, 210, littledown, eventCircle)
ex2m5recv lbl = ("m5recv", math lbl, 230, littleup, eventCircle)
ex2m6send lbl = ("m6send", math lbl, 250, littleup, eventCircle)
ex2m6recv lbl = ("m6recv", math lbl, 270, littledown, eventCircle)
ex2m1recv lbl = ("m1recv", math lbl, 310, littleup, eventCircle)

ex2arrows =
    [ ("m1send", "m1recv")
    , ("m2send", "m2recv")
    , ("m3send", "m3recv")
    , ("m4send", "m4recv")
    , ("m5send", "m5recv")
    , ("m6send", "m6recv")
    ]

mpEx2 :: Diagram B
mpEx2 =
     mkWorlds [ ("$P_1$", p1h, [], [ m2send, m3recv, m4send, m5recv, m6send, m1recv ])
              , ("$P_2$", p2h, [], [ m2recv, m3send, m4recv, m5send, m6recv ])
              , ("$P_3$", p3h, [], [ m1send ])
              ]
              ex2arrows
              messagePassingArrowOpts
  where
    m1send = ex2m1send $ send "m_1"
    m2send = ex2m2send $ send "m_2"
    m2recv = ex2m2recv $ recv "m_2"
    m3send = ex2m3send $ send "m_3"
    m3recv = ex2m3recv $ recv "m_3"
    m4send = ex2m4send $ send "m_4"
    m4recv = ex2m4recv $ recv "m_4"
    m5send = ex2m5send $ send "m_5"
    m5recv = ex2m5recv $ recv "m_5"
    m6send = ex2m6send $ send "m_6"
    m6recv = ex2m6recv $ recv "m_6"
    m1recv = ex2m1recv $ recv "m_1"

mpEx2Sc :: Diagram B
mpEx2Sc = pad 1.1 $
     mkWorlds [ ("$P_1$", p1h, [], [ m2send, m3recv, m4send, m5recv, m6send, m1recv ])
              , ("$P_2$", p2h, [], [ m2recv, m3send, m4recv, m5send, m6recv ])
              , ("$P_3$", p3h, [], [ m1send ])
              ]
              ex2arrows
              messagePassingArrowOpts
  where
    m1send = ex2m1send "1"
    m2send = ex2m2send "1"
    m2recv = ex2m2recv "2"
    m3send = ex2m3send "3"
    m3recv = ex2m3recv "4"
    m4send = ex2m4send "5"
    m4recv = ex2m4recv "6"
    m5send = ex2m5send "7"
    m5recv = ex2m5recv "8"
    m6send = ex2m6send "9"
    m6recv = ex2m6recv "10"
    m1recv = ex2m1recv "10"

mpEx2Vec :: Diagram B
mpEx2Vec = pad 1.1 $
     mkWorlds [ ("$P_1$", p1h, [], [ m2send, m3recv, m4send, m5recv, m6send, m1recv ])
              , ("$P_2$", p2h, [], [ m2recv, m3send, m4recv, m5send, m6recv ])
              , ("$P_3$", p3h, [], [ m1send ])
              ]
              ex2arrows
              messagePassingArrowOpts
  where
    m1send = scoot . ex2m1send $ vector (0,0,1)
    m2send = scoot . ex2m2send $ vector (1,0,0)
    m2recv = scoot . ex2m2recv $ vector (1,1,0)
    m3send = scoot . ex2m3send $ vector (1,2,0)
    m3recv = scoot . ex2m3recv $ vector (2,2,0)
    m4send = scoot . ex2m4send $ vector (3,2,0)
    m4recv = scoot . ex2m4recv $ vector (3,3,0)
    m5send = scoot . ex2m5send $ vector (3,4,0)
    m5recv = scoot . ex2m5recv $ vector (4,4,0)
    m6send = scoot . ex2m6send $ vector (5,4,0)
    m6recv = scoot . ex2m6recv $ vector (5,5,0)
    m1recv = scoot . ex2m1recv $ vector (5,4,1)

--- Example 3
mpEx3 :: Diagram B
mpEx3 = pad 1.1 $
     mkWorlds [ ("$P_1$", 70, [], [ m1send, m2recv1])
              , ("$P_2$", 10, [], [ m1recv2, m2send])
              , ("$P_3$", -50, [], [ m2recv3, m1recv3 ])
              ]
              [ ("m1send", "m1recv1")
              , ("m1send", "m1recv2")
              , ("m1send", "m1recv3")
              , ("m2send", "m2recv1")
              , ("m2send", "m2recv3")
              ]
              messagePassingArrowOpts
  where
    m1send  = ("m1send", math $ send "m_1", 30, littleup, eventCircle)
    m1recv3 = ("m1recv3", math "m_1^\\textrm{recv,3}", 328, littledown, eventCircle)
    m1recv2 = ("m1recv2", "$m_1^\\textrm{recv,2}$", 60, littledown, eventCircle)
    m2send  = ("m2send", "$m_2^\\textrm{send}$", 152, Just (-5, -10), eventCircle)
    m2recv1 = ("m2recv1", "$m_2^\\textrm{recv,1}$", 185, littleup, eventCircle)
    m2recv3 = ("m2recv3", "$m_2^\\textrm{recv,3}$", 165, littledown, eventCircle)

mpEx3Sc :: Diagram B
mpEx3Sc = pad 1.1 $
     mkWorlds [ ("$P_1$", 70, [], [ m1send, m2recv1])
              , ("$P_2$", 10, [], [ m1recv2, m2send])
              , ("$P_3$", -50, [], [ m2recv3, m1recv3 ])
              ]
              [ ("m1send", "m1recv1")
              , ("m1send", "m1recv2")
              , ("m1send", "m1recv3")
              , ("m2send", "m2recv1")
              , ("m2send", "m2recv3")
              ]
              messagePassingArrowOpts
  where
    m1send  = ("m1send", "1", 30, littleup, eventCircle)
    m1recv3 = ("m1recv3", "5", 328, littledown, eventCircle)
    m1recv2 = ("m1recv2", "2", 60, littledown, eventCircle)
    m2send  = ("m2send", "3", 152, littledown, eventCircle)
    m2recv1 = ("m2recv1", "4", 185, littleup, eventCircle)
    m2recv3 = ("m2recv3", "4", 165, littledown, eventCircle)

