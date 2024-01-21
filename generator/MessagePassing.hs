module MessagePassing
  ( mpEx1
  , mpEx1Sc
  , mpEx1Vec
  , mpEx2
  , mpEx2Sc
  , mpEx2Vec
  , mpEx3
  , mpEx3Sc
  , mpEx3Vec
  )
where

import Backend
import Lib
import Diagrams.Prelude

_WORLDLINE_GAP :: Double
_WORLDLINE_GAP = 60

p1h = _WORLDLINE_GAP
p2h = 0 :: Double
p3h = - _WORLDLINE_GAP

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

eventCircle :: Diagram B
eventCircle  = circle 3 # fc myBlue # lw 1 # lc black

messagePassingArrowOpts :: ArrowOpts Double
messagePassingArrowOpts = with & arrowHead  .~ dart
       & arrowTail .~ noTail
       & headLength .~ normal
       & tailLength .~ normal
       & headGap    .~ small
       & shaftStyle %~ lwL 1

-- Some offsets for simple labels
slightlyUp :: Maybe (Double, Double)
slightlyUp = Just (0, 10)

slightlyDown :: Maybe (Double, Double)
slightlyDown = Just (0, -10)

-- Slightly adjust an event to account for the large size of a vector
scootForVector :: EventSpec -> EventSpec
scootForVector spec =
  spec { lbloffset = offsetnew, lblsize = Just (15, 35) }
  where
    amt = 10
    Just (xoff, yoff) = lbloffset spec
    y_new = if yoff > 0 then yoff + amt else yoff - amt
    offsetnew = Just (xoff, y_new)


-- Create a basic event from some data
simpleEvent :: String -- ^ Name
            -> Double -- ^ Start time
            -> Maybe (Double, Double) -- ^ Label offset
            -> String -- ^ Label text
            -> EventSpec
simpleEvent name start lbloff lbl =
  EventSpec eventCircle name (math lbl) start lbloff Nothing

---- Example 1
ex1m1send = simpleEvent "m1s" 30 slightlyDown
ex1m2send = simpleEvent "m2s" 50 slightlyUp
ex1m1recv = simpleEvent "m1r" 95 slightlyUp
ex1m3send = simpleEvent "m3s" 110 slightlyDown
ex1m4send = simpleEvent "m4s" 140 slightlyUp
ex1m4recv = simpleEvent "m4r" 195 (Just (6, 10))
ex1m5send = simpleEvent "m5s" 245 (Just (5, 10))
ex1m2recv = simpleEvent "m2r" 260 slightlyDown
ex1m3recv = simpleEvent "m3r" 315 slightlyUp
ex1m5recv = simpleEvent "m5r" 320 slightlyDown

ex1arrows =
    [ ("m1s", "m1r")
    , ("m4s", "m4r")
    , ("m3s", "m3r")
    , ("m5s", "m5r")
    , ("m2s", "m2r")
    ]

mpEx1 :: Diagram B
mpEx1 =
     mkWorlds [ (math "P_1", p1h, [], [m2send, m1recv, m4send, m3recv])
              , (math "P_2", p2h, [], [m1send, m4recv, m5send])
              , (math "P_3", p3h, [], [m3send, m2recv, m5recv])
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
     mkWorlds [ (math "P_1", p1h, [], [m2send, m1recv, m4send, m3recv])
              , (math "P_2", p2h, [], [m1send, m4recv, m5send])
              , (math "P_3", p3h, [], [m3send, m2recv, m5recv])
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
     mkWorlds [ (math "P_1", p1h, [], [m2send, m1recv, m4send, m3recv])
              , (math "P_2", p2h, [], [m1send, m4recv, m5send])
              , (math "P_3", p3h, [], [m3send, m2recv, m5recv])
              ]
              ex1arrows
              messagePassingArrowOpts
  where
    m1send = scootForVector . ex1m1send $ vector (0,1,0)
    m2send = scootForVector . ex1m2send $ vector (1,0,0)
    m1recv = scootForVector . ex1m1recv $ vector (2,1,0)
    m3send = scootForVector . ex1m3send $ vector (0,0,1)
    m4send = scootForVector . ex1m4send $ vector (3,1,0)
    m4recv = scootForVector . ex1m4recv $ vector (3,2,0)
    m5send = scootForVector . ex1m5send $ vector (3,3,0)
    m2recv = scootForVector . ex1m2recv $ vector (1,0,2)
    m3recv = scootForVector . ex1m3recv $ vector (4,1,1)
    m5recv = scootForVector . ex1m5recv $ vector (3,3,3)

---- Example 2
ex2m1send = simpleEvent "m1send" 20 slightlyDown
ex2m2send = simpleEvent "m2send" 40 slightlyUp
ex2m2recv = simpleEvent "m2recv" 60 slightlyDown
ex2m3send = simpleEvent "m3send" 80 slightlyDown
ex2m3recv = simpleEvent "m3recv" 100 slightlyUp
ex2m4send = simpleEvent "m4send" 120 slightlyUp
ex2m4recv = simpleEvent "m4recv" 140 slightlyDown
ex2m5send = simpleEvent "m5send" 210 slightlyDown
ex2m5recv = simpleEvent "m5recv" 230 slightlyUp
ex2m6send = simpleEvent "m6send" 250 slightlyUp
ex2m6recv = simpleEvent "m6recv" 270 slightlyDown
ex2m1recv = simpleEvent "m1recv" 310 slightlyUp

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
     mkWorlds [ (math "P_1", p1h, [], [ m2send, m3recv, m4send, m5recv, m6send, m1recv ])
              , (math "P_2", p2h, [], [ m2recv, m3send, m4recv, m5send, m6recv ])
              , (math "P_3", p3h, [], [ m1send ])
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
mpEx2Sc =
     mkWorlds [ (math "P_1", p1h, [], [ m2send, m3recv, m4send, m5recv, m6send, m1recv ])
              , (math "P_2", p2h, [], [ m2recv, m3send, m4recv, m5send, m6recv ])
              , (math "P_3", p3h, [], [ m1send ])
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
mpEx2Vec =
     mkWorlds [ (math "P_1", p1h, [], [ m2send, m3recv, m4send, m5recv, m6send, m1recv ])
              , (math "P_2", p2h, [], [ m2recv, m3send, m4recv, m5send, m6recv ])
              , (math "P_3", p3h, [], [ m1send ])
              ]
              ex2arrows
              messagePassingArrowOpts
  where
    m1send = scootForVector . ex2m1send $ vector (0,0,1)
    m2send = scootForVector . ex2m2send $ vector (1,0,0)
    m2recv = scootForVector . ex2m2recv $ vector (1,1,0)
    m3send = scootForVector . ex2m3send $ vector (1,2,0)
    m3recv = scootForVector . ex2m3recv $ vector (2,2,0)
    m4send = scootForVector . ex2m4send $ vector (3,2,0)
    m4recv = scootForVector . ex2m4recv $ vector (3,3,0)
    m5send = scootForVector . ex2m5send $ vector (3,4,0)
    m5recv = scootForVector . ex2m5recv $ vector (4,4,0)
    m6send = scootForVector . ex2m6send $ vector (5,4,0)
    m6recv = scootForVector . ex2m6recv $ vector (5,5,0)
    m1recv = scootForVector . ex2m1recv $ vector (5,4,1)

--- Example 3
ex3m1send  = simpleEvent "m1send" 30 slightlyUp
ex3m1recv3 = simpleEvent "m1recv3" 328 slightlyDown
ex3m1recv2 = simpleEvent "m1recv2" 60 slightlyDown
ex3m2send  = simpleEvent "m2send" 152 $ Just (-5, 10)
ex3m2recv1 = simpleEvent "m2recv1" 185 slightlyUp
ex3m2recv3 = simpleEvent "m2recv3" 165 slightlyDown

ex3Arrows :: [(String, String)]
ex3Arrows =
   [ ("m1send", "m1recv1")
   , ("m1send", "m1recv2")
   , ("m1send", "m1recv3")
   , ("m2send", "m2recv1")
   , ("m2send", "m2recv3")
   ]

mpEx3 :: Diagram B
mpEx3 =
     mkWorlds [ (math "P_1", p1h, [], [ m1send, m2recv1])
              , (math "P_2", p2h, [], [ m1recv2, m2send])
              , (math "P_3", p3h, [], [ m2recv3, m1recv3 ])
              ]
              ex3Arrows
              messagePassingArrowOpts
  where
    m1send  = ex3m1send $ send "m_1"
    m1recv2 = ex3m1recv2 $ recv "m_{1, 2}"
    m2send  = ex3m2send $ send "m_2"
    m2recv1 = ex3m2recv1 $ recv "m_{2, 1}"
    m2recv3 = ex3m2recv3 $ recv "m_{2, 3}"
    m1recv3 = ex3m1recv3 $ recv "m_{1, 3}"

mpEx3Sc :: Diagram B
mpEx3Sc =
     mkWorlds [ (math "P_1", p1h, [], [ m1send, m2recv1])
              , (math "P_2", p2h, [], [ m1recv2, m2send])
              , (math "P_3", p3h, [], [ m2recv3, m1recv3 ])
              ]
              ex3Arrows
              messagePassingArrowOpts
  where
    m1send  = ex3m1send "1"
    m1recv2 = ex3m1recv2 "2"
    m2send  = ex3m2send "3"
    m2recv1 = ex3m2recv1 "4"
    m2recv3 = ex3m2recv3 "4"
    m1recv3 = ex3m1recv3 "5"

mpEx3Vec :: Diagram B
mpEx3Vec =
     mkWorlds [ (math "P_1", p1h, [], [ m1send, m2recv1])
              , (math "P_2", p2h, [], [ m1recv2, m2send])
              , (math "P_3", p3h, [], [ m2recv3, m1recv3 ])
              ]
              ex3Arrows
              messagePassingArrowOpts
  where
    m1send  = scootForVector . ex3m1send $ vector (1, 0, 0)
    m1recv2 = scootForVector . ex3m1recv2 $ vector (1, 1, 0)
    m2send  = scootForVector . ex3m2send $ vector (1, 2, 0)
    m2recv1 = scootForVector . ex3m2recv1 $ vector (2, 2, 0)
    m2recv3 = scootForVector . ex3m2recv3 $ vector (1, 2, 1)
    m1recv3 = scootForVector . ex3m1recv3 $ vector (1, 2, 2)
