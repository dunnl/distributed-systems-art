module SharedMemory where
{-
  ( external1
  , external2
  , externalDAG
  , linearEx1
  , linearEx11
  , linearEx12
  , linearEx13
  , linearEx14
  , linearEx15
  )
where

import Backend
import Lib
import Diagrams.Prelude

memoryArrowOpts :: ArrowOpts Double
memoryArrowOpts = with & arrowHead  .~ dart
       & arrowTail .~ noTail
       & headLength .~ normal
       & tailLength .~ normal
       & headGap    .~ small
       & shaftStyle %~ dashingN [0.015,0.015] 0 # lwL 1

linearizationPoint :: Diagram B
linearizationPoint =
  rect 5 3 # lw 0 # translateY 2 # fc myHiYellow

p1h = 40
p2h = 0

--- External order
p11op = math "W(x, 4)"
p12op = math "R(y)"
p13op = math "W(y, 2)"

p21op = math "R(x)"
p22op = math "W(x, 1)"
p23op = math "R(x)"

p31op = math "R(y)"
p32op = math "W(x, 3)"
p33op = math "R(x)"

p11 = ("P11", p11op, 55, 20)
p12 = ("P12", p12op, 105, 45)
p13 = ("P13", p13op, 180, 103)
p21 = ("P21", p21op, 10, 25)
p22 = ("P22", p22op, 119, 24)
p23 = ("P23", p23op, 240, 53)
p31 = ("P31", p31op, 112, 20)
p32 = ("P32", p32op, 200, 23)
p33 = ("P33", p33op, 260, 50)

externalEdges :: [(Name, Name)]
externalEdges =
  [ ("P11" .> "stop", "P22" .> "start")
  , ("P12" .> "stop", "P32" .> "start")
  , ("P21" .> "stop", "P31" .> "start")
  , ("P22" .> "stop", "P13" .> "start")
  , ("P22" .> "stop", "P32" .> "start")
  , ("P21" .> "stop", "P11" .> "start")
  , ("P32" .> "stop", "P23" .> "start")
  , ("P31" .> "stop", "P13" .> "start")
  ]

external1 =
    mkWorlds [ (math "P_1", 80, [p11, p12, p13], [])
             , (math "P_2", 40, [p21, p22, p23], [])
             , (math "P_3",  0, [p31, p32, p33], [])
             ]
             ([] :: [(String,String)])
    memoryArrowOpts

external2 =
    mkWorlds [ (math "P_1", 80, [p11, p12, p13], [])
             , (math "P_2", 40, [p21, p22, p23], [])
             , (math "P_3",  0, [p31, p32, p33], [])
             ]
             externalEdges
  memoryArrowOpts

mkNode :: String -> String -> Double -> Double -> Diagram B
mkNode name lbl x y =
  (rect 40 20 <> text lbl # fontSizeL 5)
  # named name
  # lw 1
  # lc myHiBlue
  # translate (r2 (x, y))

externalDAG =
 (mkNode "p21" p21op 0 0 <>
  mkNode "p11" p11op incr 40 <>
  mkNode "p12" p12op (incr * 2) 40 <>
  mkNode "p22" p22op (incr * 2) 0 <>
  mkNode "p31" p31op (incr * 2) (-40) <>
  mkNode "p32" p32op (incr * 3) (-40) <>
  mkNode "p13" p13op (incr * 3.5) 40 <>
  mkNode "p23" p23op (incr * 4) 0 <>
  mkNode "p33" p33op (incr * 4) (-40))
  # attachAllOutside' memoryArrowOpts
  [ (toName "p21", toName "p11")
  , (toName "p11", toName "p12")
  , (toName "p11", toName "p31")
  , (toName "p11", toName "p22")
  , (toName "p21", toName "p22")
  , (toName "p21", toName "p31")
  , (toName "p12", toName "p13")
  , (toName "p12", toName "p32")
  , (toName "p22", toName "p13")
  , (toName "p22", toName "p32")
  , (toName "p31", toName "p13")
  , (toName "p31", toName "p32")
  , (toName "p32", toName "p23")
  , (toName "p32", toName "p33")
  ]
 where incr = _WORLDLENGTH/4

op11 = ("P11", math "W(x, 1)", 35, 63)
op12 Nothing = ("P23", math "R(x)", 200, 33)
op12 (Just x) = ("P23", math "R(x, " ++ x ++ ")", 200, 33)
op21 = ("P11", math "W(x, 2)", 10, 52)
op22 Nothing = ("P12", math "R(x)", 160, 63)
op22 (Just x) = ("P12", math "R(x, " ++ x ++ ")", 160, 63)

linearEx1 =
     mkWorlds [ (math "P_1", p1h, [op11, op12 Nothing], [])
              , (math "P_2", p2h, [op21, op22 Nothing], [])
              ]
              ([] :: [(String,String)])
              def

linearEx11 =
     mkWorlds [ (math "P_1", p1h, [op11, op12 (Just "2")], [linearpt1, linearpt2])
              , (math "P_2", p2h, [op21, op22 (Just "2")], [linearpt3, linearpt4])
              ]
              ([] :: [(String,String)])
              def
  where
      linearpt1 = ("", "", 45, Nothing, linearizationPoint)
      linearpt2 = ("", "", 215, Nothing, linearizationPoint)
      linearpt3 = ("", "", 57, Nothing, linearizationPoint)
      linearpt4 = ("", "", 180, Nothing, linearizationPoint)

linearEx12 =
     mkWorlds [ (math "P_1", p1h, [op11, op12 (Just "1")], [linearpt1, linearpt2])
              , (math "P_2", p2h, [op21, op22 (Just "1")], [linearpt3, linearpt4])
              ]
              ([] :: [(String,String)])
              def
  where
      linearpt1 = ("", "", 90, Nothing, linearizationPoint)
      linearpt2 = ("", "", 215, Nothing, linearizationPoint)
      linearpt3 = ("", "", 25, Nothing, linearizationPoint)
      linearpt4 = ("", "", 180, Nothing, linearizationPoint)

linearEx13 =
     mkWorlds [ (math "P_1", p1h, [op11, op12 (Just "1")], [])
              , (math "P_2", p2h, [op21, op22 (Just "2")], [])
              ]
              ([] :: [(String,String)])
              def
op11' = ("P11", math "W(x, 1)", 150, 63)
op12' Nothing = ("P23", math "R(x)", 220, 33)
op12' (Just x) = ("P23", math "R(x, " ++ x ++ ")", 220, 33)
op21' = ("P11", math "W(x, 2)", 10, 52)
op22' Nothing = ("P12", math "R(x)", 70, 63)
op22' (Just x) = ("P12", math "R(x, " ++ x ++ ")", 70, 63)

linearEx14 =
     mkWorlds [ (math "P_1", p1h, [op11', op12' (Just "1")], [])
              , (math "P_2", p2h, [op21', op22' (Just "2")], [])
              ]
              ([] :: [(String,String)])
              def

linearEx15 =
     mkWorlds [ (math "P_1", p1h, [op11, op12 (Just "2")], [])
              , (math "P_2", p2h, [op21, op22 (Just "1")], [])
              ]
              ([] :: [(String,String)])
              def
-}
