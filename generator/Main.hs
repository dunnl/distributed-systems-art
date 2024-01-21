module Main (main) where

import Backend
import Lib
import MessagePassing
import SharedMemory
import Diagrams.Prelude

main :: IO ()
main = multiMain $
  [ ("mpEx1", mpEx1)
  , ("mpEx1Sc", mpEx1Sc)
  , ("mpEx1Vec", mpEx1Vec)
  , ("mpEx2", mpEx2)
  , ("mpEx2Sc", mpEx2Sc)
  , ("mpEx2Vec", mpEx2Vec)
  , ("mpEx3", mpEx3)
  , ("mpEx3Sc", mpEx3Sc)
  , ("mpEx3Vec", mpEx3Vec)
  ]
{-
  , ("external1", external1)
  , ("external2", external2)
  , ("externalDAG", externalDAG)
  , ("linearEx1", linearEx1)
  , ("linearEx11", linearEx11)
  , ("linearEx12", linearEx12)
  , ("linearEx13", linearEx13)
  , ("linearEx14", linearEx14)
  , ("linearEx15", linearEx15)
  ]
-}
