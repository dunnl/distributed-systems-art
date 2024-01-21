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
  , ("external1", external1)
  , ("external2", external2)
  , ("externalDAG", externalDAG)
  ]
