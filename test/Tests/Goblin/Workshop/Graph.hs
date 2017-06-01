module Tests.Goblin.Workshop.Graph where

import Data.Map
import Test.Tasty
import Test.Tasty.HUnit
import Goblin.Workshop.Graph

tests :: TestTree
tests = testGroup "Workshop.Graph"
  [ testCase "Test initGraph" testInitGraph
  , testCase "Test entryPoints" testEntryPoints
  , testCase "Test removeEntryPoints" testRemoveEntryPoints
  ]

sampleData :: Graph Int String
sampleData = let kvps = [ (000, "Alpha")
                        , (111, "Beta")
                        , (222, "Gamma")
                        , (333, "Delta")
                        , (444, "Epsilon")
                        ]
                 conns = [ (000, 111)
                         , (111, 222)
                         , (111, 333)
                         , (222, 444)
                         ]
             in initGraph kvps conns


testInitGraph :: Assertion
testInitGraph = do
  vertices sampleData ! 222 @?= "Gamma"
  outEdgesMap sampleData ! 111 @?= [222, 333]
  inEdgesMap sampleData ! 111 @?= [000]
  outEdgesMap sampleData ! 000 @?= [111]
  inEdgesMap sampleData ! 000 @?= []
  outEdgesMap sampleData ! 444 @?= []
  inEdgesMap sampleData ! 444 @?= [222]

testEntryPoints :: Assertion
testEntryPoints = do
  entryPoints sampleData @?= [ (000, "Alpha")
                             ]

testRemoveEntryPoints :: Assertion
testRemoveEntryPoints = do
  let g = removeEntryPoints [444] sampleData
  g @?= sampleData
  let g' = removeEntryPoints [000, 222] sampleData
  g' @?= initGraph
    [ (111, "Beta")
    , (222, "Gamma")
    , (333, "Delta")
    , (444, "Epsilon")
    ]
    [ (111, 222)
    , (111, 333)
    , (222, 444)
    ]
  let empty = removeEntryPoint 444 . removeEntryPoints [222, 333] . removeEntryPoint 111 . removeEntryPoint 000 $ sampleData
  empty @?= initGraph [] []
