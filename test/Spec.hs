import qualified Data.Graph.Inductive.Basic        as G (undir)
import qualified Data.Graph.Inductive.Graph        as G (LEdge, LNode, mkGraph)
import qualified Data.Graph.Inductive.Query.BFS    as G (esp)
import qualified Data.Graph.Inductive.PatriciaTree as G (Gr)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Utils (end, mkNodes, start)

main :: IO ()
main = defaultMain tests

mkGraph :: [G.LNode String] -> [G.LEdge String] -> G.Gr String String
mkGraph nodes edges = G.undir $ G.mkGraph nodes edges

tests :: TestTree
tests = testGroup "\nTests"
  [ testGroup "Graphs without shortest path"
      [ testCase "no edges" $ do
          let gr = mkGraph mkNodes []
          let sp = G.esp start end gr
          null sp @? "sp should be empty"
      , testCase "1 edge looping to same node" $ do
          let gr = mkGraph mkNodes [(start, start, "")]
          let sp = G.esp start end gr
          null sp @? "sp should be empty"
      , testCase "many edges looping to same node" $ do
          let gr = mkGraph
                      mkNodes
                      [ (start, start, "")
                      , (start, start, "")
                      , (start, start, "")
                      ]
          let sp = G.esp start end gr
          null sp @? "sp should be empty"
      , testCase "many edges but none reaching destination"
          $ do
                let gr = mkGraph
                            mkNodes
                            [(start, 24, ""), (24, 36, ""), (36, 10, "")]
                let sp = G.esp start end gr
                null sp @? "sp should be empty"
      , testCase "Graph with \"backwards\" arcs (cycles) terminates"
          $ do
                let gr = mkGraph
                            mkNodes
                            [ (start, 24, "")
                            , (24   , 36, "")
                            , (36   , 24, "")
                            , (24   , 15, "")
                            ]
                let sp = G.esp start end gr
                null sp @? "sp should be empty"
      ]
  , testGroup "Graphs with shortest path"
      [ testCase "Basic graph" $ do
          let gr = mkGraph mkNodes [(start, 24, ""), (24, end, "")]
          let sp = G.esp start end gr
          3 @=? length sp
      , testCase "Graph with 2 paths picks shortest one" $ do
          let gr = mkGraph
                      mkNodes
                      [ (start, 24 , "")
                      , (24   , end, "")
                      , (24   , 30 , "")
                      , (30   , end, "")
                      ]
          let sp = G.esp start end gr
          3 @=? length sp
      , testCase "Graph with 2 paths picks shortest one (paths scrambled)"
          $ do
                let gr = mkGraph
                            mkNodes
                            [ (24   , end, "")
                            , (30   , end, "")
                            , (24   , 30 , "")
                            , (start, 24 , "")
                            ]
                let sp = G.esp start end gr
                3 @=? length sp
      , testCase
              ("Graph with 2 paths and \"backwards\" arcs (cycles) terminates "
              ++ "and picks shortest one")
          $ do
                let gr = mkGraph
                            mkNodes
                            [ (start, 24 , "")
                            , (24   , 30 , "")
                            , (30   , 24 , "")
                            , (24   , end, "")
                            , (30   , end, "")
                            ]
                let sp = G.esp start end gr
                3 @=? length sp
      -- this test takes around 10s on my laptop
      , testCase
              ("Graph with 2.003.640 edges terminates in O(V+E) "
              ++ "and picks shortest one")
          $ do
                let n = 1414
                    nodes = (\n' ->(n', show n')) <$> [0..n]
                    edges = [ (a,b,"") | a <- [0..n], b <- [0..n] ]
                    -- or with random edges: edges <- mkEdges n (1415^2)
                    gr = mkGraph nodes edges
                    sp = G.esp 0 n gr
                not (null sp) @? "there should be a shortest path"
      ]
  ]
