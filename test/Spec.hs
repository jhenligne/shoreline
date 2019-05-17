import qualified Data.Graph.Inductive.Graph        as G (labNodes, mkGraph)
import qualified Data.Graph.Inductive.Query.BFS    as G (esp)
import qualified Data.Graph.Inductive.PatriciaTree as G (Gr)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Utils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "\nTests"
  [ testGroup "Graphs without shortest path"
      [ testCase "no edges" $ do
          let gr = G.mkGraph mkNodes [] :: G.Gr String String
          let sp = G.esp start end gr
          null sp @? "sp should be empty"
      , testCase "1 edge looping to same node" $ do
          let gr =
                  G.mkGraph mkNodes [(start, start, "")] :: G.Gr String String
          let sp = G.esp start end gr
          null sp @? "sp should be empty"
      , testCase "many edges looping to same node" $ do
          let gr =
                  G.mkGraph
                      mkNodes
                      [ (start, start, "")
                      , (start, start, "")
                      , (start, start, "")
                      ] :: G.Gr String String
          let sp = G.esp start end gr
          null sp @? "sp should be empty"
      , testCase "many edges but none reaching destination"
          $ do
                let
                    gr =
                        G.mkGraph
                            mkNodes
                            [
                              (start, 24, ""), (24, 36, ""), (36, 10, "")
                            ] :: G.Gr String String
                let sp = G.esp start end gr
                null sp @? "sp should be empty"
      , testCase "Graph with \"backwards\" arcs (cycles) terminates"
          $ do
                let gr =
                        G.mkGraph
                            mkNodes
                            [ (start, 24, "")
                            , (24   , 36, "")
                            , (36   , 24, "")
                            , (24   , 15, "")
                            ] :: G.Gr String String
                let sp = G.esp start end gr
                null sp @? "sp should be empty"
      ]
  , testGroup "Graphs with shortest path"
      [ testCase "Basic graph" $ do
          let
              gr =
                  G.mkGraph mkNodes
                  [(start, 24, ""), (24, end, "")] :: G.Gr String String
          let sp = G.esp start end gr
          3 @=? length sp
      , testCase "Graph with 2 paths picks shortest one" $ do
          let gr =
                  G.mkGraph
                      mkNodes
                      [ (start, 24 , "")
                      , (24   , end, "")
                      , (24   , 30 , "")
                      , (30   , end, "")
                      ] :: G.Gr String String
          let sp = G.esp start end gr
          3 @=? length sp
      , testCase "Graph with 2 paths picks shortest one (paths scrambled)"
          $ do
                let gr =
                        G.mkGraph
                            mkNodes
                            [ (24   , end, "")
                            , (30   , end, "")
                            , (24   , 30 , "")
                            , (start, 24 , "")
                            ] :: G.Gr String String
                let sp = G.esp start end gr
                3 @=? length sp
      , testCase
              ("Graph with 2 paths and \"backwards\" arcs (cycles) terminates "
              ++ "and picks shortest one")
          $ do
                let gr =
                        G.mkGraph
                            mkNodes
                            [ (start, 24 , "")
                            , (24   , 30 , "")
                            , (30   , 24 , "")
                            , (24   , end, "")
                            , (30   , end, "")
                            ] :: G.Gr String String
                let sp = G.esp start end gr
                3 @=? length sp
      -- this test takes around 10s on my laptop
      , testCase
              ("Graph with 2.000.000 edges terminates quickly "
              ++ "and picks shortest one")
          $ do
                edges <- mkEdges 2000000
                let gr = G.mkGraph mkNodes edges :: G.Gr String String
                let sp = G.esp start end gr
                not (null sp) @? "there should be a shortest path"
      ]
  ]
