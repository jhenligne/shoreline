module Main where

import           Control.Monad (void, when)
import qualified Data.Graph.Inductive.Query.BFS as G (esp)
import qualified Data.GraphViz                  as GV (isGraphvizInstalled)
import           Data.List (intercalate)

import           Utils

main :: IO ()
main = do
  gr <- mkGraph
  let sp = G.esp start end gr
  putStrLn $
    if null sp
      then "No shortest path found between Kaci Sunde and Chieko Sondag"
      else "shortest path between Kaci Sunde and Chieko Sondag: "
           ++ intercalate " -> " ((names !!) <$> sp)
  graphvizInstalled <- GV.isGraphvizInstalled
  when graphvizInstalled $ void (mkPng gr sp)
