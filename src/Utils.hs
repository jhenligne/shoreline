module Utils
    ( start
    , end
    , names
    , mkNodes
    , mkEdges
    , mkGraph
    , mkPng
    ) where

import           Control.Monad (void)
import qualified Data.Graph.Inductive.Graph        as G (LEdge, LNode
                                                       , mkGraph, Path)
import qualified Data.Graph.Inductive.Query.BFS    as G (esp)
import qualified Data.Graph.Inductive.PatriciaTree as G (Gr)
import qualified Data.GraphViz                     as GV
import           System.Random

-- Setup
start :: Int
start = 0

end :: Int
end = length names - 1

names :: [String] -- Random names generated from listofrandomnames.com
names = 
    [ "Kaci Sunde", "Layne Barcus", "Donny Stokely", "Treena Cheek"
    , "Astrid Mai" , "Tawna Hoaglin", "Chan Showers", "Bong Reeve"
    , "Ja Solem", "Rudy Engberg", "Sadie Repka", "Kellee Wax"
    , "Dierdre Crochet", "Dagny Trenholm", "Viviana Milner", "Anjelica Mccall"
    , "Jefferson Stroble", "Danille Lamb", "Elia Cote", "Oralia Mccuin"
    , "Sid Blakeslee", "Antonetta Kensey", "Rossie Stermer", "Lai Brisker"
    , "Niesha Brueggeman", "Danilo Bigler", "Janette Weddle", "Angelena Dye"
    , "Suzette Alder", "Elwanda Vallo", "Gaynelle Colosimo", "Vickie Iwamoto"
    , "Oren Delnero", "Juli Merlos", "Evangeline Iacovelli", "Jolyn Granda"
    , "Benedict Jahn", "Cassidy Lamkin", "Tawanna Dahlberg", "Cinderella Ayotte"
    , "Julee Mazer", "Sherise Riddell", "Isaias Sartor", "Josefa Chase"
    , "Lucio Syring", "Delma Kubala", "Lisbeth Biery", "Ericka Davenport"
    , "Tyra Vaugh", "Chieko Sondag"]

{- utilities:
     pre-defined nodes,
     random n edges
     random graph,
     graph saved as a png
-}
mkNodes :: [G.LNode String]
mkNodes = zip [start..end] names

mkEdges :: Int -> IO [G.LEdge String]
mkEdges n = do
  seed1 <- newStdGen
  seed2 <- newStdGen
  return $ zipWith (\a b -> (a,b,"")) (mkInts seed1) (mkInts seed2)
  where
    mkInts seed = take n $ randomRs (start,end) seed

mkGraph :: IO (G.Gr String String)
mkGraph = do
  edges <- mkEdges 100
  return $ G.mkGraph mkNodes edges

mkPng :: G.Gr String String -> G.Path -> IO ()
mkPng gr sp = do
  let dot = GV.graphToDot (customParams sp) gr
  void $ GV.runGraphviz dot GV.Png "graph.png"
  putStrLn "graph.png file generated"
  where
    customParams sp =
      GV.nonClusteredParams {
        GV.fmtNode = \(i, l)
          -> [ GV.style GV.filled
             , GV.fillColor $ getNodeColor i sp
             , GV.toLabel l ],
        GV.fmtEdge = \(from, to, _)
          -> [GV.style GV.filled, GV.color $ getEdgeColor from to sp]
      }
    getNodeColor i sp
      | i == start || i == end = GV.Green
      | i `elem` sp            = GV.Orange
      | otherwise              = GV.Transparent
    getEdgeColor from to sp
      | from `elem` sp && to `elem` sp  = GV.Orange
      | otherwise                       = GV.Black
