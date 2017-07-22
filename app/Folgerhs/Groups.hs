module Folgerhs.Groups (groupGraph) where

import Data.List ((\\), intercalate, intersect)
import Data.Sequence (fromList)
import Data.Text.Lazy (unpack, pack)

import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types (printDotGraph)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete

import Folgerhs.Stage
import Folgerhs.Parse (parse)
import Folgerhs.Display (displayCharacter)

isSingle :: Character -> Bool
isSingle = not . null . intersect lowercase . takeWhile (/= '.') . displayCharacter
    where lowercase = ['a'..'z']

nodeName :: Line -> Character -> String
nodeName l c = displayCharacter c ++ "_" ++ l

clusterName :: Line -> [Character] -> GraphID
clusterName l cs = let name = intercalate "_" (l : map displayCharacter cs)
                    in Str (pack name)

edges :: [Stage] -> Character -> [DotEdge String]
edges ss c = [ DotEdge (nodeName fl c) (nodeName tl c) []
             | (fl, tl) <- zip (Folgerhs.Stage.lines ss) (tail $ Folgerhs.Stage.lines ss)
             , fl /= tl
             ]

cluster :: Line -> [Character] -> DotSubGraph String
cluster l cs = DotSG { isCluster = True
                     , subGraphID = Just $ clusterName l cs
                     , subGraphStmts = fromList 
                        [ DN $ DotNode (nodeName l c) [] | c <- cs ]
                     }

clusters :: [Stage] -> [Character] -> [DotSubGraph String]
clusters ss cs = let (ls, _, css) = unzip3 ss
                     gss = groups css
                  in [ cluster l (g `intersect` cs)
                     | (l, gs) <- zip ls gss
                     , g <- gs
                     , not $ null g ]

graph :: [Stage] -> [Character] -> DotGraph String
graph ss cs = DotGraph { strictGraph = True
                      , directedGraph = True
                      , graphID = Nothing
                      , graphStatements = fromList $
                              global_attrs
                           ++ (map SG (clusters ss cs))
                           ++ (map DE $ concatMap (edges ss) (cs))
                      }
    where global_attrs = [ GA $ GraphAttrs [ Pack DoPack
                                           , PackMode PackClust
                                           , RankDir FromTop
                                           ]
                         , GA $ NodeAttrs [ toLabel ""
                                          , style invis
                                          , shape PointShape
                                          , Width 0
                                          , Height 0
                                          , NodeSep 0.02
                                          ]
                         , GA $ EdgeAttrs [ edgeEnds NoDir
                                          , penWidth 5
                                          ]
                         ]


groupGraph :: FilePath -> IO ()
groupGraph f = do source <- readFile f
                  let ss = perLine $ parse source
                  let cs = filter isSingle $ characters ss
                  putStrLn $ unpack $ printDotGraph $ graph ss cs
                  return ()
