module Folgerhs.Groups (groupGraph) where

import Data.List ((\\))
import Data.Sequence (fromList)
import Data.Text.Lazy (unpack)

import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types (printDotGraph)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete

import Folgerhs.Stage
import Folgerhs.Parse (parse)

nodeName :: Line -> Character -> String
nodeName l c = l ++ "-" ++ c

edges :: [Stage] -> Character -> [DotEdge String]
edges ss c = [ DotEdge (nodeName fl c) (nodeName tl c) []
             | (fl, tl) <- zip (Folgerhs.Stage.lines ss) (tail $ Folgerhs.Stage.lines ss)
             , fl /= tl
             ]

cluster :: Line -> [Character] -> DotSubGraph String
cluster l cs = DotSG { isCluster = True
                     , subGraphID = Nothing
                     , subGraphStmts = fromList 
                        [ DN $ DotNode (nodeName l c) [] | c <- cs ]
                     }

clusters :: [Stage] -> [DotSubGraph String]
clusters ss = let (ls, _, css) = unzip3 ss
                  gss = groups css
               in [ cluster l g | (l, gs) <- zip ls gss
                                , g <- gs
                                , not $ null g ]

graph :: [Stage] -> DotGraph String
graph ss = DotGraph { strictGraph = True
                    , directedGraph = True
                    , graphID = Nothing
                    , graphStatements = fromList $
                            global_attrs
                         ++ (map SG (clusters ss))
                         ++ (map DE $ concatMap (edges ss) (take 3 $ characters ss))
                    }
    where global_attrs = [ GA $ GraphAttrs [ RankDir FromTop
                                           ]
                         , GA $ NodeAttrs [ toLabel ""
                                          , style invis
                                          , shape PointShape
                                          , Width 0
                                          , Height 0
                                          , NodeSep 0.02
                                          ]
                         , GA $ EdgeAttrs [ edgeEnds NoDir
                                          , penWidth 10
                                          ]
                         ]

groupGraph :: FilePath -> IO ()
groupGraph f = do source <- readFile f
                  let g = graph (take 1000 $ parse source)
                  putStrLn $ unpack $ printDotGraph g
                  return ()
