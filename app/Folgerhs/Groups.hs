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

edges :: Character -> [Line] -> [DotEdge String]
edges c ls = [ DotEdge (nodeName fl c) (nodeName tl c) []
             | (fl, tl) <- zip ls (tail ls), fl /= tl
             ]

cluster :: Line -> Group -> DotSubGraph String
cluster l cs = DotSG { isCluster = True
                     , subGraphID = Just $ clusterName l cs
                     , subGraphStmts = fromList 
                        [ DN $ DotNode (nodeName l c) [] | c <- cs ]
                     }

characterStatements :: Character -> [Stage] -> [DotStatement String]
characterStatements c ss = let j = journey c ss
                               (ls, _) = unzip j
                            in (map DE $ edges c ls) ++ map (SG . uncurry cluster) j


globalAttrs :: [DotStatement String]
globalAttrs = [ GA $ GraphAttrs [ Pack DoPack
                                 , PackMode PackClust
                                 , RankDir FromTop
                                 ]
               , GA $ NodeAttrs [ toLabel ""
                                , shape PointShape
                                , Width 0.02
                                , Height 0.02
                                , NodeSep 0.02
                                ]
               , GA $ EdgeAttrs [ edgeEnds NoDir
                                , penWidth 5
                                ]
               ]

graph :: [Character] -> [Stage] -> DotGraph String
graph cs ss = DotGraph { strictGraph = True
                       , directedGraph = True
                       , graphID = Nothing
                       , graphStatements = fromList $
                               globalAttrs
                            ++ concatMap (\c -> characterStatements c ss) cs
                       }


groupGraph :: FilePath -> IO ()
groupGraph f = do source <- readFile f
                  let ss = perLine $ parse source
                  let cs = characters ss
                  putStrLn $ unpack $ printDotGraph $ graph cs ss
                  return ()
