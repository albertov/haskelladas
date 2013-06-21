{-# LANGUAGE BangPatterns #-}
module Topsort (
    topsort,
    topsort_levels,
    Dep (Dep)
) where

import Data.List (groupBy, nubBy)
import Data.Function (on)

data Dep a = Dep a a deriving (Show, Eq)


--
-- topsort
--

topsort ::  Eq a => [Dep a] -> Maybe [a]
topsort g = topsort' g (independents g) []


topsort' [] []    acc = Just . reverse $ acc

topsort' g  []    acc = Nothing -- has cycles

topsort' g  (n:s) acc = 
    let
        g'   = removeDepsToNode n g
        next = independentNodesExceptOn n g
        s'   = (undupeBy id) (s ++ next)
    in
        topsort' g' s' (n:acc)

--
-- topsort_levels
--

topsort_levels:: (Eq a) => [Dep a] -> Maybe [[a]]
topsort_levels g = topsort_levels' g (assignLevel 0 (independents g)) []


topsort_levels' [] [] acc =
    let
        groupByLevel = groupBy ((==) `on` fst)
        undecorate = map (map snd)
    in
        Just . reverse . undecorate . groupByLevel $ acc

topsort_levels' g [] acc  = Nothing -- has cycles

topsort_levels' g (n:s) acc =
    let
        g'        = removeDepsToNode n' g
        next      = independentNodesExceptOn n' g
        (l, n')   = n
        s'        = (undupeBy snd) (s ++ (assignLevel (l+1) next))
    in
        topsort_levels' g' s' (n:acc)

--
-- Utils
--
removeDepsToNode ::  Eq a => a -> [Dep a] -> [Dep a]
removeDepsToNode n = filter $ \(Dep _ m') -> m'/=n

independentNodesExceptOn ::  Eq t => t -> [Dep t] -> [t]
independentNodesExceptOn n g =
    let
        g' = removeDepsToNode n g
    in
        [m | (Dep m m') <- g, m'==n, isIndependent g' m]

dependents ::  [Dep b] -> [b]
dependents = map $ \(Dep a b) -> a

dependees ::  [Dep b] -> [b]
dependees = map  $ \(Dep a b) -> b

independents ::  Eq b => [Dep b] -> [b]
independents g = filter (isIndependent g) $ undupeBy id $ dependees g

isIndependent ::  Eq a => [Dep a] -> a -> Bool
isIndependent g x = notElem x $ dependents g

assignLevel ::  t -> [t1] -> [(t, t1)]
assignLevel l = map $ \n -> (l,n)

undupeBy ::  Eq b => (a -> b) -> [a] -> [a]
undupeBy f = nubBy ((==) `on` f)
