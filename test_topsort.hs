{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
import Data.List (elemIndex)
import qualified Data.Set as Set

import Test.QuickCheck
import Test.QuickCheck.All

import Topsort


instance Arbitrary (Dep Int) where
    arbitrary = do
        a <- choose (0,15) :: Gen Int
        b <- choose (0,15) :: Gen Int
        return (Dep a b)
                

prop_edges_can_be_compared_for_equality:: Dep Int -> Dep Int -> Bool
prop_edges_can_be_compared_for_equality a b
    | a==b = a1==b1 && a2==b2
    | a/=b = a1/=b1 || a2/=b2
    where
        (Dep a1 a2) = a
        (Dep b1 b2) = b
                
prop_edgelist_can_be_shown:: [Dep Int] -> Bool
prop_edgelist_can_be_shown a = length (show a) > 0

prop_topsort_isordered:: [Dep Int] -> Property
prop_topsort_isordered g =
    notHasCycle g ==>
    all (isOrdered result) g
    where Just result = topsort g

prop_topsort_no_duplicates:: [Dep Int] -> Property
prop_topsort_no_duplicates g =
    notHasCycle g ==>
    length result == (length . Set.toList . Set.fromList . nodes)  g
    where Just result = topsort g


prop_topsort_all_nodes_present:: [Dep Int] -> Property
prop_topsort_all_nodes_present g =
    notHasCycle g ==>
    all (`elem` result) (nodes g)
    where Just result = topsort g


prop_topsort_cycles_return_nothing:: [Dep Int] -> Property
prop_topsort_cycles_return_nothing g =
    not (notHasCycle g) ==>
    topsort g == Nothing


prop_topsort_levels_isordered:: [Dep Int] -> Property
prop_topsort_levels_isordered g =
    notHasCycle g ==>
    all (isOrdered (concat result)) g
    where Just result = topsort_levels g



prop_topsort_levels_all_nodes_present:: [Dep Int] -> Property
prop_topsort_levels_all_nodes_present g =
    notHasCycle g ==>
    all (`elem` (concat result)) (nodes g)
    where Just result = topsort_levels g


prop_topsort_levels_relative_independence:: [Dep Int] -> Property
prop_topsort_levels_relative_independence g =
    notHasCycle g ==>
    allGroupsIndep result
    where
        Just result = topsort_levels g
        allGroupsIndep = all isCoindependent
        isCoindependent ns = all (isIndependent ns) ns
        isIndependent ns n = all (\x-> x `notElem` ns && x/=n) (dependent n)
        dependent n = [a | (Dep a b) <- g, b==n]
    

prop_topsort_levels_cycles_return_nothing:: [Dep Int] -> Property
prop_topsort_levels_cycles_return_nothing g =
    hasCycle g ==>
    topsort_levels g == Nothing

prop_topsort_levels_no_duplicates:: [Dep Int] -> Property
prop_topsort_levels_no_duplicates g =
    notHasCycle g ==>
    length (concat result) == (length . Set.toList . Set.fromList . nodes) g
    where Just result = topsort_levels g


-- Utilities

nodes g = [a | (Dep a b) <- g] ++ [b | (Dep a b) <- g]

isOrdered sorted (Dep a b) = idxB < idxA
    where
        idxA = elemIndex a sorted
        idxB = elemIndex b sorted


notHasCycle = not . hasCycle

hasCycle [] = False
hasCycle g = any (cycle' []) (nodes g)
    where
        cycle' visited n
            | n `elem` visited     = True
            | null (reachable n)   = False
            | otherwise            = any (cycle' (n:visited)) (reachable n)
        reachable n = [b | (Dep a b) <- g, a==n]


main = $(quickCheckAll)
