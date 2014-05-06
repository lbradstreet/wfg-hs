module WfgStreamFusion (wfgFusion,nds)

where

import qualified Data.List.Stream as S
import Utils

boundPoint :: Point -> Point -> Point
boundPoint [] [] = []
boundPoint (b:bound) (p:point) = min b p : boundPoint bound point

-- assume that all functions expect a rectangular array of
-- mutually non-dominating points
dom :: Point -> Point -> Bool
dom p1 p2 = all (uncurry (>=)) (S.zip p1 p2)

dominated :: Point -> Points -> Bool
dominated p ps = any (`dom` p) ps

wfgFusion :: Objective -> Point -> Points -> Volume
wfgFusion _ref _bound []    = 0
wfgFusion ref bound ps = S.sum (S.map (exclhv ref bound) pPairedPs)
	where pPairedPs = S.zip ps (S.takeWhile (not . null) (S.iterate S.tail (S.tail ps))) ++ [(S.last ps, [])]


nds :: Points -> Points
nds ps = S.map fst (S.filter (\(p, pps) -> not (dominated p pps)) pointVsPoints) ++ [last s]
	where s = S.sort ps
              pointVsPoints = S.zip s (S.takeWhile (not . null) (S.iterate S.tail (S.tail s)))

exclhv :: Objective -> Point -> (Point, Points) -> Volume
exclhv ref _bound (p, []) = inclhv ref p
exclhv ref bound (p, ps) = inclhv ref p - wfgFusion ref newbound boundedFront
    where newbound = boundPoint bound p
          boundedFront = nds (S.map (boundPoint newbound) ps)

inclhv :: Objective -> Point -> Volume
-- need to use bound here
inclhv ref p = S.product (S.map (\o -> o - ref) p)
