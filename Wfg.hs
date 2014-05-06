module Wfg (wfg,nds)

where

import qualified Data.List.Stream as S
import Utils


boundPoint :: Point -> Point -> Point
boundPoint [] [] = []
boundPoint (b:bound) (p:point) = min b p : boundPoint bound point

data Dominated = XbeatsY | YbeatsX | NeitherDominates deriving (Enum, Eq)


dom :: Dominated -> Point -> Point -> Dominated
dom NeitherDominates [] [] = XbeatsY
dom domsofar [] [] = domsofar

dom NeitherDominates (x:xx) (y:yy) | x > y = dom XbeatsY xx yy
		                   | y > x = dom YbeatsX xx yy
				   | otherwise = dom NeitherDominates xx yy

dom XbeatsY (x:xx) (y:yy) | x >= y = dom XbeatsY xx yy
            	          | y > x = NeitherDominates

dom YbeatsX (x:xx) (y:yy) | y >= x = dom YbeatsX xx yy
			  | x > y = NeitherDominates


filterDominated :: Point -> Points -> (Maybe Point, Points)
filterDominated dp []     = (Just dp, [])
filterDominated dp (p:ps) | domResult == XbeatsY = filterDominated dp ps
		          | domResult == YbeatsX = (Nothing, (p:ps))
		          | otherwise = (\(x, xx) -> (x, (p : xx))) (filterDominated dp ps)
			   where domResult = dom NeitherDominates dp p

nds :: Points -> Points
nds [] = []
nds (p : ps) = dropP (filterDominated p ps)
	     where dropP (Nothing, xs) = nds xs
		   dropP (Just x, xs) = x : nds xs

wfg :: Objective -> Point -> Points -> Volume
wfg ref bound ps = wfg' ref bound (S.sort ps)

wfg' :: Objective -> Point -> Points -> Volume
wfg' _ref _bound []    = 0
wfg' ref bound (p:ps) = exclhv ref bound p ps + wfg' ref bound ps

exclhv :: Objective -> Point -> Point -> Points -> Volume
exclhv ref _bound p [] = inclhv ref p
exclhv ref bound p ps = inclhv ref p - wfg' ref newbound (S.sort nonDomFront)
	              where newbound = boundPoint bound p
                            boundedFront = (S.map (boundPoint newbound) ps)
          		    nonDomFront = nds boundedFront

inclhv :: Objective -> Point -> Volume
inclhv ref p = S.product [x - ref | x <- p]
