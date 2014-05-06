module Main

where

import System.Environment
import Utils
import Wfg
import WfgStreamFusion

main :: IO()
main = do args <- getArgs
          if length args < 2
           then putStrLn "More arguments please"
           else do cs   <- readFile (head args)
                   let pss = parsePoints cs
                   mapM_ (print . alg (args !! 1) (read (args !! 2))) pss
                   where alg "wfg" ref ps  = wfg ref (boundingPoint (length (head ps))) ps
		         alg "wfgFusion" ref ps  = wfgFusion ref (boundingPoint (length (head ps))) ps
                         boundingPoint nobjectives = replicate nobjectives 10000.0

