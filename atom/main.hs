{-# LANGUAGE QuasiQuotes #-}

module Main where

import Language.Atom
import Language.Atom.Unit
import GHC.Word

import System.Environment   
import System.Directory  
import System.IO
import Data.List  

import qualified Opts 
import StringEmbed

execute :: Opts.Flag -> IO ()
execute Opts.Help = do
	print "Usage : TODO"
execute Opts.Version = do
	print ""
execute (Opts.Generate prefixName) = do
	(sched, _, _, _, _) <- compile prefixName atomCfg counter
	putStrLn $ reportSchedule sched
execute Opts.PrintSched = do
	putStrLn ""

main = do
	args <- getArgs
	(flags, _) <- Opts.compileOpts args
	mapM_ execute flags

atomCfg :: Config
atomCfg = defaults { cFuncName = "io_step"
		, cStateName = "state_example"
		, cCode = prePostCode
		, hCode = prePostHeader
		, cRuleCoverage = False
}

prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode _ _ _ =
	([embedStr| 
		#include <stdlib.h>
		#include <stdio.h>
		#include <unistd.h>

	|],
	[embedStr|
		int main(void) {
			while (true) {
				io_step();
				usleep(1000);
			}
			return 0;
		}
	|])

prePostHeader :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostHeader _ _ _ =
	( [embedStr| |]
	, [embedStr| |])

-- This program print the "Hello world !" string each 2 seconds.

counter :: Atom ()
counter = atom "main" $ do
	period 1000 $ exactPhase 0 $ atom "second" $ do
		cnt <- word64 "counter" 0
		atom "incr" $ do incr cnt
		atom "check_cnt" $ do
			cond $ value cnt ==. Const 2
			cnt <== 0
			printStrLn "Hello world !"
