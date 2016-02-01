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

import qualified HWComponents.Counter as HWCounter
import qualified HWComponents.Bus as HWBus
import qualified HWComponents.Basic as HWBasic
import HWComponents.Common

execute :: Opts.Flag -> IO ()
execute Opts.Help = do
	print "Usage : TODO"
execute Opts.Version = do
	print ""
execute (Opts.Generate prefixName) = do
	--(sched, _, _, _, _) <- compile prefixName atomCfg counter
	(sched, _, _, _, _) <- compile prefixName atomCfg test
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

		#define BUFF_SIZE 32
		size_t buff_size;
		uint8_t buff[BUFF_SIZE];
	|],
	[embedStr|
		/* TODO: has to be replaced by a pselect call. */
		int main(void) {
			size_t bs;
			while (true) {
			#if 0
				bs = read(1, buff, BUFF_SIZE);
				if (bs > 0) {
					buff_size = bs;
					io_step();
				}
				#endif
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



-- Basic counter

-- counter :: Integer -> Word64 -> V Bool -> Atom (E Word64)
-- counter i init reset = atom (nodeName "counter" i) $ do
-- 	cnt <- word64 "counter_value" 0
-- 	cnt <== (value cnt) + 1
-- 	atom "counter_reset" $ do
-- 		cond $ (value reset)
-- 		cnt <== Const 0
-- 	return (value cnt)

-- TODO comment

printEach :: Integer -> String -> Word64 -> Word64 -> Atom ()
printEach i s p ph = atom (nodeName "printEach" i) $ do
	c <- HWBasic.oscillator 1 False p ph
	atom "print" $ do
		cond c
		printStrLn s

test :: Atom ()
test = period 1000 $ exactPhase 0 $ atom "test_counter" $ do
	w <- HWCounter.asynchCounter 0
	w2 <- HWCounter.synchCounter 0
	--printBus 0 w
	HWBus.printBus 1 w2
