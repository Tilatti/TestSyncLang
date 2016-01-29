{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

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

nodeName :: String -> Integer -> String
nodeName s i = s ++ (show i)

-- Basic counter

-- counter :: Integer -> Word64 -> V Bool -> Atom (E Word64)
-- counter i init reset = atom (nodeName "counter" i) $ do
-- 	cnt <- word64 "counter_value" 0
-- 	cnt <== (value cnt) + 1
-- 	atom "counter_reset" $ do
-- 		cond $ (value reset)
-- 		cnt <== Const 0
-- 	return (value cnt)

-- Oscillate with a period and a phase.
-- Return (q).

oscillator :: Integer -> Bool -> Word64 -> Word64 -> Atom (E Bool)
oscillator i init period phase = atom (nodeName "oscillator" i) $ do
	q <- bool "q" init
	cnt <- word64 "cnt" phase
	cnt <== (value cnt) + 1
	atom "oscillator_set" $ do
		cond $ (value cnt) ==. Const period
		q <== Not (value q)
		cnt <== Const 0
	return (value q)

-- Hold the input value while trig is not False.
-- Take an trigger (trig) and an input value (s), return (q).

latch :: Integer -> Bool -> E Bool -> E Bool -> Atom (E Bool)
latch i init c s = atom (nodeName "latch" i) $ do
	cond c
	q <- bool "q" init
	q <== s
	return (value q)

jkFlipFlop :: Integer -> E Bool -> E Bool -> E Bool -> Atom (E Bool)
jkFlipFlop i j k c = atom (nodeName "JKFlipFlop" i) $ do
	cond c
	q <- bool "q" False
	q  <== ((not_ (value q)) &&. j) ||. ((not_ k) &&. (value q))
	return $ value q

fall :: Integer -> E Bool -> Atom (E Bool)
fall i s = atom (nodeName "isFall" i) $ do
		last <- bool "last" True
		last <== s
		return $ (value last) &&. (not_ s)

rise :: Integer -> E Bool -> Atom (E Bool)
rise i s = atom (nodeName "isRise" i) $ do
		last <- bool "last" False
		last <== s
		return $ not_ (value last) &&. s

--toggleFlipFlop :: Integer -> Atom (V Bool)
--toggleFlipFlop i = atom (nodeName "toggle" i) $ do
--	vdd <- bool "vdd" True
--	q <- jkFlipFlop i vdd vdd
--	return q

latchTest :: Atom ()
latchTest = atom "latchTest" $ do
	t <- bool "trig" True
	period 1000 $ exactPhase 0 $ atom "test" $ do
		-- b_out <- oscillator 1 False 4 0
		b_out <- jkFlipFlop 0 (Const True) (Const True) (Const True)

		is_rise <- rise 0 b_out

		atom "b_rise" $ do
			cond $ is_rise
			printStrLn "RISE !"

		atom "chec_b_true" $ do
			cond $ b_out
			printStrLn "True"

		is_fall <- fall 0 b_out

		atom "b_fall" $ do
		 	cond $ is_fall
		 	printStrLn "FALL !"

		atom "chec_b_false" $ do
			cond $ Not b_out
			printStrLn "False"

-- TODO comment

printEach :: Integer -> String -> Word64 -> Word64 -> Atom ()
printEach i s p ph = atom (nodeName "printEach" i) $ do
	c <- oscillator 1 False p ph
	atom "print" $ do
		cond c
		printStrLn s

counter :: Atom ()
counter = atom "asynch_counter" $ do

	c <- oscillator 0 False 1 0
	v0 <- jkFlipFlop 0 (Const True) (Const True) c
	n_v0 <- atom "f0" $ oneShotFall v0

	v1 <- jkFlipFlop 1 (Const True) (Const True) n_v0
	n_v1 <- atom "f1" $ oneShotFall v1

	v2 <- jkFlipFlop 2 (Const True) (Const True) n_v1
	n_v2 <- atom "f2" $ oneShotFall v2

	v3 <- jkFlipFlop 3 (Const True) (Const True) n_v2

	--return $ (foldl (.|.) [(value v0) (.<<.) 3, (value v1) (.<<.) 2, (value v2) (.<<.) 1, value v3])
	atom "v0" $ do
		cond $ v0
		printStrLn "0: 1"

	atom "nv0" $ do
		cond $ Not v0
		printStrLn "0: 0"

	atom "v1" $ do
		cond $ v1
		printStrLn "1: 1"

	atom "nv1" $ do
		cond $ Not v1
		printStrLn "1: 0"

	atom "v2" $ do
		cond $ v2
		printStrLn "2: 1"

	atom "nv2" $ do
		cond $ Not v2
		printStrLn "2: 0"

	atom "v3" $ do
		cond $ v3
		printStrLn "3: 1"

	atom "nv3" $ do
		cond $ Not v3
		printStrLn "3: 0"

	printStrLn "===="

test :: Atom ()
--test = period 500 $ exactPhase 0 $ atom "test" $ printEach 0 "Hello world !" 1 0
test = period 1000 $ exactPhase 0 $ atom "test_counter" $ latchTest
	--v <- counter_
	--printStrLn ""
