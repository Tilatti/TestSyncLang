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

-- JK FlipFlop

jkFlipFlop :: Integer -> E Bool -> E Bool -> E Bool -> Atom (E Bool)
jkFlipFlop i j k c = atom (nodeName "JKFlipFlop" i) $ do
	cond c
	q <- bool "q" False
	q  <== ((not_ (value q)) &&. j) ||. ((not_ k) &&. (value q))
	return $ value q

-- Return True on falling edge.

fall :: Integer -> E Bool -> Atom (E Bool)
fall i s = atom (nodeName "is_falling" i) $ do
		q <- bool "q" False
		last <- bool "last" True
		q <== (value last) &&. (not_ s)
		last <== s
		return $ (value q)

-- Return True on raising edge.

rise :: Integer -> E Bool -> Atom (E Bool)
rise i s = atom (nodeName "is_rising" i) $ do
		q <- bool "q" False
		last <- bool "last" False
		q <== (not_ (value last)) &&. s
		last <== s
		return $ (value q)

--toggleFlipFlop :: Integer -> Atom (V Bool)
--toggleFlipFlop i = atom (nodeName "toggle" i) $ do
--	vdd <- bool "vdd" True
--	q <- jkFlipFlop i vdd vdd
--	return q

latchTest :: Atom ()
latchTest = atom "latchTest" $ do
	period 1000 $ exactPhase 0 $ atom "test" $ do
		b_out <- jkFlipFlop 0 (Const True) (Const True) (Const True)

		is_rising <- rise 0 b_out

		atom "b_rise" $ do
			cond $ is_rising
			printStrLn "RISE !"
		atom "check_b_true" $ do
			cond $ b_out
			printStrLn "True"

		is_falling <- fall 0 b_out

		atom "b_fall" $ do
		 	cond $ is_falling
		 	printStrLn "FALL !"
		atom "check_b_false" $ do
			cond $ Not b_out
			printStrLn "False"

-- TODO comment

printEach :: Integer -> String -> Word64 -> Word64 -> Atom ()
printEach i s p ph = atom (nodeName "printEach" i) $ do
	c <- oscillator 1 False p ph
	atom "print" $ do
		cond c
		printStrLn s

counter :: Atom (E Word64)
counter = atom "asynch_counter" $ do

	c <- oscillator 0 False 1 0
	n_c <- fall 3 c

	v0 <- jkFlipFlop 0 (Const True) (Const True) n_c
	n_v0 <- fall 0 v0 

	v1 <- jkFlipFlop 1 (Const True) (Const True) n_v0
	n_v1 <- fall 1 v1

	v2 <- jkFlipFlop 2 (Const True) (Const True) n_v1
	n_v2 <- fall 2 v2

	return $ (v2 .<<. 3) .|. (v1 .<<. 2) .|. (v0 .<<. 1) .|. c

test :: Atom ()
--test = period 500 $ exactPhase 0 $ atom "test" $ printEach 0 "Hello world !" 1 0
test = period 1000 $ exactPhase 0 $ atom "test_counter" $ do
	w <- counter
	printStrLn (show w)
	--v <- counter_
	--printStrLn ""
