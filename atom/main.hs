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

-- JK FlipFlop
-- J = 1 | K = 0 | Q = 1 (Set)
-- J = 0 | K = 1 | Q = 0 (Reset)
-- J = 0 | K = 0 | Q = Q (Hold)
-- J = 1 | K = 0 | Q = not Q (Toggle)

jk :: Integer -> E Bool -> E Bool -> E Bool -> Atom (E Bool)
jk i j k c = atom (nodeName "JKFlipFlop" i) $ do
	q <- bool "q" False
	c_rise <- rise 0 c

	-- Set/Reset/Toggle only when clock is rising.
	atom "trigged" $ do
		cond c_rise
		q  <== ((not_ (value q)) &&. j) ||. ((not_ k) &&. (value q))

	return $ value q

--toggleFlipFlop :: Integer -> Atom (V Bool)
--toggleFlipFlop i = atom (nodeName "toggle" i) $ do
--	vdd <- bool "vdd" True
--	q <- jkFlipFlop i vdd vdd
--	return q

latchTest :: Atom ()
latchTest = atom "latchTest" $ do
	period 1000 $ exactPhase 0 $ atom "test" $ do
		b_out <- jk 0 (Const True) (Const True) (Const True)

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

type Bus = (E Bool, E Bool, E Bool, E Bool)

-- Counter implemented with JK FlipFlop (asynchronous)

asynchCounter :: Integer -> Atom Bus
asynchCounter i = atom (nodeName "asynch_counter" i) $ do

	c <- oscillator 0 False 1 0
	v0 <- jk 0 (Const True) (Const True) (not_ c)
	v1 <- jk 1 (Const True) (Const True) (not_ v0)
	v2 <- jk 2 (Const True) (Const True) (not_ v1)

	return $ (c, v0, v1, v2)

-- Counter implemented with JK FlipFlop (synchronous)

synchCounter :: Integer -> Atom Bus
synchCounter i = atom (nodeName "synch_counter" i) $ do

	c <- oscillator 0 False 1 0
	v0 <- jk 0 (Const True) (Const True) (not_ c)
	v1 <- jk 1 (Const True) (Const True) (not_ c &&. not_ v0)
	v2 <- jk 2 (Const True) (Const True) (not_ c &&. not_ v0 &&. not_ v1)

	return $ (c, v0, v1, v2)

printBus :: Integer -> Bus -> Atom ()
printBus i (v0, v1, v2, v3) = atom (nodeName "print_bus" i) $ do

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

test :: Atom ()
test = period 1000 $ exactPhase 0 $ atom "test_counter" $ do
	w <- asynchCounter 0
	w2 <- synchCounter 0
	--printBus 0 w
	printBus 1 w2
