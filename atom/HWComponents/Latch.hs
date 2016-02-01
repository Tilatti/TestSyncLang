{-# LANGUAGE QuasiQuotes #-}

module HWComponents.Latch
(
	latch,
	jk	
) where

import Language.Atom
import Language.Atom.Unit

import HWComponents.Common
import HWComponents.Basic

-- Hold the input value while trig is not False.
-- Take an trigger (trig) and an input value (s), return (q).

latch :: Integer -> Bool -> E Bool -> E Bool -> Atom (E Bool)
latch i init c s = atom (nodeName "latch" i) $ do
	cond c
	q <- bool "q" init
	q <== s
	return (value q)

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
