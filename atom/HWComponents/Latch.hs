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

-- RS FlipFlop

rs :: Integer -> E Bool -> E Bool -> E Bool -> Atom (E Bool)
rs i s r c = atom (nodeName "RSFlipFlop" i) $ do

	q <- bool "q" False
	c_rise <- rise 0 c

	-- Set/Reset/Hold only when clock is rising.
	atom "trigged" $ do
		cond c_rise
		q  <== (s &&. (not_ r)) ||. (((s &&. r) ||. (not_ s &&. not_ r)) &&. (value q))

	return $ value q

-- D FlipFlop

d :: Integer -> E Bool -> E Bool -> Atom (E Bool)
d i d h = atom (nodeName "DFlipFlop" i) $ do

	q <- bool "q" False

	-- Copy D input when H is high.
	atom "copy" $ do
		cond h
		q <== d

	return $ value q

-- JK FlipFlop
-- J = 1 | K = 0 | Q = 1 (Set)
-- J = 0 | K = 1 | Q = 0 (Reset)
-- J = 0 | K = 0 | Q = Q (Hold)
-- J = 1 | K = 1 | Q = not Q (Toggle)

jk :: Integer -> E Bool -> E Bool -> E Bool -> Atom (E Bool)
jk i j k c = atom (nodeName "JKFlipFlop" i) $ do
	q <- bool "q" False
	c_rise <- rise 0 c

	-- Set/Reset/Toggle/Hold only when clock is rising.
	atom "trigged" $ do
		cond c_rise
		q  <== ((not_ (value q)) &&. j) ||. ((not_ k) &&. (value q))

	return $ value q

--toggleFlipFlop :: Integer -> Atom (V Bool)
--toggleFlipFlop i = atom (nodeName "toggle" i) $ do
--	vdd <- bool "vdd" True
--	q <- jkFlipFlop i vdd vdd
--	return q
