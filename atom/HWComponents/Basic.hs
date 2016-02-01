module HWComponents.Basic
(
	oscillator,
	fall,
	rise,
) where

import Language.Atom
import Language.Atom.Unit
import GHC.Word

import HWComponents.Common

-- Oscillate with a period and a phase.

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
