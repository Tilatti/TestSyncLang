{-# LANGUAGE QuasiQuotes #-}

module HWComponents.Counter
(
	asynchCounter,
	asynchCounterN,
	synchCounter
) where

import Language.Atom
import Language.Atom.Unit
import GHC.Word

import HWComponents.Bus
import HWComponents.Common
import HWComponents.Basic
import HWComponents.Latch
import HWComponents.Bus

-- Counter

counter :: Integer -> Atom (E Word32)
counter i = atom (nodeName "counter" i) $ do
	q <- word32 "q" 0
	q <== (value q) + 1
	return $ value q

-- Counter implemented with JK FlipFlop (asynchronous)

asynchCounter :: Integer -> Atom (E Word32)
asynchCounter i = atom (nodeName "asynch_counter" i) $ do

	c <- oscillator 0 False 1 0
	v0 <- jk 0 (Const True) (Const True) (not_ c)
	v1 <- jk 1 (Const True) (Const True) (not_ v0)
	v2 <- jk 2 (Const True) (Const True) (not_ v1)

	return $ busToWord32 (v2, v1, v0, c)

-- Counter implemented with JK FlipFlop (synchronous)

synchCounter :: Integer -> Atom (E Word32)
synchCounter i = atom (nodeName "synch_counter" i) $ do

	c <- oscillator 0 False 1 0
	v0 <- jk 0 (Const True) (Const True) (not_ c)
	v1 <- jk 1 (Const True) (Const True) (not_ c &&. not_ v0)
	v2 <- jk 2 (Const True) (Const True) (not_ c &&. not_ v0 &&. not_ v1)

	return $ busToWord32 (v2, v1, v0, c)

-- Generic counter implemented with JK FlipFlop (asynchronous)
-- TODO: A cleaner implementation (with Control.Monad functions) ?

asynchCounterN :: Integer -> Word32 -> Atom (E Word32)
asynchCounterN i n
	| n < 8 = error "N is too low."
	| otherwise =
		atom (nodeName "asynch_counter_generic" i) $ do
			c <- oscillator 0 False 1 0
			r <- counterN 0 (n - 1) c
			return $ addToWord32 n r c
		where
			counterN :: Integer -> Word32 -> E Bool -> Atom (E Word32)
			counterN _ 0 _ = do
				return 0
			counterN y i c = do
				q <- jk y (Const True) (Const True) (not_ c)
				r <- counterN (y + 1) (i - 1) q
				return $ addToWord32 i r q
