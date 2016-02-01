{-# LANGUAGE QuasiQuotes #-}

module HWComponents.Counter
(
	asynchCounter,
	synchCounter
) where

import Language.Atom
import Language.Atom.Unit
import GHC.Word

import HWComponents.Bus
import HWComponents.Common
import HWComponents.Basic
import HWComponents.Latch

-- Counter implemented with JK FlipFlop (asynchronous)

asynchCounter :: Integer -> Atom Bus
asynchCounter i = atom (nodeName "asynch_counter" i) $ do

	c <- oscillator 0 False 1 0
	v0 <- jk 0 (Const True) (Const True) (not_ c)
	v1 <- jk 1 (Const True) (Const True) (not_ v0)
	v2 <- jk 2 (Const True) (Const True) (not_ v1)

	return $ (v2, v1, v0, c)

-- Counter implemented with JK FlipFlop (synchronous)

synchCounter :: Integer -> Atom Bus
synchCounter i = atom (nodeName "synch_counter" i) $ do

	c <- oscillator 0 False 1 0
	v0 <- jk 0 (Const True) (Const True) (not_ c)
	v1 <- jk 1 (Const True) (Const True) (not_ c &&. not_ v0)
	v2 <- jk 2 (Const True) (Const True) (not_ c &&. not_ v0 &&. not_ v1)

	return $ (v2, v1, v0, c)
