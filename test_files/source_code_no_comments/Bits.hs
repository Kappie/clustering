{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, BangPatterns, MagicHash #-}
module Data.Bits (
  Bits(
    (.&.), (.|.), xor,
    complement,
    shift,
    rotate,
    zeroBits,
    bit,
    setBit,
    clearBit,
    complementBit,
    testBit,
    bitSizeMaybe,
    bitSize,
    isSigned,
    shiftL, shiftR,
    unsafeShiftL, unsafeShiftR,
    rotateL, rotateR,
    popCount
  ),
  FiniteBits(
    finiteBitSize,
    countLeadingZeros,
    countTrailingZeros
  ),
  bitDefault,
  testBitDefault,
  popCountDefault,
  toIntegralSized
 ) where
#include "MachDeps.h"
#ifdef MIN_VERSION_integer_gmp
# define HAVE_INTEGER_GMP1 MIN_VERSION_integer_gmp(1,0,0)
#endif
import Data.Maybe
import GHC.Enum
import GHC.Num
import GHC.Base
import GHC.Real
#if HAVE_INTEGER_GMP1
import GHC.Integer.GMP.Internals (bitInteger, popCountInteger)
#endif
infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.
{-# DEPRECATED bitSize "Use 'bitSizeMaybe' or 'finiteBitSize' instead" #-} 
class Eq a => Bits a where
    {-# MINIMAL (.&.), (.|.), xor, complement,
                (shift | (shiftL, shiftR)),
                (rotate | (rotateL, rotateR)),
                bitSize, bitSizeMaybe, isSigned, testBit, bit, popCount #-}
    (.&.) :: a -> a -> a
    (.|.) :: a -> a -> a
    xor :: a -> a -> a
    complement        :: a -> a
    shift             :: a -> Int -> a
    x `shift`   i | i<0       = x `shiftR` (-i)
                  | i>0       = x `shiftL` i
                  | otherwise = x
    rotate            :: a -> Int -> a
    x `rotate`  i | i<0       = x `rotateR` (-i)
                  | i>0       = x `rotateL` i
                  | otherwise = x
    zeroBits :: a
    zeroBits = clearBit (bit 0) 0
    bit               :: Int -> a
    setBit            :: a -> Int -> a
    clearBit          :: a -> Int -> a
    complementBit     :: a -> Int -> a
    testBit           :: a -> Int -> Bool
    bitSizeMaybe      :: a -> Maybe Int
    bitSize           :: a -> Int
    isSigned          :: a -> Bool
    {-# INLINE setBit #-}
    {-# INLINE clearBit #-}
    {-# INLINE complementBit #-}
    x `setBit` i        = x .|. bit i
    x `clearBit` i      = x .&. complement (bit i)
    x `complementBit` i = x `xor` bit i
    shiftL            :: a -> Int -> a
    {-# INLINE shiftL #-}
    x `shiftL`  i = x `shift`  i
    unsafeShiftL            :: a -> Int -> a
    {-# INLINE unsafeShiftL #-}
    x `unsafeShiftL` i = x `shiftL` i
    shiftR            :: a -> Int -> a
    {-# INLINE shiftR #-}
    x `shiftR`  i = x `shift`  (-i)
    unsafeShiftR            :: a -> Int -> a
    {-# INLINE unsafeShiftR #-}
    x `unsafeShiftR` i = x `shiftR` i
    rotateL           :: a -> Int -> a
    {-# INLINE rotateL #-}
    x `rotateL` i = x `rotate` i
    rotateR           :: a -> Int -> a
    {-# INLINE rotateR #-}
    x `rotateR` i = x `rotate` (-i)
    popCount          :: a -> Int
class Bits b => FiniteBits b where
    finiteBitSize :: b -> Int
    countLeadingZeros :: b -> Int
    countLeadingZeros x = (w-1) - go (w-1)
      where
        go i | i < 0       = i 
             | testBit x i = i
             | otherwise   = go (i-1)
        w = finiteBitSize x
    countTrailingZeros :: b -> Int
    countTrailingZeros x = go 0
      where
        go i | i >= w      = i
             | testBit x i = i
             | otherwise   = go (i+1)
        w = finiteBitSize x
bitDefault :: (Bits a, Num a) => Int -> a
bitDefault = \i -> 1 `shiftL` i
{-# INLINE bitDefault #-}
testBitDefault ::  (Bits a, Num a) => a -> Int -> Bool
testBitDefault = \x i -> (x .&. bit i) /= 0
{-# INLINE testBitDefault #-}
popCountDefault :: (Bits a, Num a) => a -> Int
popCountDefault = go 0
 where
   go !c 0 = c
   go c w = go (c+1) (w .&. (w - 1)) 
{-# INLINABLE popCountDefault #-}
instance Bits Bool where
    (.&.) = (&&)
    (.|.) = (||)
    xor = (/=)
    complement = not
    shift x 0 = x
    shift _ _ = False
    rotate x _ = x
    bit 0 = True
    bit _ = False
    testBit x 0 = x
    testBit _ _ = False
    bitSizeMaybe _ = Just 1
    bitSize _ = 1
    isSigned _ = False
    popCount False = 0
    popCount True  = 1
instance FiniteBits Bool where
    finiteBitSize _ = 1
    countTrailingZeros x = if x then 0 else 1
    countLeadingZeros  x = if x then 0 else 1
instance Bits Int where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}
    zeroBits = 0
    bit     = bitDefault
    testBit = testBitDefault
    (I# x#) .&.   (I# y#)          = I# (x# `andI#` y#)
    (I# x#) .|.   (I# y#)          = I# (x# `orI#`  y#)
    (I# x#) `xor` (I# y#)          = I# (x# `xorI#` y#)
    complement (I# x#)             = I# (notI# x#)
    (I# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)      = I# (x# `iShiftL#` i#)
        | otherwise                = I# (x# `iShiftRA#` negateInt# i#)
    (I# x#) `shiftL` (I# i#)       = I# (x# `iShiftL#` i#)
    (I# x#) `unsafeShiftL` (I# i#) = I# (x# `uncheckedIShiftL#` i#)
    (I# x#) `shiftR` (I# i#)       = I# (x# `iShiftRA#` i#)
    (I# x#) `unsafeShiftR` (I# i#) = I# (x# `uncheckedIShiftRA#` i#)
    {-# INLINE rotate #-}       
    (I# x#) `rotate` (I# i#) =
        I# ((x# `uncheckedIShiftL#` i'#) `orI#` (x# `uncheckedIShiftRL#` (wsib -# i'#)))
      where
        !i'# = i# `andI#` (wsib -# 1#)
        !wsib = WORD_SIZE_IN_BITS#   
    bitSizeMaybe i         = Just (finiteBitSize i)
    bitSize i              = finiteBitSize i
    popCount (I# x#) = I# (word2Int# (popCnt# (int2Word# x#)))
    isSigned _             = True
instance FiniteBits Int where
    finiteBitSize _ = WORD_SIZE_IN_BITS
    countLeadingZeros  (I# x#) = I# (word2Int# (clz# (int2Word# x#)))
    countTrailingZeros (I# x#) = I# (word2Int# (ctz# (int2Word# x#)))
instance Bits Word where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}
    (W# x#) .&.   (W# y#)    = W# (x# `and#` y#)
    (W# x#) .|.   (W# y#)    = W# (x# `or#`  y#)
    (W# x#) `xor` (W# y#)    = W# (x# `xor#` y#)
    complement (W# x#)       = W# (x# `xor#` mb#)
        where !(W# mb#) = maxBound
    (W# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)      = W# (x# `shiftL#` i#)
        | otherwise                = W# (x# `shiftRL#` negateInt# i#)
    (W# x#) `shiftL` (I# i#)       = W# (x# `shiftL#` i#)
    (W# x#) `unsafeShiftL` (I# i#) = W# (x# `uncheckedShiftL#` i#)
    (W# x#) `shiftR` (I# i#)       = W# (x# `shiftRL#` i#)
    (W# x#) `unsafeShiftR` (I# i#) = W# (x# `uncheckedShiftRL#` i#)
    (W# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#) = W# x#
        | otherwise  = W# ((x# `uncheckedShiftL#` i'#) `or#` (x# `uncheckedShiftRL#` (wsib -# i'#)))
        where
        !i'# = i# `andI#` (wsib -# 1#)
        !wsib = WORD_SIZE_IN_BITS#  
    bitSizeMaybe i           = Just (finiteBitSize i)
    bitSize i                = finiteBitSize i
    isSigned _               = False
    popCount (W# x#)         = I# (word2Int# (popCnt# x#))
    bit                      = bitDefault
    testBit                  = testBitDefault
instance FiniteBits Word where
    finiteBitSize _ = WORD_SIZE_IN_BITS
    countLeadingZeros  (W# x#) = I# (word2Int# (clz# x#))
    countTrailingZeros (W# x#) = I# (word2Int# (ctz# x#))
instance Bits Integer where
   (.&.) = andInteger
   (.|.) = orInteger
   xor = xorInteger
   complement = complementInteger
   shift x i@(I# i#) | i >= 0    = shiftLInteger x i#
                     | otherwise = shiftRInteger x (negateInt# i#)
   shiftL x (I# i#) = shiftLInteger x i#
   shiftR x (I# i#) = shiftRInteger x i#
   testBit x (I# i) = testBitInteger x i
   zeroBits   = 0
#if HAVE_INTEGER_GMP1
   bit (I# i#) = bitInteger i#
   popCount x  = I# (popCountInteger x)
#else
   bit        = bitDefault
   popCount   = popCountDefault
#endif
   rotate x i = shift x i   
   bitSizeMaybe _ = Nothing
   bitSize _  = error "Data.Bits.bitSize(Integer)"
   isSigned _ = True
toIntegralSized :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
toIntegralSized x                 
  | maybe True (<= x) yMinBound
  , maybe True (x <=) yMaxBound = Just y
  | otherwise                   = Nothing
  where
    y = fromIntegral x
    xWidth = bitSizeMaybe x
    yWidth = bitSizeMaybe y
    yMinBound
      | isBitSubType x y = Nothing
      | isSigned x, not (isSigned y) = Just 0
      | isSigned x, isSigned y
      , Just yW <- yWidth = Just (negate $ bit (yW-1)) 
      | otherwise = Nothing
    yMaxBound
      | isBitSubType x y = Nothing
      | isSigned x, not (isSigned y)
      , Just xW <- xWidth, Just yW <- yWidth
      , xW <= yW+1 = Nothing 
      | Just yW <- yWidth = if isSigned y
                            then Just (bit (yW-1)-1)
                            else Just (bit yW-1)
      | otherwise = Nothing
{-# INLINEABLE toIntegralSized #-}
isBitSubType :: (Bits a, Bits b) => a -> b -> Bool
isBitSubType x y
  | xWidth == yWidth, xSigned == ySigned = True
  | ySigned, Nothing == yWidth                  = True
  | not xSigned, not ySigned, Nothing == yWidth = True
  | xSigned == ySigned,   Just xW <- xWidth, Just yW <- yWidth = xW <= yW
  | not xSigned, ySigned, Just xW <- xWidth, Just yW <- yWidth = xW <  yW
  | otherwise = False
  where
    xWidth  = bitSizeMaybe x
    xSigned = isSigned     x
    yWidth  = bitSizeMaybe y
    ySigned = isSigned     y
{-# INLINE isBitSubType #-}
