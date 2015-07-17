{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, BangPatterns, StandaloneDeriving,
             MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define DIGITS       9
#define BASE         1000000000
#elif SIZEOF_HSWORD == 8
#define DIGITS       18
#define BASE         1000000000000000000
#else
#error Please define DIGITS and BASE
#endif
module GHC.Show
        (
        Show(..), ShowS,
        shows, showChar, showString, showMultiLineString,
        showParen, showList__, showSpace,
        showLitChar, showLitString, protectEsc,
        intToDigit, showSignedInt,
        appPrec, appPrec1,
        asciiTab,
  )
        where
import GHC.Base
import GHC.Num
import GHC.List ((!!), foldr1, break)
type ShowS = String -> String
class  Show a  where
    {-# MINIMAL showsPrec | show #-}
    showsPrec :: Int    
              -> a      
              -> ShowS
    show      :: a   -> String
    showList  :: [a] -> ShowS
    showsPrec _ x s = show x ++ s
    show x          = shows x ""
    showList ls   s = showList__ shows ls s
showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)
appPrec, appPrec1 :: Int
appPrec = I# 10#        
appPrec1 = I# 11#       
deriving instance Show ()
instance Show a => Show [a]  where
  {-# SPECIALISE instance Show [String] #-}
  {-# SPECIALISE instance Show [Char] #-}
  {-# SPECIALISE instance Show [Int] #-}
  showsPrec _         = showList
deriving instance Show Bool
deriving instance Show Ordering
instance  Show Char  where
    showsPrec _ '\'' = showString "'\\''"
    showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''
    showList cs = showChar '"' . showLitString cs . showChar '"'
instance Show Int where
    showsPrec = showSignedInt
instance Show Word where
    showsPrec _ (W# w) = showWord w
showWord :: Word# -> ShowS
showWord w# cs
 | isTrue# (w# `ltWord#` 10##) = C# (chr# (ord# '0'# +# word2Int# w#)) : cs
 | otherwise = case chr# (ord# '0'# +# word2Int# (w# `remWord#` 10##)) of
               c# ->
                   showWord (w# `quotWord#` 10##) (C# c# : cs)
deriving instance Show a => Show (Maybe a)
instance  (Show a, Show b) => Show (a,b)  where
  showsPrec _ (a,b) s = show_tuple [shows a, shows b] s
instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a,b,c) s = show_tuple [shows a, shows b, shows c] s
instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a,b,c,d) s = show_tuple [shows a, shows b, shows c, shows d] s
instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showsPrec _ (a,b,c,d,e) s = show_tuple [shows a, shows b, shows c, shows d, shows e] s
instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a,b,c,d,e,f) where
  showsPrec _ (a,b,c,d,e,f) s = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f] s
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
        => Show (a,b,c,d,e,f,g) where
  showsPrec _ (a,b,c,d,e,f,g) s
        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g] s
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
         => Show (a,b,c,d,e,f,g,h) where
  showsPrec _ (a,b,c,d,e,f,g,h) s
        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h] s
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i)
         => Show (a,b,c,d,e,f,g,h,i) where
  showsPrec _ (a,b,c,d,e,f,g,h,i) s
        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i] s
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j)
         => Show (a,b,c,d,e,f,g,h,i,j) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j) s
        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j] s
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k)
         => Show (a,b,c,d,e,f,g,h,i,j,k) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k) s
        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k] s
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
          Show l)
         => Show (a,b,c,d,e,f,g,h,i,j,k,l) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l) s
        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k, shows l] s
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
          Show l, Show m)
         => Show (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m) s
        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k, shows l, shows m] s
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
          Show l, Show m, Show n)
         => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n) s
        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k, shows l, shows m, shows n] s
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
          Show l, Show m, Show n, Show o)
         => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) s
        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k, shows l, shows m, shows n, shows o] s
show_tuple :: [ShowS] -> ShowS
show_tuple ss = showChar '('
              . foldr1 (\s r -> s . showChar ',' . r) ss
              . showChar ')'
shows           :: (Show a) => a -> ShowS
shows           =  showsPrec 0
showChar        :: Char -> ShowS
showChar        =  (:)
showString      :: String -> ShowS
showString      =  (++)
showParen       :: Bool -> ShowS -> ShowS
showParen b p   =  if b then showChar '(' . p . showChar ')' else p
showSpace :: ShowS
showSpace =  \ xs -> ' ' : xs
showLitChar                :: Char -> ShowS
showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDec (shows (ord c)) s)
showLitChar '\DEL'         s =  showString "\\DEL" s
showLitChar '\\'           s =  showString "\\\\" s
showLitChar c s | c >= ' '   =  showChar c s
showLitChar '\a'           s =  showString "\\a" s
showLitChar '\b'           s =  showString "\\b" s
showLitChar '\f'           s =  showString "\\f" s
showLitChar '\n'           s =  showString "\\n" s
showLitChar '\r'           s =  showString "\\r" s
showLitChar '\t'           s =  showString "\\t" s
showLitChar '\v'           s =  showString "\\v" s
showLitChar '\SO'          s =  protectEsc (== 'H') (showString "\\SO") s
showLitChar c              s =  showString ('\\' : asciiTab!!ord c) s
showLitString :: String -> ShowS
showLitString []         s = s
showLitString ('"' : cs) s = showString "\\\"" (showLitString cs s)
showLitString (c   : cs) s = showLitChar c (showLitString cs s)
showMultiLineString :: String -> [String]
showMultiLineString str
  = go '\"' str
  where
    go ch s = case break (== '\n') s of
                (l, _:s'@(_:_)) -> (ch : showLitString l "\\n\\") : go '\\' s'
                (l, "\n")       -> [ch : showLitString l "\\n\""]
                (l, _)          -> [ch : showLitString l "\""]
isDec :: Char -> Bool
isDec c = c >= '0' && c <= '9'
protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f             = f . cont
                             where cont s@(c:_) | p c = "\\&" ++ s
                                   cont s             = s
asciiTab :: [String]
asciiTab = 
           ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"]
intToDigit :: Int -> Char
intToDigit (I# i)
    | isTrue# (i >=# 0#)  && isTrue# (i <=#  9#) = unsafeChr (ord '0' + I# i)
    | isTrue# (i >=# 10#) && isTrue# (i <=# 15#) = unsafeChr (ord 'a' + I# i - 10)
    | otherwise =  error ("Char.intToDigit: not a digit " ++ show (I# i))
showSignedInt :: Int -> Int -> ShowS
showSignedInt (I# p) (I# n) r
    | isTrue# (n <# 0#) && isTrue# (p ># 6#) = '(' : itos n (')' : r)
    | otherwise                              = itos n r
itos :: Int# -> String -> String
itos n# cs
    | isTrue# (n# <# 0#) =
        let !(I# minInt#) = minInt in
        if isTrue# (n# ==# minInt#)
           then '-' : (case n# `quotRemInt#` 10# of
                       (# q, r #) ->
                           itos' (negateInt# q) (itos' (negateInt# r) cs))
           else '-' : itos' (negateInt# n#) cs
    | otherwise = itos' n# cs
    where
    itos' :: Int# -> String -> String
    itos' x# cs'
        | isTrue# (x# <# 10#) = C# (chr# (ord# '0'# +# x#)) : cs'
        | otherwise = case x# `quotRemInt#` 10# of
                      (# q, r #) ->
                          case chr# (ord# '0'# +# r) of
                          c# ->
                              itos' q (C# c# : cs')
instance Show Integer where
    showsPrec p n r
        | p > 6 && n < 0 = '(' : integerToString n (')' : r)
        | otherwise = integerToString n r
    showList = showList__ (showsPrec 0)
integerToString :: Integer -> String -> String
integerToString n0 cs0
    | n0 < 0    = '-' : integerToString' (- n0) cs0
    | otherwise = integerToString' n0 cs0
    where
    integerToString' :: Integer -> String -> String
    integerToString' n cs
        | n < BASE  = jhead (fromInteger n) cs
        | otherwise = jprinth (jsplitf (BASE*BASE) n) cs
    jsplitf :: Integer -> Integer -> [Integer]
    jsplitf p n
        | p > n     = [n]
        | otherwise = jsplith p (jsplitf (p*p) n)
    jsplith :: Integer -> [Integer] -> [Integer]
    jsplith p (n:ns) =
        case n `quotRemInteger` p of
        (# q, r #) ->
            if q > 0 then q : r : jsplitb p ns
                     else     r : jsplitb p ns
    jsplith _ [] = error "jsplith: []"
    jsplitb :: Integer -> [Integer] -> [Integer]
    jsplitb _ []     = []
    jsplitb p (n:ns) = case n `quotRemInteger` p of
                       (# q, r #) ->
                           q : r : jsplitb p ns
    jprinth :: [Integer] -> String -> String
    jprinth (n:ns) cs =
        case n `quotRemInteger` BASE of
        (# q', r' #) ->
            let q = fromInteger q'
                r = fromInteger r'
            in if q > 0 then jhead q $ jblock r $ jprintb ns cs
                        else jhead r $ jprintb ns cs
    jprinth [] _ = error "jprinth []"
    jprintb :: [Integer] -> String -> String
    jprintb []     cs = cs
    jprintb (n:ns) cs = case n `quotRemInteger` BASE of
                        (# q', r' #) ->
                            let q = fromInteger q'
                                r = fromInteger r'
                            in jblock q $ jblock r $ jprintb ns cs
    jhead :: Int -> String -> String
    jhead n cs
        | n < 10    = case unsafeChr (ord '0' + n) of
            c@(C# _) -> c : cs
        | otherwise = case unsafeChr (ord '0' + r) of
            c@(C# _) -> jhead q (c : cs)
        where
        (q, r) = n `quotRemInt` 10
    jblock = jblock'  DIGITS
    jblock' :: Int -> Int -> String -> String
    jblock' d n cs
        | d == 1    = case unsafeChr (ord '0' + n) of
             c@(C# _) -> c : cs
        | otherwise = case unsafeChr (ord '0' + r) of
             c@(C# _) -> jblock' (d - 1) q (c : cs)
        where
        (q, r) = n `quotRemInt` 10
