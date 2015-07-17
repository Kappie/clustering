{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, PolyKinds, StandaloneDeriving,
             TypeOperators, GADTs, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Data (
        module Data.Typeable,
        Data(
                gfoldl,
                gunfold,
                toConstr,
                dataTypeOf,
                dataCast1,      
                dataCast2,      
                gmapT,
                gmapQ,
                gmapQl,
                gmapQr,
                gmapQi,
                gmapM,
                gmapMp,
                gmapMo
            ),
        DataType,       
        mkDataType,
        mkIntType,
        mkFloatType,
        mkCharType,
        mkNoRepType,
        dataTypeName,
        DataRep(..),
        dataTypeRep,
        repConstr,
        isAlgType,
        dataTypeConstrs,
        indexConstr,
        maxConstrIndex,
        isNorepType,
        Constr,         
        ConIndex,       
        Fixity(..),
        mkConstr,
        mkIntegralConstr,
        mkRealConstr,
        mkCharConstr,
        constrType,
        ConstrRep(..),
        constrRep,
        constrFields,
        constrFixity,
        constrIndex,
        showConstr,
        readConstr,
        tyconUQname,
        tyconModule,
        fromConstr,
        fromConstrB,
        fromConstrM
  ) where
import Data.Either
import Data.Eq
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Typeable
import Data.Version( Version(..) )
import GHC.Base hiding (Any)
import GHC.List
import GHC.Num
import GHC.Read
import GHC.Show
import Text.Read( reads )
import Data.Int              
import Data.Type.Coercion
import Data.Word             
import GHC.Real              
import GHC.Ptr               
import GHC.ForeignPtr        
import GHC.Arr               
class Typeable a => Data a where
  gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)
          -> (forall g. g -> c g)
          -> a
          -> c a
  gfoldl _ z = z
  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a
  toConstr   :: a -> Constr
  dataTypeOf  :: a -> DataType
  dataCast1 :: Typeable t
            => (forall d. Data d => c (t d))
            -> Maybe (c a)
  dataCast1 _ = Nothing
  dataCast2 :: Typeable t
            => (forall d e. (Data d, Data e) => c (t d e))
            -> Maybe (c a)
  dataCast2 _ = Nothing
  gmapT :: (forall b. Data b => b -> b) -> a -> a
  gmapT f x0 = unID (gfoldl k ID x0)
    where
      k :: Data d => ID (d->b) -> d -> ID b
      k (ID c) x = ID (c (f x))
  gmapQl :: forall r r'. (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQl o r f = unCONST . gfoldl k z
    where
      k :: Data d => CONST r (d->b) -> d -> CONST r b
      k c x = CONST $ (unCONST c) `o` f x
      z :: g -> CONST r g
      z _   = CONST r
  gmapQr :: forall r r'. (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQr o r0 f x0 = unQr (gfoldl k (const (Qr id)) x0) r0
    where
      k :: Data d => Qr r (d->b) -> d -> Qr r b
      k (Qr c) x = Qr (\r -> c (f x `o` r))
  gmapQ :: (forall d. Data d => d -> u) -> a -> [u]
  gmapQ f = gmapQr (:) [] f
  gmapQi :: forall u. Int -> (forall d. Data d => d -> u) -> a -> u
  gmapQi i f x = case gfoldl k z x of { Qi _ q -> fromJust q }
    where
      k :: Data d => Qi u (d -> b) -> d -> Qi u b
      k (Qi i' q) a = Qi (i'+1) (if i==i' then Just (f a) else q)
      z :: g -> Qi q g
      z _           = Qi 0 Nothing
  gmapM :: forall m. Monad m => (forall d. Data d => d -> m d) -> a -> m a
  gmapM f = gfoldl k return
    where
      k :: Data d => m (d -> b) -> d -> m b
      k c x = do c' <- c
                 x' <- f x
                 return (c' x')
  gmapMp :: forall m. MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a
  gmapMp f x = unMp (gfoldl k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z :: g -> Mp m g
      z g = Mp (return (g,False))
      k :: Data d => Mp m (d -> b) -> d -> Mp m b
      k (Mp c) y
        = Mp ( c >>= \(h, b) ->
                 (f y >>= \y' -> return (h y', True))
                 `mplus` return (h y, b)
             )
  gmapMo :: forall m. MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a
  gmapMo f x = unMp (gfoldl k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z :: g -> Mp m g
      z g = Mp (return (g,False))
      k :: Data d => Mp m (d -> b) -> d -> Mp m b
      k (Mp c) y
        = Mp ( c >>= \(h,b) -> if b
                        then return (h y, b)
                        else (f y >>= \y' -> return (h y',True))
                             `mplus` return (h y, b)
             )
newtype ID x = ID { unID :: x }
newtype CONST c a = CONST { unCONST :: c }
data Qi q a = Qi Int (Maybe q)
newtype Qr r a = Qr { unQr  :: r -> r }
newtype Mp m x = Mp { unMp :: m (x, Bool) }
fromConstr :: Data a => Constr -> a
fromConstr = fromConstrB (error "Data.Data.fromConstr")
fromConstrB :: Data a
            => (forall d. Data d => d)
            -> Constr
            -> a
fromConstrB f = unID . gunfold k z
 where
  k :: forall b r. Data b => ID (b -> r) -> ID r
  k c = ID (unID c f)
  z :: forall r. r -> ID r
  z = ID
fromConstrM :: forall m a. (Monad m, Data a)
            => (forall d. Data d => m d)
            -> Constr
            -> m a
fromConstrM f = gunfold k z
 where
  k :: forall b r. Data b => m (b -> r) -> m r
  k c = do { c' <- c; b <- f; return (c' b) }
  z :: forall r. r -> m r
  z = return
data DataType = DataType
                        { tycon   :: String
                        , datarep :: DataRep
                        }
              deriving Show
data Constr = Constr
                        { conrep    :: ConstrRep
                        , constring :: String
                        , confields :: [String] 
                        , confixity :: Fixity   
                        , datatype  :: DataType
                        }
instance Show Constr where
 show = constring
instance Eq Constr where
  c == c' = constrRep c == constrRep c'
data DataRep = AlgRep [Constr]
             | IntRep
             | FloatRep
             | CharRep
             | NoRep
            deriving (Eq,Show)
data ConstrRep = AlgConstr    ConIndex
               | IntConstr    Integer
               | FloatConstr  Rational
               | CharConstr   Char
               deriving (Eq,Show)
type ConIndex = Int
data Fixity = Prefix
            | Infix     
            deriving (Eq,Show)
dataTypeName :: DataType -> String
dataTypeName = tycon
dataTypeRep :: DataType -> DataRep
dataTypeRep = datarep
constrType :: Constr -> DataType
constrType = datatype
constrRep :: Constr -> ConstrRep
constrRep = conrep
repConstr :: DataType -> ConstrRep -> Constr
repConstr dt cr =
      case (dataTypeRep dt, cr) of
        (AlgRep cs, AlgConstr i)      -> cs !! (i-1)
        (IntRep,    IntConstr i)      -> mkIntegralConstr dt i
        (FloatRep,  FloatConstr f)    -> mkRealConstr dt f
        (CharRep,   CharConstr c)     -> mkCharConstr dt c
        _ -> error "Data.Data.repConstr: The given ConstrRep does not fit to the given DataType."
mkDataType :: String -> [Constr] -> DataType
mkDataType str cs = DataType
                        { tycon   = str
                        , datarep = AlgRep cs
                        }
mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dt str fields fix =
        Constr
                { conrep    = AlgConstr idx
                , constring = str
                , confields = fields
                , confixity = fix
                , datatype  = dt
                }
  where
    idx = head [ i | (c,i) <- dataTypeConstrs dt `zip` [1..],
                     showConstr c == str ]
dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs dt = case datarep dt of
                        (AlgRep cons) -> cons
                        _ -> error $ "Data.Data.dataTypeConstrs is not supported for "
                                    ++ dataTypeName dt ++
                                    ", as it is not an algebraic data type."
constrFields :: Constr -> [String]
constrFields = confields
constrFixity :: Constr -> Fixity
constrFixity = confixity
showConstr :: Constr -> String
showConstr = constring
readConstr :: DataType -> String -> Maybe Constr
readConstr dt str =
      case dataTypeRep dt of
        AlgRep cons -> idx cons
        IntRep      -> mkReadCon (\i -> (mkPrimCon dt str (IntConstr i)))
        FloatRep    -> mkReadCon ffloat
        CharRep     -> mkReadCon (\c -> (mkPrimCon dt str (CharConstr c)))
        NoRep       -> Nothing
  where
    mkReadCon :: Read t => (t -> Constr) -> Maybe Constr
    mkReadCon f = case (reads str) of
                    [(t,"")] -> Just (f t)
                    _ -> Nothing
    idx :: [Constr] -> Maybe Constr
    idx cons = let fit = filter ((==) str . showConstr) cons
                in if fit == []
                     then Nothing
                     else Just (head fit)
    ffloat :: Double -> Constr
    ffloat =  mkPrimCon dt str . FloatConstr . toRational
isAlgType :: DataType -> Bool
isAlgType dt = case datarep dt of
                 (AlgRep _) -> True
                 _ -> False
indexConstr :: DataType -> ConIndex -> Constr
indexConstr dt idx = case datarep dt of
                        (AlgRep cs) -> cs !! (idx-1)
                        _           -> error $ "Data.Data.indexConstr is not supported for "
                                               ++ dataTypeName dt ++
                                               ", as it is not an algebraic data type."
constrIndex :: Constr -> ConIndex
constrIndex con = case constrRep con of
                    (AlgConstr idx) -> idx
                    _ -> error $ "Data.Data.constrIndex is not supported for "
                                 ++ dataTypeName (constrType con) ++
                                 ", as it is not an algebraic data type."
maxConstrIndex :: DataType -> ConIndex
maxConstrIndex dt = case dataTypeRep dt of
                        AlgRep cs -> length cs
                        _            -> error $ "Data.Data.maxConstrIndex is not supported for "
                                                 ++ dataTypeName dt ++
                                                 ", as it is not an algebraic data type."
mkIntType :: String -> DataType
mkIntType = mkPrimType IntRep
mkFloatType :: String -> DataType
mkFloatType = mkPrimType FloatRep
mkCharType :: String -> DataType
mkCharType = mkPrimType CharRep
mkPrimType :: DataRep -> String -> DataType
mkPrimType dr str = DataType
                        { tycon   = str
                        , datarep = dr
                        }
mkPrimCon :: DataType -> String -> ConstrRep -> Constr
mkPrimCon dt str cr = Constr
                        { datatype  = dt
                        , conrep    = cr
                        , constring = str
                        , confields = error "Data.Data.confields"
                        , confixity = error "Data.Data.confixity"
                        }
mkIntegralConstr :: (Integral a, Show a) => DataType -> a -> Constr
mkIntegralConstr dt i = case datarep dt of
                  IntRep -> mkPrimCon dt (show i) (IntConstr (toInteger  i))
                  _ -> error $ "Data.Data.mkIntegralConstr is not supported for "
                               ++ dataTypeName dt ++
                               ", as it is not an Integral data type."
mkRealConstr :: (Real a, Show a) => DataType -> a -> Constr
mkRealConstr dt f = case datarep dt of
                    FloatRep -> mkPrimCon dt (show f) (FloatConstr (toRational f))
                    _ -> error $ "Data.Data.mkRealConstr is not supported for "
                                 ++ dataTypeName dt ++
                                 ", as it is not an Real data type."
mkCharConstr :: DataType -> Char -> Constr
mkCharConstr dt c = case datarep dt of
                   CharRep -> mkPrimCon dt (show c) (CharConstr c)
                   _ -> error $ "Data.Data.mkCharConstr is not supported for "
                                ++ dataTypeName dt ++
                                ", as it is not an Char data type."
mkNoRepType :: String -> DataType
mkNoRepType str = DataType
                        { tycon   = str
                        , datarep = NoRep
                        }
isNorepType :: DataType -> Bool
isNorepType dt = case datarep dt of
                   NoRep -> True
                   _ -> False
tyconUQname :: String -> String
tyconUQname x = let x' = dropWhile (not . (==) '.') x
                 in if x' == [] then x else tyconUQname (tail x')
tyconModule :: String -> String
tyconModule x = let (a,b) = break ((==) '.') x
                 in if b == ""
                      then b
                      else a ++ tyconModule' (tail b)
  where
    tyconModule' y = let y' = tyconModule y
                      in if y' == "" then "" else ('.':y')
falseConstr :: Constr
falseConstr  = mkConstr boolDataType "False" [] Prefix
trueConstr :: Constr
trueConstr   = mkConstr boolDataType "True"  [] Prefix
boolDataType :: DataType
boolDataType = mkDataType "Prelude.Bool" [falseConstr,trueConstr]
instance Data Bool where
  toConstr False = falseConstr
  toConstr True  = trueConstr
  gunfold _ z c  = case constrIndex c of
                     1 -> z False
                     2 -> z True
                     _ -> error $ "Data.Data.gunfold: Constructor "
                                  ++ show c
                                  ++ " is not of type Bool."
  dataTypeOf _ = boolDataType
charType :: DataType
charType = mkCharType "Prelude.Char"
instance Data Char where
  toConstr x = mkCharConstr charType x
  gunfold _ z c = case constrRep c of
                    (CharConstr x) -> z x
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Char."
  dataTypeOf _ = charType
floatType :: DataType
floatType = mkFloatType "Prelude.Float"
instance Data Float where
  toConstr = mkRealConstr floatType
  gunfold _ z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Float."
  dataTypeOf _ = floatType
doubleType :: DataType
doubleType = mkFloatType "Prelude.Double"
instance Data Double where
  toConstr = mkRealConstr doubleType
  gunfold _ z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Double."
  dataTypeOf _ = doubleType
intType :: DataType
intType = mkIntType "Prelude.Int"
instance Data Int where
  toConstr x = mkIntegralConstr intType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int."
  dataTypeOf _ = intType
integerType :: DataType
integerType = mkIntType "Prelude.Integer"
instance Data Integer where
  toConstr = mkIntegralConstr integerType
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z x
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Integer."
  dataTypeOf _ = integerType
int8Type :: DataType
int8Type = mkIntType "Data.Int.Int8"
instance Data Int8 where
  toConstr x = mkIntegralConstr int8Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int8."
  dataTypeOf _ = int8Type
int16Type :: DataType
int16Type = mkIntType "Data.Int.Int16"
instance Data Int16 where
  toConstr x = mkIntegralConstr int16Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int16."
  dataTypeOf _ = int16Type
int32Type :: DataType
int32Type = mkIntType "Data.Int.Int32"
instance Data Int32 where
  toConstr x = mkIntegralConstr int32Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int32."
  dataTypeOf _ = int32Type
int64Type :: DataType
int64Type = mkIntType "Data.Int.Int64"
instance Data Int64 where
  toConstr x = mkIntegralConstr int64Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int64."
  dataTypeOf _ = int64Type
wordType :: DataType
wordType = mkIntType "Data.Word.Word"
instance Data Word where
  toConstr x = mkIntegralConstr wordType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word"
  dataTypeOf _ = wordType
word8Type :: DataType
word8Type = mkIntType "Data.Word.Word8"
instance Data Word8 where
  toConstr x = mkIntegralConstr word8Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word8."
  dataTypeOf _ = word8Type
word16Type :: DataType
word16Type = mkIntType "Data.Word.Word16"
instance Data Word16 where
  toConstr x = mkIntegralConstr word16Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word16."
  dataTypeOf _ = word16Type
word32Type :: DataType
word32Type = mkIntType "Data.Word.Word32"
instance Data Word32 where
  toConstr x = mkIntegralConstr word32Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word32."
  dataTypeOf _ = word32Type
word64Type :: DataType
word64Type = mkIntType "Data.Word.Word64"
instance Data Word64 where
  toConstr x = mkIntegralConstr word64Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word64."
  dataTypeOf _ = word64Type
ratioConstr :: Constr
ratioConstr = mkConstr ratioDataType ":%" [] Infix
ratioDataType :: DataType
ratioDataType = mkDataType "GHC.Real.Ratio" [ratioConstr]
instance (Data a, Integral a) => Data (Ratio a) where
  gfoldl k z (a :% b) = z (%) `k` a `k` b
  toConstr _ = ratioConstr
  gunfold k z c | constrIndex c == 1 = k (k (z (%)))
  gunfold _ _ _ = error "Data.Data.gunfold(Ratio)"
  dataTypeOf _  = ratioDataType
nilConstr :: Constr
nilConstr    = mkConstr listDataType "[]" [] Prefix
consConstr :: Constr
consConstr   = mkConstr listDataType "(:)" [] Infix
listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]
instance Data a => Data [a] where
  gfoldl _ z []     = z []
  gfoldl f z (x:xs) = z (:) `f` x `f` xs
  toConstr []    = nilConstr
  toConstr (_:_) = consConstr
  gunfold k z c = case constrIndex c of
                    1 -> z []
                    2 -> k (k (z (:)))
                    _ -> error "Data.Data.gunfold(List)"
  dataTypeOf _ = listDataType
  dataCast1 f  = gcast1 f
  gmapT  _   []     = []
  gmapT  f   (x:xs) = (f x:f xs)
  gmapQ  _   []     = []
  gmapQ  f   (x:xs) = [f x,f xs]
  gmapM  _   []     = return []
  gmapM  f   (x:xs) = f x >>= \x' -> f xs >>= \xs' -> return (x':xs')
nothingConstr :: Constr
nothingConstr = mkConstr maybeDataType "Nothing" [] Prefix
justConstr :: Constr
justConstr    = mkConstr maybeDataType "Just"    [] Prefix
maybeDataType :: DataType
maybeDataType = mkDataType "Prelude.Maybe" [nothingConstr,justConstr]
instance Data a => Data (Maybe a) where
  gfoldl _ z Nothing  = z Nothing
  gfoldl f z (Just x) = z Just `f` x
  toConstr Nothing  = nothingConstr
  toConstr (Just _) = justConstr
  gunfold k z c = case constrIndex c of
                    1 -> z Nothing
                    2 -> k (z Just)
                    _ -> error "Data.Data.gunfold(Maybe)"
  dataTypeOf _ = maybeDataType
  dataCast1 f  = gcast1 f
ltConstr :: Constr
ltConstr         = mkConstr orderingDataType "LT" [] Prefix
eqConstr :: Constr
eqConstr         = mkConstr orderingDataType "EQ" [] Prefix
gtConstr :: Constr
gtConstr         = mkConstr orderingDataType "GT" [] Prefix
orderingDataType :: DataType
orderingDataType = mkDataType "Prelude.Ordering" [ltConstr,eqConstr,gtConstr]
instance Data Ordering where
  gfoldl _ z LT  = z LT
  gfoldl _ z EQ  = z EQ
  gfoldl _ z GT  = z GT
  toConstr LT  = ltConstr
  toConstr EQ  = eqConstr
  toConstr GT  = gtConstr
  gunfold _ z c = case constrIndex c of
                    1 -> z LT
                    2 -> z EQ
                    3 -> z GT
                    _ -> error "Data.Data.gunfold(Ordering)"
  dataTypeOf _ = orderingDataType
leftConstr :: Constr
leftConstr     = mkConstr eitherDataType "Left"  [] Prefix
rightConstr :: Constr
rightConstr    = mkConstr eitherDataType "Right" [] Prefix
eitherDataType :: DataType
eitherDataType = mkDataType "Prelude.Either" [leftConstr,rightConstr]
instance (Data a, Data b) => Data (Either a b) where
  gfoldl f z (Left a)   = z Left  `f` a
  gfoldl f z (Right a)  = z Right `f` a
  toConstr (Left _)  = leftConstr
  toConstr (Right _) = rightConstr
  gunfold k z c = case constrIndex c of
                    1 -> k (z Left)
                    2 -> k (z Right)
                    _ -> error "Data.Data.gunfold(Either)"
  dataTypeOf _ = eitherDataType
  dataCast2 f  = gcast2 f
tuple0Constr :: Constr
tuple0Constr = mkConstr tuple0DataType "()" [] Prefix
tuple0DataType :: DataType
tuple0DataType = mkDataType "Prelude.()" [tuple0Constr]
instance Data () where
  toConstr ()   = tuple0Constr
  gunfold _ z c | constrIndex c == 1 = z ()
  gunfold _ _ _ = error "Data.Data.gunfold(unit)"
  dataTypeOf _  = tuple0DataType
tuple2Constr :: Constr
tuple2Constr = mkConstr tuple2DataType "(,)" [] Infix
tuple2DataType :: DataType
tuple2DataType = mkDataType "Prelude.(,)" [tuple2Constr]
instance (Data a, Data b) => Data (a,b) where
  gfoldl f z (a,b) = z (,) `f` a `f` b
  toConstr (_,_) = tuple2Constr
  gunfold k z c | constrIndex c == 1 = k (k (z (,)))
  gunfold _ _ _ = error "Data.Data.gunfold(tup2)"
  dataTypeOf _  = tuple2DataType
  dataCast2 f   = gcast2 f
tuple3Constr :: Constr
tuple3Constr = mkConstr tuple3DataType "(,,)" [] Infix
tuple3DataType :: DataType
tuple3DataType = mkDataType "Prelude.(,,)" [tuple3Constr]
instance (Data a, Data b, Data c) => Data (a,b,c) where
  gfoldl f z (a,b,c) = z (,,) `f` a `f` b `f` c
  toConstr (_,_,_) = tuple3Constr
  gunfold k z c | constrIndex c == 1 = k (k (k (z (,,))))
  gunfold _ _ _ = error "Data.Data.gunfold(tup3)"
  dataTypeOf _  = tuple3DataType
tuple4Constr :: Constr
tuple4Constr = mkConstr tuple4DataType "(,,,)" [] Infix
tuple4DataType :: DataType
tuple4DataType = mkDataType "Prelude.(,,,)" [tuple4Constr]
instance (Data a, Data b, Data c, Data d)
         => Data (a,b,c,d) where
  gfoldl f z (a,b,c,d) = z (,,,) `f` a `f` b `f` c `f` d
  toConstr (_,_,_,_) = tuple4Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (z (,,,)))))
                    _ -> error "Data.Data.gunfold(tup4)"
  dataTypeOf _ = tuple4DataType
tuple5Constr :: Constr
tuple5Constr = mkConstr tuple5DataType "(,,,,)" [] Infix
tuple5DataType :: DataType
tuple5DataType = mkDataType "Prelude.(,,,,)" [tuple5Constr]
instance (Data a, Data b, Data c, Data d, Data e)
         => Data (a,b,c,d,e) where
  gfoldl f z (a,b,c,d,e) = z (,,,,) `f` a `f` b `f` c `f` d `f` e
  toConstr (_,_,_,_,_) = tuple5Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (k (z (,,,,))))))
                    _ -> error "Data.Data.gunfold(tup5)"
  dataTypeOf _ = tuple5DataType
tuple6Constr :: Constr
tuple6Constr = mkConstr tuple6DataType "(,,,,,)" [] Infix
tuple6DataType :: DataType
tuple6DataType = mkDataType "Prelude.(,,,,,)" [tuple6Constr]
instance (Data a, Data b, Data c, Data d, Data e, Data f)
         => Data (a,b,c,d,e,f) where
  gfoldl f z (a,b,c,d,e,f') = z (,,,,,) `f` a `f` b `f` c `f` d `f` e `f` f'
  toConstr (_,_,_,_,_,_) = tuple6Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (k (k (z (,,,,,)))))))
                    _ -> error "Data.Data.gunfold(tup6)"
  dataTypeOf _ = tuple6DataType
tuple7Constr :: Constr
tuple7Constr = mkConstr tuple7DataType "(,,,,,,)" [] Infix
tuple7DataType :: DataType
tuple7DataType = mkDataType "Prelude.(,,,,,,)" [tuple7Constr]
instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g)
         => Data (a,b,c,d,e,f,g) where
  gfoldl f z (a,b,c,d,e,f',g) =
    z (,,,,,,) `f` a `f` b `f` c `f` d `f` e `f` f' `f` g
  toConstr  (_,_,_,_,_,_,_) = tuple7Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (k (k (k (z (,,,,,,))))))))
                    _ -> error "Data.Data.gunfold(tup7)"
  dataTypeOf _ = tuple7DataType
instance Data a => Data (Ptr a) where
  toConstr _   = error "Data.Data.toConstr(Ptr)"
  gunfold _ _  = error "Data.Data.gunfold(Ptr)"
  dataTypeOf _ = mkNoRepType "GHC.Ptr.Ptr"
  dataCast1 x  = gcast1 x
instance Data a => Data (ForeignPtr a) where
  toConstr _   = error "Data.Data.toConstr(ForeignPtr)"
  gunfold _ _  = error "Data.Data.gunfold(ForeignPtr)"
  dataTypeOf _ = mkNoRepType "GHC.ForeignPtr.ForeignPtr"
  dataCast1 x  = gcast1 x
instance (Data a, Data b, Ix a) => Data (Array a b)
 where
  gfoldl f z a = z (listArray (bounds a)) `f` (elems a)
  toConstr _   = error "Data.Data.toConstr(Array)"
  gunfold _ _  = error "Data.Data.gunfold(Array)"
  dataTypeOf _ = mkNoRepType "Data.Array.Array"
  dataCast2 x  = gcast2 x
proxyConstr :: Constr
proxyConstr = mkConstr proxyDataType "Proxy" [] Prefix
proxyDataType :: DataType
proxyDataType = mkDataType "Data.Proxy.Proxy" [proxyConstr]
instance (Data t) => Data (Proxy t) where
  gfoldl _ z Proxy  = z Proxy
  toConstr Proxy  = proxyConstr
  gunfold _ z c = case constrIndex c of
                    1 -> z Proxy
                    _ -> error "Data.Data.gunfold(Proxy)"
  dataTypeOf _ = proxyDataType
  dataCast1 f  = gcast1 f
reflConstr :: Constr
reflConstr = mkConstr equalityDataType "Refl" [] Prefix
equalityDataType :: DataType
equalityDataType = mkDataType "Data.Type.Equality.(:~:)" [reflConstr]
instance (a ~ b, Data a) => Data (a :~: b) where
  gfoldl _ z Refl = z Refl
  toConstr Refl   = reflConstr
  gunfold _ z c   = case constrIndex c of
                      1 -> z Refl
                      _ -> error "Data.Data.gunfold(:~:)"
  dataTypeOf _    = equalityDataType
  dataCast2 f     = gcast2 f
coercionConstr :: Constr
coercionConstr = mkConstr equalityDataType "Coercion" [] Prefix
coercionDataType :: DataType
coercionDataType = mkDataType "Data.Type.Coercion.Coercion" [coercionConstr]
instance (Coercible a b, Data a, Data b) => Data (Coercion a b) where
  gfoldl _ z Coercion = z Coercion
  toConstr Coercion = coercionConstr
  gunfold _ z c   = case constrIndex c of
                      1 -> z Coercion
                      _ -> error "Data.Data.gunfold(Coercion)"
  dataTypeOf _    = coercionDataType
  dataCast2 f     = gcast2 f
versionConstr :: Constr
versionConstr = mkConstr versionDataType "Version" ["versionBranch","versionTags"] Prefix
versionDataType :: DataType
versionDataType = mkDataType "Data.Version.Version" [versionConstr]
instance Data Version where
  gfoldl k z (Version bs ts) = z Version `k` bs `k` ts
  toConstr (Version _ _) = versionConstr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (z Version))
                    _ -> error "Data.Data.gunfold(Version)"
  dataTypeOf _  = versionDataType
dualConstr :: Constr
dualConstr = mkConstr dualDataType "Dual" ["getDual"] Prefix
dualDataType :: DataType
dualDataType = mkDataType "Data.Monoid.Dual" [dualConstr]
instance Data a => Data (Dual a) where
  gfoldl f z (Dual x) = z Dual `f` x
  gunfold k z _ = k (z Dual)
  toConstr (Dual _) = dualConstr
  dataTypeOf _ = dualDataType
  dataCast1 f = gcast1 f
allConstr :: Constr
allConstr = mkConstr allDataType "All" ["getAll"] Prefix
allDataType :: DataType
allDataType = mkDataType "All" [allConstr]
instance Data All where
  gfoldl f z (All x) = (z All `f` x)
  gunfold k z _ = k (z All)
  toConstr (All _) = allConstr
  dataTypeOf _ = allDataType
anyConstr :: Constr
anyConstr = mkConstr anyDataType "Any" ["getAny"] Prefix
anyDataType :: DataType
anyDataType = mkDataType "Any" [anyConstr]
instance Data Any where
  gfoldl f z (Any x) = (z Any `f` x)
  gunfold k z _ = k (z Any)
  toConstr (Any _) = anyConstr
  dataTypeOf _ = anyDataType
sumConstr :: Constr
sumConstr = mkConstr sumDataType "Sum" ["getSum"] Prefix
sumDataType :: DataType
sumDataType = mkDataType "Data.Monoid.Sum" [sumConstr]
instance Data a => Data (Sum a) where
  gfoldl f z (Sum x) = z Sum `f` x
  gunfold k z _ = k (z Sum)
  toConstr (Sum _) = sumConstr
  dataTypeOf _ = sumDataType
  dataCast1 f = gcast1 f
productConstr :: Constr
productConstr = mkConstr productDataType "Product" ["getProduct"] Prefix
productDataType :: DataType
productDataType = mkDataType "Data.Monoid.Product" [productConstr]
instance Data a => Data (Product a) where
  gfoldl f z (Product x) = z Product `f` x
  gunfold k z _ = k (z Product)
  toConstr (Product _) = productConstr
  dataTypeOf _ = productDataType
  dataCast1 f = gcast1 f
firstConstr :: Constr
firstConstr = mkConstr firstDataType "First" ["getFirst"] Prefix
firstDataType :: DataType
firstDataType = mkDataType "Data.Monoid.First" [firstConstr]
instance Data a => Data (First a) where
  gfoldl f z (First x) = (z First `f` x)
  gunfold k z _ = k (z First)
  toConstr (First _) = firstConstr
  dataTypeOf _ = firstDataType
  dataCast1 f = gcast1 f
lastConstr :: Constr
lastConstr = mkConstr lastDataType "Last" ["getLast"] Prefix
lastDataType :: DataType
lastDataType = mkDataType "Data.Monoid.Last" [lastConstr]
instance Data a => Data (Last a) where
  gfoldl f z (Last x) = (z Last `f` x)
  gunfold k z _ = k (z Last)
  toConstr (Last _) = lastConstr
  dataTypeOf _ = lastDataType
  dataCast1 f = gcast1 f
altConstr :: Constr
altConstr = mkConstr altDataType "Alt" ["getAlt"] Prefix
altDataType :: DataType
altDataType = mkDataType "Alt" [altConstr]
instance (Data (f a), Data a, Typeable f) => Data (Alt f a) where
  gfoldl f z (Alt x) = (z Alt `f` x)
  gunfold k z _ = k (z Alt)
  toConstr (Alt _) = altConstr
  dataTypeOf _ = altDataType
