-- |
-- Module      :  Data.MultextEastMsd
-- Copyright   :  (c) 2009 Jan Snajder
-- License     :  BSD-3 (see the LICENSE file)
--
-- Maintainer  :  Jan Snajder <jan.snajder@fer.hr>
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of the MULTEXT-East Version 3 morphosyntactic descriptors.
--
-- MULTEXT-East encodes values of morphosyntatic attributes in a single string, 
-- using positional encoding. Each attribute is represented by a single letter 
-- at a predefined position, while non-applicable attributes are represented 
-- by hyphens. For example, @Ncmsg@ denotes a common noun (@Nc@) in masculine
-- singular genitive (@msg@) case.
--
-- MULTEXT-EAST Version 3 covers morphosyntactic descriptions for 
-- Bulgarian, Croatian, Czech, English, Estonian, Hungarian, Lithuanian, 
-- Macedonian, Persian, Polish, Resian, Romanian, Russian, Serbian, Slovak, 
-- Slovene, and Ukrainian. For details, refer to <http://nl.ijs.si/ME/V3/>.
--
-- Usage example:
-- 
-- >>> let Just d1 = fromString "Ncmsg"
-- >>> pos d1
-- Noun
-- >>> features d1
-- [NType Common,Gender Masculine,Number Singular,Case Genitive]
-- >>> let d2 = unset NType d1
-- >>> toString d2
-- "N-msg"
-- >>> d1 == d2
-- False
-- >>> d1 =~= d2
-- True

-------------------------------------------------------------------------------

module Data.MultextEastMsd (
  -- * Datatype constructor
  Msd,
  msd,
  PoS (..),
  -- * Getting and setting values
  Attribute,
  get,
  set,
  unset,
  check,
  features,
  pos,
  -- * Wildcard matching
  (=~=),
  -- * From/to string conversion
  toString,
  fromString,
  validString,
  -- * Morphosyntactic features
  Feature (..),
  AType (..),
  Aspect (..),
  Case (..),
  Class (..),
  CoordType (..),
  CType (..),
  Definiteness (..),
  Degree (..),
  Formation (..),
  Gender (..),
  MForm (..),
  MType (..),
  NType (..),
  Number (..),
  Person (..),
  SType (..),
  SubType (..),
  Tense (..),
  VForm (..),
  Voice (..),
  VType (..)) where

import Data.List (find,sort,delete,findIndex,deleteBy,intersectBy,nubBy)
import Data.Maybe (catMaybes,isJust)
import Control.Monad (liftM)

data Msd = Msd PoS [Feature]
  deriving (Show)

instance Eq Msd where
  Msd p1 fs1 == Msd p2 fs2 = p1==p2 && sort fs1==sort fs2

data PoS = Noun | Verb | Adjective | Adposition | Conjunction | Numeral
  deriving (Eq,Enum,Show)

data Feature
  = Animate Bool
  | AType AType
  | Aspect Aspect
  | Case Case  
  | Class Class
  | Clitic Bool
  | CliticS Bool
  | CoordType CoordType
  | Courtesy Bool
  | CType CType
  | Definiteness Definiteness
  | Degree Degree
  | Formation Formation
  | Gender Gender
  | MForm MForm
  | MType MType
  | Negative Bool
  | NType NType
  | Number Number
  | OwnedNumber Number
  | OwnerNumber Number
  | OwnerPerson Person
  | Person Person
  | SType SType
  | SubType SubType
  | Tense Tense
  | VForm VForm
  | Voice Voice
  | VType VType
  deriving (Show,Ord,Eq)

data NType = 
  Common | Proper 
  deriving (Eq,Enum,Ord,Show)
data Gender = 
  Masculine | Feminine | Neuter
  deriving (Eq,Enum,Ord,Show)
data Number = 
  Singular | Plural | Dual | Count | Collective
  deriving (Eq,Enum,Ord,Show)
data Case = 
  Nominative | Genitive | Dative | Accusative | Vocative | 
  Locative | Instrumental | Direct | Oblique | Partitive |
  Illative | Inessive | Elative | Allative | Adessive | Ablative |
  Translative | Terminative | Essive | Abessive | Komitative | 
  Aditive | Temporalis | Causalis | Sublative | Delative | 
  Sociative | Factive | Superessive | Distributive | EssiveFormal |
  Multiplicative
  deriving (Eq,Enum,Ord,Show)
data Definiteness = 
  No | Yes | ShortArt | FullArt | OneSTwoS
  deriving (Eq,Enum,Ord,Show)
data VType = 
  Main | Auxiliary | Modal | Copula | Base
  deriving (Eq,Enum,Ord,Show)
data VForm = 
  Indicative | Subjunctive | Imperative | Conditional | 
  Infinitive | Participle | Gerund | Supine | Transgressive | Quotative
  deriving (Eq,Enum,Ord,Show)
data Tense = 
  Present | Imperfect | Future | Past | Pluperfect | Aorist
  deriving (Eq,Enum,Ord,Show)
data Person = 
  First | Second | Third
  deriving (Eq,Enum,Ord,Show)
data Voice = 
  Active | Passive
  deriving (Eq,Enum,Ord,Show)
data Aspect = 
  Progressive | Perfective
  deriving (Eq,Enum,Ord,Show)
data AType = 
  Qualificative | Indefinite | Possessive | OrdinalT
  deriving (Eq,Enum,Ord,Show)
data Degree = 
  Positive | Comparative | Superlative | ElativeD | Diminutive
  deriving (Eq,Enum,Ord,Show)
data Formation = 
  Nominal | Simple | Compound
  deriving (Eq,Enum,Ord,Show)
data MType = 
  Cardinal | Ordinal | Fractal | Multiple | Collect | Special
  deriving (Eq,Enum,Ord,Show)
data MForm = 
  Digit | Roman | Letter | Both | MForm_ | Approx
  deriving (Eq,Enum,Ord,Show)
data Class = 
  Definite1 | Definite2 | Definite34 | Definite | Demonstrative |
  IndefiniteC | Interrogative | Relative
  deriving (Eq,Enum,Ord,Show)
data SType = 
  Preposition | Postposition
  deriving (Eq,Enum,Ord,Show)
data CType = 
  Coordinating | Subordinating | Portmanteau
  deriving (Eq,Enum,Ord,Show)
data CoordType = 
  CTSimple | CTRepetit | CTCorrelat | CTSentence | CTWords | 
  Initial | NonInitial
  deriving (Eq,Enum,Ord,Show)
data SubType = 
  STNegative | STPositive
  deriving (Eq,Enum,Ord,Show)

-- A helper function to identify constructors in a list

eq (Animate _)      (Animate _)      = True
eq (AType _)        (AType _)        = True
eq (Aspect _)       (Aspect _)       = True
eq (Case _)         (Case _)         = True
eq (Class _)        (Class _)        = True
eq (Clitic _)       (Clitic _)       = True
eq (CoordType _)    (CoordType _)    = True
eq (Courtesy _)     (Courtesy _)     = True
eq (CType _)        (CType _)        = True
eq (Definiteness _) (Definiteness _) = True
eq (Degree _)       (Degree _)       = True
eq (Formation _)    (Formation _)    = True
eq (Gender _)       (Gender _)       = True
eq (MForm _)        (MForm _)        = True
eq (MType _)        (MType _)        = True
eq (Negative _)     (Negative _)     = True
eq (NType _)        (NType _)        = True
eq (Number _)       (Number _)       = True
eq (OwnedNumber _)  (OwnedNumber _)  = True
eq (OwnerNumber _)  (OwnerNumber _)  = True
eq (OwnerPerson _)  (OwnerPerson _)  = True
eq (Person _)       (Person _)       = True
eq (SType _)        (SType _)        = True
eq (SubType _)      (SubType _)      = True
eq (Tense _)        (Tense _)        = True
eq (VForm _)        (VForm _)        = True
eq (Voice _)        (Voice _)        = True
eq (VType _)        (VType _)        = True
eq _                _                = False

-- | Constructs a morphosyntactic descriptor (an abstract @Msd@ datatype) of 
-- a specified part-of-speech and with specified features (attribute-value 
-- pairs). Duplicated attributes and attributes not applicable to the given 
-- part-of-speech are ignored.
msd :: PoS -> [Feature] -> Msd
msd p fs = set fs $ Msd p []

class MsdPattern a where
  -- | A wildcard-matching operator between two Msd patterns.
  -- Relation @ msd1 =~= msd2 @ holds iff @msd1@ and @msd2@ are of the same
  -- part-of-speech and the attributes common to @msd1@
  -- and @msd2@ have identical values. The attributes of @msd1@ that are not
  -- set in @msd2@ (and conversely) are ignored in the comparison.
  -- In MULTEXT-East notation, this is tantamount to 
  -- having character code @-@ (hyphen) act as a wildcard.
  (=~=) :: a -> a -> Bool

infix 4 =~=

instance MsdPattern Msd where
  Msd p1 fs1 =~= Msd p2 fs2 = 
    Msd p1 (intersectBy eq fs1 fs2) == Msd p2 (intersectBy eq fs2 fs1)

instance MsdPattern a => MsdPattern (Maybe a) where
  (Just x) =~= (Just y) = x =~= y
  _        =~= _        = False

instance MsdPattern a => MsdPattern [a] where
  xs =~= ys = and (zipWith (=~=) xs ys) && length xs == length ys

type Attribute a = a -> Feature

x_ :: (Enum a) => a
x_ = toEnum 0

-- | Gets the value of a specified attribute.
get :: (Enum a) => Attribute a -> Msd -> Maybe Feature
get a (Msd _ fs) = find (`eq` a x_) fs

-- | Sets the specified features (attribute-value pairs). Duplicated 
-- attributes and  attributes not applicable to the given part-of-speech 
-- are ignored.
set :: [Feature] -> Msd -> Msd
set fs2 (Msd p fs1) = Msd p $ nubBy eq fs3
  where fs3 = intersectBy eq fs2 (posFeatures p) ++ fs1

-- | Unsets the value of a specified attribute.
unset :: (Enum a) => Attribute a -> Msd -> Msd
unset a (Msd p fs) = Msd p $ deleteBy eq (a x_) fs

-- | Checks whether the attributes are set to the specified values.
check :: [Feature] -> Msd -> Bool
check fs2 (Msd _ fs1) = all (\av -> isJust . find (==av) $ fs1) fs2

-- | Returns the features (attribute-value pairs) of a @Msd@.
features :: Msd -> [Feature]
features (Msd _ fs) = fs

-- | Returns a part-of-speech ('PoS' value) of an @Msd@.
pos :: Msd -> PoS
pos (Msd pos _) = pos

posCodes = "NVASCM"

decodeWith :: (Enum a) => String -> Char -> Maybe a
decodeWith cs c = toEnum `liftM` findIndex (==c) cs

encodeWith :: (Enum a) => String -> a -> Char
encodeWith cs x = cs !! fromEnum x

-- | Converts an @Msd@ datatype into a MULTEXT-East string notation.
toString :: Msd -> String
toString (Msd p fs) = 
  trim $ c : map (\av -> enc $ get av fs) (posFeatures p)
  where trim = reverse . dropWhile (=='-') . reverse
        enc Nothing  = '-'
        enc (Just x) = encode x
        c = encodeWith posCodes p
        get av = find (`eq` av)
  
-- | Converts a MULTEXT-East string notation into an @Msd@ datatype.
-- Returns @Nothing@ if string is not a valid MULTEXT-East string.
fromString :: String -> Maybe Msd
fromString (c:cs) = do
  p <- decodeWith posCodes c
  let fs = zipWith decode (posFeatures p) cs
  if (all isJust fs) 
    then Just $ Msd p (catMaybes . catMaybes $ fs)
    else Nothing

-- | Checks whether the string conforms to the MULTEXT-East specification.
-- Defined as:
-- @ validString = isJust . fromString @
validString :: String -> Bool
validString = isJust . fromString
 
posFeatures Noun = 
  [NType x_,Gender x_,Number x_,Case x_,Clitic x_,Definiteness x_,
   Animate x_,OwnerNumber x_,OwnerPerson x_,OwnedNumber x_]
posFeatures Verb =
  [VType x_,VForm x_,Tense x_,Person x_,Number x_,Gender x_,Voice x_,
   Negative x_,Definiteness x_,Clitic x_,Case x_,Animate x_,CliticS x_,
   Aspect x_,Courtesy x_]
posFeatures Adjective =
  [AType x_,Degree x_,Gender x_,Number x_,Case x_,Definiteness x_,
   Clitic x_,Animate x_,Formation x_,OwnerNumber x_,OwnerPerson x_,
   OwnedNumber x_]
posFeatures Adposition =
  [SType x_,Formation x_,Case x_,Clitic x_]
posFeatures Conjunction =
  [CType x_,Formation x_,CoordType x_,SubType x_,Clitic x_,Number x_,
   Person x_]
posFeatures Numeral = 
  [MType x_,Gender x_,Number x_,Case x_,MForm x_,Definiteness x_,
   Clitic x_,Class x_,Animate x_,OwnerNumber x_,OwnerPerson x_,
   OwnedNumber x_]

-- Helper functions to unwrap the values

encode (NType v)        = enc NType v
encode (Gender v)       = enc Gender v
encode (Number v)       = enc Number v
encode (Case v)         = enc Case v
encode (Definiteness v) = enc Definiteness v
encode (Clitic v)       = enc Clitic v
encode (CliticS v)      = enc CliticS v
encode (VType v)        = enc VType v
encode (VForm v)        = enc VForm v
encode (Tense v)        = enc Tense v
encode (Person v)       = enc Person v
encode (AType v)        = enc AType v
encode (Voice v)        = enc Voice v
encode (Aspect v)       = enc Aspect v
encode (Degree v)       = enc Degree v
encode (Formation v)    = enc Formation v       
encode (MType v)        = enc MType v
encode (MForm v)        = enc MForm v
encode (Class v)        = enc Class v
encode (SType v)        = enc SType v
encode (CType v)        = enc CType v
encode (CoordType v)    = enc CoordType v
encode (SubType v)      = enc SubType v
encode (Animate v)      = enc Animate v
encode (OwnerNumber v)  = enc OwnerNumber v
encode (OwnerPerson v)  = enc OwnerPerson v
encode (OwnedNumber v)  = enc OwnedNumber v
encode (Negative v)     = enc Negative v
encode (Courtesy v)     = enc Courtesy v

decode (NType _)        = dec NType
decode (Gender _)       = dec Gender
decode (Number _)       = dec Number
decode (Case _)         = dec Case
decode (Definiteness _) = dec Definiteness
decode (Clitic _)       = dec Clitic
decode (CliticS _)      = dec CliticS
decode (VType _)        = dec VType
decode (VForm _)        = dec VForm
decode (Tense _)        = dec Tense
decode (Person _)       = dec Person
decode (AType _)        = dec AType
decode (Voice _)        = dec Voice
decode (Aspect _)       = dec Aspect
decode (Degree _)       = dec Degree
decode (Formation _)    = dec Formation
decode (MType _)        = dec MType
decode (MForm _)        = dec MForm
decode (Class _)        = dec Class
decode (SType _)        = dec SType
decode (CType _)        = dec CType
decode (CoordType _)    = dec CoordType
decode (SubType _)      = dec SubType
decode (Animate _)      = dec Animate
decode (OwnerNumber _)  = dec OwnerNumber
decode (OwnerPerson _)  = dec OwnerPerson
decode (OwnedNumber _)  = dec OwnedNumber
decode (Negative _)     = dec Negative
decode (Courtesy _)     = dec Courtesy

enc :: (Enum a) => Attribute a -> a -> Char
enc a v = encodeWith (codes $ a x_) v

dec :: (Enum a) => Attribute a -> Char -> Maybe (Maybe Feature)
dec _ '-' = Just Nothing
dec a c   = decodeWith (codes $ a x_) c >>= return . Just . a

-- MULTEXT-East Version 3 codes

codes :: Feature -> String
codes (NType x)         = "cp"
codes (Gender x_)       = "mfn"
codes (Number x_)       = "spdtl"
codes (Case x_)         = "ngdavliro1x2et3b49w5k7mcshqypuf6"
codes (Definiteness x_) = "nysf2"
codes (Clitic x_)       = "ny"
codes (CliticS x_)      = "ny"
codes (VType x_)        = "maoc"
codes (VForm x_)        = "ismcnpgutq"
codes (Tense x_)        = "pifsla"
codes (Person x_)       = "123"
codes (AType x_)        = "fiso"
codes (Voice x_)        = "ap"
codes (Aspect x_)       = "pe"
codes (Degree x_)       = "pcsed"
codes (Formation x_)    = "nsc"
codes (MType x_)        = "cofmls"
codes (MForm x_)        = "drlbma"
codes (Class x_)        = "123fdiqr"
codes (SType x_)        = "pt"
codes (CType x_)        = "csr"
codes (CoordType x_)    = "srcpwin"
codes (SubType x_)      = "zp"
codes (Animate x_)      = "ny"
codes (Courtesy x_)     = "ny"
codes (Negative x_)     = "ny"
codes (OwnerNumber x_)  = "spdtl"
codes (OwnerPerson x_)  = "123"
codes (OwnedNumber x_)  = "spdtl"

