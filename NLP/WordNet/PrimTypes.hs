module NLP.WordNet.PrimTypes where

import Data.Array
import System.IO
import Control.OldException
import Data.Dynamic (Typeable)

type Offset = Integer

-- | The basic part of speech type, either a 'Noun', 'Verb', 'Adj'ective or 'Adv'erb.
data POS = Noun | Verb | Adj | Adv
         deriving (Eq, Ord, Show, Ix, Typeable)
allPOS = [Noun ..]

data EPOS = POS POS | Satellite | AdjSatellite | IndirectAnt | DirectAnt | UnknownEPos | Pertainym
          deriving (Eq, Ord, Typeable)

fromEPOS (POS p) = p
fromEPOS _ = Adj

allEPOS = [POS Noun ..]

instance Enum POS where
  toEnum 1 = Noun
  toEnum 2 = Verb
  toEnum 3 = Adj
  toEnum 4 = Adv
  fromEnum Noun = 1
  fromEnum Verb = 2
  fromEnum Adj  = 3
  fromEnum Adv  = 4
  enumFrom i = enumFromTo i Adv
  enumFromThen i j = enumFromThenTo i j Adv

instance Enum EPOS where
  toEnum 1 = POS Noun
  toEnum 2 = POS Verb
  toEnum 3 = POS Adj
  toEnum 4 = POS Adv
  toEnum 5 = Satellite
  toEnum 6 = AdjSatellite
  fromEnum (POS Noun) = 1
  fromEnum (POS Verb) = 2
  fromEnum (POS Adj)  = 3
  fromEnum (POS Adv)  = 4
  fromEnum Satellite  = 5
  fromEnum AdjSatellite = 6
  enumFrom i = enumFromTo i AdjSatellite
  enumFromThen i j = enumFromThenTo i j AdjSatellite

instance Show EPOS where
  showsPrec i (POS p) = showsPrec i p
  showsPrec i (Satellite) = showString "Satellite"
  showsPrec i (AdjSatellite) = showString "AdjSatellite"
  showsPrec i (IndirectAnt) = showString "IndirectAnt"
  showsPrec i (DirectAnt) = showString "DirectAnt"
  showsPrec i (Pertainym) = showString "Pertainym"
  showsPrec i (UnknownEPos) = showString "UnknownEPos"

instance Ix EPOS where
  range (i,j) = [i..j]
  index (i,j) a = fromEnum a - fromEnum i
  inRange (i,j) a = a `elem` [i..j]


readEPOS :: String -> EPOS
readEPOS "n" = POS Noun
readEPOS "v" = POS Verb
readEPOS "a" = POS Adj
readEPOS "r" = POS Adv
readEPOS "s" = Satellite

data WordNetEnv =
     WordNetEnv {
       dataHandles :: Array POS (Handle, Handle), -- index, real
       excHandles  :: Array POS Handle, -- for morphology
       senseHandle, 
       countListHandle,
       keyIndexHandle,
       revKeyIndexHandle :: Maybe Handle,  -- these are all optional
       vSentHandle :: Maybe (Handle, Handle), -- index, real
       wnReleaseVersion :: Maybe String,
       dataDirectory :: FilePath,
       warnAbout :: String -> Exception -> IO ()
     }

wordNetEnv0 = WordNetEnv { 
                dataHandles = undefined,
                excHandles  = undefined,
                senseHandle = Nothing, 
                countListHandle = Nothing,
                keyIndexHandle = Nothing,
                revKeyIndexHandle = Nothing,
                vSentHandle = Nothing,
                wnReleaseVersion = Nothing,
                dataDirectory = "",
                warnAbout = \_ _ -> return ()
              }

data SenseKey = 
     SenseKey {
       senseKeyPOS :: POS, 
       senseKeyString :: String,
       senseKeyWord :: String
     } deriving (Show, Typeable)


-- | A 'Key' is a simple pointer into the database, which can be
-- followed using 'lookupKey'.
newtype Key = Key (Offset, POS) deriving (Eq, Typeable)

data Synset =
     Synset {
       hereIAm :: Offset,
       ssType :: EPOS,
       fnum :: Int,
       pos :: EPOS,
       ssWords :: [(String, Int, SenseType)], -- (word, lex-id, sense)
       whichWord :: Maybe Int,
       forms :: [(Form, Offset, EPOS, Int, Int)],
       frames :: [(Int, Int)],
       defn :: String,
       key :: Maybe Offset,

       searchType :: Int,
       headWord :: String,
       headSense :: SenseType
     } -- deriving (Show)

synset0 = Synset 0 UnknownEPos (-1) undefined [] Nothing [] [] "" Nothing (-1) "" AllSenses

-- | The basic type which holds search results.  Its 'Show' instance simply
-- shows the string corresponding to the associated WordNet synset.
data SearchResult =
     SearchResult {
       srSenseKey  :: Maybe SenseKey,
       -- | This provides (maybe) the associated overview for a SearchResult.
       -- The 'Overview' is only available if this 'SearchResult' was
       -- derived from a real search, rather than 'lookupKey'.
       srOverview  :: Maybe Overview,
       srIndex     :: Maybe Index,
       -- | This provides (maybe) the associated sense number for a SearchResult.
       -- The 'SenseType' is only available if this 'SearchResult' was
       -- derived from a real search, rather than 'lookupKey'.
       srSenseNum  :: Maybe SenseType,
       srSynset    :: Synset
     } deriving (Typeable)

data Index = 
     Index {
       indexWord :: String,
       indexPOS :: EPOS,
       indexSenseCount :: Int,
       indexForms :: [Form],
       indexTaggedCount :: Int,
       indexOffsets :: [Offset]
     } deriving (Eq, Ord, Show, Typeable)

index0 = Index "" (POS Noun) (-1) [] (-1) []

-- | The different types of relations which can hold between WordNet Synsets.
data Form = Antonym | Hypernym | Hyponym | Entailment | Similar
          | IsMember | IsStuff | IsPart
          | HasMember | HasStuff | HasPart
          | Meronym | Holonym | CauseTo | PPL | SeeAlso
          | Attribute | VerbGroup | Derivation | Classification | Class | Nominalization
          -- misc:
          | Syns | Freq | Frames | Coords | Relatives | HMeronym | HHolonym | WNGrep | OverviewForm
          | Unknown
          deriving (Eq, Ord, Show, Enum, Typeable)


-- | A 'SenseType' is a way of controlling search.  Either you specify
-- a certain sense (using @SenseNumber n@, or, since 'SenseType' is an
-- instance of 'Num', you can juse use @n@) or by searching using all
-- senses, through 'AllSenses'.  The 'Num' instance performs standard
-- arithmetic on 'SenseNumber's, and 'fromInteger' yields a 'SenseNumber' (always),
-- but any arithmetic involving 'AllSenses' returns 'AllSenses'.
data SenseType = AllSenses | SenseNumber Int deriving (Eq, Ord, Show, Typeable)

instance Num SenseType where
  fromInteger = SenseNumber . fromInteger
  SenseNumber n + SenseNumber m = SenseNumber $ n+m
  _ + _ = AllSenses
  SenseNumber n - SenseNumber m = SenseNumber $ n-m
  _ - _ = AllSenses
  SenseNumber n * SenseNumber m = SenseNumber $ n*m
  _ * _ = AllSenses
  negate (SenseNumber n) = SenseNumber $ negate n
  negate x = x
  abs (SenseNumber n) = SenseNumber $ abs n
  abs x = x
  signum (SenseNumber n) = SenseNumber $ signum n
  signum x = x
  

-- | The 'Overview' type is the return type which gives you an
-- overview of a word, for all sense and for all parts of speech.
data Overview =
     Overview {
       nounIndex,
       verbIndex,
       adjIndex,
       advIndex :: Maybe Index
     } deriving (Eq, Ord, Show, Typeable)
