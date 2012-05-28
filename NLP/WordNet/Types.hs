-----------------------------------------------------------------------------
-- |
-- Module      :  NLP.WordNet.Types
-- Copyright   :  (c) Hal Daume III 2003-2004
-- License     :  BSD-style
--
-- Maintainer  :  hdaume@isi.edu
-- Stability   :  experimental
-- Portability :  non-portable (H98 + implicit parameters)
--
-- This module is maintained at:
--    <http://www.isi.edu/~hdaume/HWordNet/>.
--
-- This module describes the types used in the purely functional interface.
--
-- More information about WordNet is available at:
--    <http://http://www.cogsci.princeton.edu/~wn/>.
-------------------------------------------------------------------------------
module NLP.WordNet.Types
    (
     -- * The wrapper type for wordnet functions.
     WN(),

     -- * The basic Word type (just a 'String').
     Word,

     -- * The part of speech type.
     POS(..),

     -- * The type, and functions dealing with overview searches.
     Overview(),
       numNounSenses,
       numVerbSenses,
       numAdjSenses,
       numAdvSenses,
       taggedCountNounSenses, 
       taggedCountVerbSenses, 
       taggedCountAdjSenses, 
       taggedCountAdvSenses,

     -- * The type, and functions dealing with the word net environment.
     WordNetEnv(),
       getReleaseVersion,
       getDataDirectory,

     -- The type, and functions dealing with senses.
--     SenseKey(),
--       senseKeyPOS,
--       senseKeyWord,

     -- * The type to control which sense a search is looking at.
     SenseType(..),

     -- * The type, and functions dealing with search results.
     SearchResult(),
       srOverview,
       srSenseNum,
       srPOS,
       srDefinition,
       srSenses,
       srWords,
       srForms,
       srFormKeys,
       srToKey,

     -- The type, and functions dealing with indices.
--     Index(),
--       indexWord,
--       indexPOS,

     -- * A sum type of the different relations which can hold between words.
     Form(..),

     -- * A simple key into the database.
     Key(),
    )
    where

import System.IO.Unsafe
import Data.List
import Data.Maybe

import NLP.WordNet.PrimTypes
import NLP.WordNet.Util

-- | In actuality this type is:
--
-- > type WN a = (?wne :: WordNetEnv) => a
-- 
-- but Haddock cannot parse this at this time.
-- type WN a = a
type WN a = (?wne :: WordNetEnv) => a

-- | A Word is just a String.
type Word = String

-- basically, all we do is wrap the SenseKey, Synset and WordNetEnv
-- datatypes in a bunch of useful functions

-- | This will give you the current release of the WordNet databases
-- we are using (if we know).
getReleaseVersion :: WN (Maybe String)
getReleaseVersion = wnReleaseVersion ?wne

-- | This will give you the directory from which the databases are being read.
getDataDirectory :: WN FilePath
getDataDirectory = dataDirectory ?wne


-- | Given an 'Overview', this will tell you how many noun senses the searched-for word has.
numNounSenses :: Overview -> Int
numNounSenses = maybe 0 indexSenseCount . nounIndex

-- | Given an 'Overview', this will tell you how many verb senses the searched-for word has.
numVerbSenses :: Overview -> Int
numVerbSenses = maybe 0 indexSenseCount . verbIndex

-- | Given an 'Overview', this will tell you how many adjective senses the searched-for word has.
numAdjSenses :: Overview -> Int
numAdjSenses  = maybe 0 indexSenseCount . adjIndex

-- | Given an 'Overview', this will tell you how many adverb senses the searched-for word has.
numAdvSenses :: Overview -> Int
numAdvSenses  = maybe 0 indexSenseCount . advIndex


-- | Given an 'Overview', this will tell you how many times this word was tagged as a noun.
taggedCountNounSenses :: Overview -> Int
taggedCountNounSenses = maybe 0 indexTaggedCount . nounIndex

-- | Given an 'Overview', this will tell you how many times this word was tagged as a verb.
taggedCountVerbSenses :: Overview -> Int
taggedCountVerbSenses = maybe 0 indexTaggedCount . verbIndex

-- | Given an 'Overview', this will tell you how many times this word was tagged as an adjective.
taggedCountAdjSenses :: Overview -> Int
taggedCountAdjSenses  = maybe 0 indexTaggedCount . adjIndex

-- | Given an 'Overview', this will tell you how many times this word was tagged as an adverb.
taggedCountAdvSenses :: Overview -> Int
taggedCountAdvSenses  = maybe 0 indexTaggedCount . advIndex

-- | This gives the part of speech of a 'SearchResult'
srPOS :: SearchResult -> POS
srPOS sr = case pos (srSynset sr) of { POS p -> p; _ -> Adj }

-- | This gives the definition of the sense of a word in a 'SearchResult'.
srDefinition :: SearchResult -> String
srDefinition = defn . srSynset

-- | This gives a list of senses the word has.
srSenses :: SearchResult -> [SenseType]
srSenses = nub . map thr3 . ssWords . srSynset

-- | This gives the actual words used to describe the Synset of a search result.
srWords :: SearchResult -> SenseType -> [Word]
srWords sr t = nub . map fst3 . filter ((isType t) . thr3) . ssWords . srSynset $ sr
  where isType AllSenses _ = True
        isType _ AllSenses = True
        isType (SenseNumber n) (SenseNumber m) = n == m

-- | This gives all the 'Form's a word has (i.e., what sort of relations hold between
-- it and other words.
srForms :: SearchResult -> [Form]
srForms = nub . map (\ (f,_,_,_,_) -> f) . forms . srSynset

-- | This provides a 'Key' (which can be searched for using 'lookupKey') for
-- a 'SearchResult' under a given form.  For instance, it can be used to
-- get all 'Hypernym's of a given word.
srFormKeys :: SearchResult -> Form -> [Key]
srFormKeys sr f = nub . mapMaybe mkKey . filter (\ (f',_,_,_,_) -> f == f') . forms . srSynset $ sr
  where mkKey (_,o,POS p,_,_) = Just $ Key (o,p)
        mkKey (_,o,_,_,_)     =
          case srSenseKey sr of
            Nothing -> Nothing
            Just sk -> Just $ Key (o, senseKeyPOS sk)

-- | This converts a 'SearchResult' into a 'Key'.
srToKey :: SearchResult -> Key  -- returns the key associated with this search result
srToKey sr = Key (hereIAm $ srSynset sr, p)
  where p = case srSenseKey sr of
              Just sk -> senseKeyPOS sk
              Nothing -> case pos (srSynset sr) of
                           POS pp -> pp
                           _ -> case ssType (srSynset sr) of
                                  POS pp -> pp
                                  _      -> Adj

instance Show SearchResult where
  showsPrec i (SearchResult { srSynset = ss }) =
    showChar '<' . showString (unwords $ map fst3 $ ssWords ss) . showChar '>'
