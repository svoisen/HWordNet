-----------------------------------------------------------------------------
-- |
-- Module      :  NLP.WordNet
-- Copyright   :  (c) Hal Daume III 2003-2004
-- License     :  BSD-style
--
-- Maintainer  :  hdaume@isi.edu
-- Stability   :  experimental
-- Portability :  non-portable (H98 + implicit parameters)
--
-- This is the top level module to the Haskell WordNet interface.
--
-- This module is maintained at:
--    <http://www.isi.edu/~hdaume/HWordNet/>.
--
-- This is the only module in the WordNet package you need to import.
-- The others provide utility functions and primitives that this
-- module is based on.
--
-- More information about WordNet is available at:
--    <http://http://www.cogsci.princeton.edu/~wn/>.
-----------------------------------------------------------------------------
module NLP.WordNet
    (
     -- * The basic type system
     module NLP.WordNet.Types,

     -- * Top level execution functions
     runWordNet,
     runWordNetQuiet,
     runWordNetWithOptions,

     -- * Functions to manually initialize the WordNet system; these are not
     --   needed if you use one of the "runWordNet" functions above.
     initializeWordNet,
     initializeWordNetWithOptions,
     closeWordNet,
     runs,

     -- * The basic database access functions.
     getOverview,
     searchByOverview,
     search,
     lookupKey,

     -- * The agglomeration functions
     relatedBy,
     closure,
     closureOn,

     -- * Computing lowest-common ancestor functions; the implementation
     --   of these can be tuned by providing a different "Bag" implementation.
     --   use "emptyQueue" for breadth-first-search (recommended) or "emptyStack"
     --   for depth-first-search, or write your own.
     meet,
     meetPaths,
     meetSearchPaths,
     Bag(..),
     emptyQueue,
     emptyStack,
    )
    where

import Prelude hiding (catch)
import Data.Array
import GHC.Arr (unsafeIndex)
import GHC.Handle
import Data.Tree
import Data.IORef
import Data.Dynamic
import qualified Data.Set as Set
import Numeric (readHex, readDec)
import System.IO.Unsafe

import NLP.WordNet.Common
import NLP.WordNet.Consts
import NLP.WordNet.Util
import NLP.WordNet.Types

import qualified NLP.WordNet.PrimTypes as T
import qualified NLP.WordNet.Prims     as P

-- | Takes a WordNet command, initializes the environment
-- and returns the results in the 'IO' monad.  WordNet
-- warnings are printed to stderr.
runWordNet :: WN a -> IO a
runWordNet = runWordNetWithOptions Nothing Nothing

-- | Takes a WordNet command, initializes the environment
-- and returns the results in the 'IO' monad.  WordNet
-- warnings are ignored.
runWordNetQuiet :: WN a -> IO a
runWordNetQuiet = runWordNetWithOptions Nothing (Just (\_ _ -> return ()))

-- | Takes a FilePath to the directory holding WordNet and
-- a function to do with warnings and a WordNet command, initializes 
-- the environment and returns the results in the 'IO' monad.
runWordNetWithOptions :: 
    Maybe FilePath ->                          -- word net data directory
    Maybe (String -> Exception -> IO ()) ->    -- warning function (by default, warnings go to stderr)
    WN a ->                                    -- what to run
      IO a
runWordNetWithOptions dd warn wn = do
  wne <- P.initializeWordNetWithOptions dd warn
  let a = let ?wne = wne in wn
  -- P.closeWordNet wne
  return a

-- | Gives you a 'WordNetEnv' which can be passed to 'runs' or used
-- as the implicit parameter to the other WordNet functions.
initializeWordNet :: IO WordNetEnv
initializeWordNet = P.initializeWordNet

-- | Takes a FilePath to the directory holding WordNet and
-- a function to do with warnings, initializes 
-- the environment and returns a 'WordNetEnv' as in 'initializeWordNet'.
initializeWordNetWithOptions :: Maybe FilePath -> Maybe (String -> Exception -> IO ()) -> IO WordNetEnv
initializeWordNetWithOptions = P.initializeWordNetWithOptions

-- | Closes all the handles associated with the 'WordNetEnv'.  Since
-- the functions provided in the "NLP.WordNet.WordNet" module
-- are /lazy/, you shouldn't do this until you're really done.
-- Or perhaps not at all (GC will eventually kick in).
closeWordNet :: WordNetEnv -> IO ()
closeWordNet = P.closeWordNet

-- | This simply takes a 'WordNetEnv' and provides it as the
-- implicit parameter to the WordNet command.
runs :: WordNetEnv -> WN a -> a
runs wne x = let ?wne = wne in x


-- | This takes a word and returns an 'Overview' of all its senses
-- for all parts of speech.
getOverview :: WN (Word -> Overview)
getOverview word = unsafePerformIO $ do
  idxN <- unsafeInterleaveIO $ getOverview' Noun
  idxV <- unsafeInterleaveIO $ getOverview' Verb
  idxA <- unsafeInterleaveIO $ getOverview' Adj
  idxR <- unsafeInterleaveIO $ getOverview' Adv
  return (T.Overview idxN idxV idxA idxR)
  where
    getOverview' pos = do
      strM <- P.getIndexString ?wne word pos
      case strM of
        Nothing -> return Nothing
        Just  s -> unsafeInterleaveIO $ P.indexLookup ?wne word pos

-- | This takes an 'Overview' (see 'getOverview'), a 'POS' and a 'SenseType' and returns
-- a list of search results.  If 'SenseType' is 'AllSenses', there will be one
-- 'SearchResult' in the results for each valid sense.  If 'SenseType' is
-- a single sense number, there will be at most one element in the result list.
searchByOverview :: WN (Overview -> POS -> SenseType -> [SearchResult])
searchByOverview overview pos sense = unsafePerformIO $ 
  case (case pos of { Noun -> T.nounIndex ; Verb -> T.verbIndex ; Adj -> T.adjIndex ; Adv -> T.advIndex })
          overview of
    Nothing  -> return []
    Just idx -> do
      let numSenses = T.indexSenseCount idx
      skL <- mapMaybe id `liftM` 
             unsafeInterleaveIO (
               mapM (\sense -> do
                     skey <- P.indexToSenseKey ?wne idx sense
                     return (liftM ((,) sense) skey)
                    ) (sensesOf numSenses sense)
             )
      r <- unsafeInterleaveIO $ mapM (\ (snum, skey) ->
                 unsafeInterleaveIO (P.getSynsetForSense ?wne skey) >>= \v ->
                 case v of
                   Nothing -> return Nothing
                   Just ss -> return $ Just (T.SearchResult 
                                               (Just skey) 
                                               (Just overview) 
                                               (Just idx)
                                               (Just (SenseNumber snum)) 
                                               ss)
                ) skL
      return (mapMaybe id r)

-- | This takes a 'Word', a 'POS' and a 'SenseType' and returns
-- the equivalent of first running 'getOverview' and then 'searchByOverview'.
search :: WN (Word -> POS -> SenseType -> [SearchResult])
search word pos sense = searchByOverview (getOverview word) pos sense

-- | This takes a 'Key' (see 'srToKey' and 'srFormKeys') and looks it
-- up in the databse.
lookupKey :: WN (Key -> SearchResult)
lookupKey (T.Key (o,p)) = unsafePerformIO $ do
  ss <- unsafeInterleaveIO $ P.readSynset ?wne p o ""
  return $ T.SearchResult Nothing Nothing Nothing Nothing ss

-- | This takes a 'Form' and a 'SearchResult' and returns all
-- 'SearchResult' related to the given one by the given 'Form'.
--
-- For example:
--
-- > relatedBy Antonym (head (search "happy" Adj 1))
-- > [<unhappy>]
-- >
-- > relatedBy Hypernym (head (search "dog" Noun 1))
-- > [<canine canid>]
relatedBy :: WN (Form -> SearchResult -> [SearchResult])
relatedBy form sr = map lookupKey $ srFormKeys sr form

-- | This is a utility function to build lazy trees from a function and a root.
closure :: (a -> [a]) -> a -> Tree a
closure f x = Node x (map (closure f) $ f x)

-- | This enables 'Form'-based trees to be built.
--
-- For example:
--
-- > take 5 $ flatten $ closureOn Antonym (head (search "happy" Adj AllSenses)))
-- > [<happy>,<unhappy>,<happy>,<unhappy>,<happy>]
--
-- > closureOn Hypernym (head (search "dog" Noun 1)))
-- > - <dog domestic_dog Canis_familiaris> --- <canine canid> --- <carnivore>\\
-- >   --- <placental placental_mammal eutherian eutherian_mammal> --- <mammal>\\
-- >   --- <vertebrate craniate> --- <chordate> --- <animal animate_being beast\\
-- >   brute creature fauna> --- <organism being> --- <living_thing animate_thing>\\
-- >   --- <object physical_object> --- <entity> 
closureOn :: WN (Form -> SearchResult -> Tree SearchResult)
closureOn form = closure (relatedBy form)

-- | A simple bag class for our 'meet' implementation.
class Bag b a where
  emptyBag :: b a
  addToBag :: b a -> a -> b a
  addListToBag :: b a -> [a] -> b a
  isEmptyBag :: b a -> Bool
  splitBag :: b a -> (a, b a)
  addListToBag = foldr (flip addToBag)

instance Bag [] a where
  emptyBag = []
  addToBag = flip (:)
  isEmptyBag = null
  splitBag (x:xs) = (x, xs)

-- | A very slow queue based on lists.
newtype Queue a = Queue [a] deriving (Show)

instance Bag Queue a where
  emptyBag = Queue []
  addToBag (Queue l) a = Queue (l++[a])
  isEmptyBag (Queue l) = null l
  splitBag (Queue (x:xs)) = (x, Queue xs)
  addListToBag (Queue l) l' = Queue (l ++ l')

-- | An empty stack.
emptyStack :: [a]
emptyStack = []

-- | An empty queue.
emptyQueue :: Queue a
emptyQueue = Queue []

-- | This function takes an empty bag (in particular, this is to specify
-- what type of search to perform), and the results of two search.
-- It returns (maybe) the lowest point at which the two terms
-- meet in the WordNet hierarchy.
--
-- For example:
--
-- > meet emptyQueue (head $ search "cat" Noun 1) (head $ search "dog" Noun 1)
-- > Just <carnivore>
--
-- > meet emptyQueue (head $ search "run" Verb 1) (head $ search "walk" Verb 1)
-- > Just <travel go move locomote>
meet :: Bag b (Tree SearchResult) => WN (b (Tree SearchResult) -> SearchResult -> SearchResult -> Maybe SearchResult)
meet emptyBg sr1 sr2 = srch Set.empty Set.empty (addToBag emptyBg t1) (addToBag emptyBg t2)
  where
    t1 = closureOn Hypernym sr1
    t2 = closureOn Hypernym sr2

    srch v1 v2 bag1 bag2
        | isEmptyBag bag1 && isEmptyBag bag2 = Nothing
        | isEmptyBag bag1                    = srch v2 v1 bag2 bag1
        | otherwise = 
            let (Node sr chl, bag1') = splitBag bag1
            in  if v2 `containsResult` sr 
                  then Just sr
                  else srch v2 (addResult v1 sr) bag2 (addListToBag bag1' chl) -- flip the order :)

    containsResult v sr = srWords sr AllSenses `Set.member` v
    addResult v sr = Set.insert (srWords sr AllSenses) v

-- | This function takes an empty bag (see 'meet'), and the results of two searches.
-- It returns (maybe) the lowest point at which the two terms
-- meet in the WordNet hierarchy, as well as the paths leading from each
-- term to this common term.
--
-- For example:
--
-- > meetPaths emptyQueue (head $ search "cat" Noun 1) (head $ search "dog" Noun 1)
-- > Just ([<cat true_cat>,<feline felid>],<carnivore>,[<canine canid>,<dog domestic_dog Canis_familiaris>])
--
-- > meetPaths emptyQueue (head $ search "run" Verb 1) (head $ search "walk" Verb 1)
-- > Just ([<run>,<travel_rapidly speed hurry zip>],<travel go move locomote>,[<walk>])
--
-- This is marginally less efficient than just using 'meet', since it uses
-- linear-time lookup for the visited sets, whereas 'meet' uses log-time
-- lookup.
meetPaths :: Bag b (Tree SearchResult) => 
             WN (
                 b (Tree SearchResult) ->       -- bag implementation
                 SearchResult ->                -- word 1
                 SearchResult ->                -- word 2
                 Maybe ([SearchResult], SearchResult, [SearchResult]))   -- word 1 -> common,
                                                                         -- common
                                                                         -- common -> word 2
meetPaths emptyBg sr1 sr2 = meetSearchPaths emptyBg t1 t2
  where
    t1 = closureOn Hypernym sr1
    t2 = closureOn Hypernym sr2

meetSearchPaths emptyBg t1 t2 =
  let srch b v1 v2 bag1 bag2
        | isEmptyBag bag1 && isEmptyBag bag2 = Nothing
        | isEmptyBag bag1                    = srch (not b) v2 v1 bag2 bag1
        | otherwise = 
            let (Node sr chl, bag1') = splitBag bag1
                sl = srWords sr AllSenses
            in  if v2 `containsResult` sl
                  then Just $ if b 
                                then (reverse v1, sr, drop 1 $ dropWhile ((/=sl) . flip srWords AllSenses) v2) 
                                else (reverse $ drop 1 $ dropWhile ((/=sl) . flip srWords AllSenses) v2, sr, v1)
                  else srch (not b)
                            v2 (addResult v1 sr) 
                            bag2 (addListToBag bag1' chl) -- flip the order :)
  in  srch True [] [] (addToBag emptyBg t1) (addToBag emptyBg t2)
  where
    containsResult v sl = sl `elem` map (flip srWords AllSenses) v
    addResult v sr = sr:v

personTree       = runWordNetQuiet (closureOn Hypernym (head $ search "person"       Noun AllSenses))
organizationTree = runWordNetQuiet (closureOn Hypernym (head $ search "organization" Noun AllSenses))

