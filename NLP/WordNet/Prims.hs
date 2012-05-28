-- | NLP.WordNet.Prims provides primitive operations over the word net database.
-- The general scheme of things is to call 'initializeWordNet' to get a 'WordNetEnv'.
-- Once you have this, you can start querying.  A query usually looks like (suppose
-- we want "Dog" as a Noun:
--
-- 'getIndexString' on "Dog".  This will give us back a cannonicalized string, in this
-- case, still "dog".  We then use 'indexLookup' to get back an index for this string.
-- Then, we call 'indexToSenseKey' to with the index and the sense number (the Index
-- contains the number of senses) to get back a SenseKey.  We finally call
-- 'getSynsetForSense' on the sense key to get back a Synset.
--
-- We can continue to query like this or we can use the offsets provided in the
-- various fields of the Synset to query directly on an offset.  Given an offset
-- and a part of speech, we can use 'readSynset' directly to get a synset (instead
-- of going through all this business with indices and sensekeys.
module NLP.WordNet.Prims
    (
     initializeWordNet,
     initializeWordNetWithOptions,
     closeWordNet,
     getIndexString,
     getSynsetForSense,
     readSynset,
     indexToSenseKey,
     indexLookup
    )
    where

import System.IO -- hiding (try, catch)
import System.Environment
import Numeric (readHex, readDec)
import Data.Char (toLower, isSpace)
import Data.Array
import Control.OldException
import Control.Monad (when, liftM, mplus)
import Data.List (findIndex, find)
import Data.Maybe (isNothing, fromJust, isJust, fromMaybe)
import GHC.Handle -- (openFileEx, BinaryMode(..))

import NLP.WordNet.PrimTypes
import NLP.WordNet.Util
import NLP.WordNet.Consts

-- | initializeWordNet looks for the word net data files in the
-- default directories, starting with environment variables WNSEARCHDIR
-- and WNHOME, and then falling back to 'defaultPath' as defined in
-- NLP.WordNet.Consts.
initializeWordNet :: IO WordNetEnv
initializeWordNet =
  initializeWordNetWithOptions Nothing Nothing


-- | initializeWordNetWithOptions looks for the word net data files in the
-- specified directory.  Use this if wordnet is installed in a non-standard
-- place on your machine and you don't have the appropriate env vars set up.
initializeWordNetWithOptions :: 
    Maybe FilePath ->                          -- word net data directory
    Maybe (String -> Exception -> IO ()) ->    -- "warning" function (by default, warnings go to stderr)
      IO WordNetEnv
initializeWordNetWithOptions mSearchdir mWarn = do
  searchdir <- case mSearchdir of { Nothing -> getDefaultSearchDir ; Just d -> return d }
  let warn = fromMaybe (\s e -> hPutStrLn stderr (s ++ "\n" ++ show e)) mWarn
  version <- tryMaybe $ getEnv "WNDBVERSION"
  dHands <- mapM (\pos -> do
                  idxH  <- openFileEx
                             (makePath [searchdir, "index." ++ partName pos])
                             (BinaryMode ReadMode)
                  dataH <- openFileEx 
                             (makePath [searchdir, "data." ++ partName pos])
                             (BinaryMode ReadMode)
                  return (idxH, dataH)
                 ) allPOS
  -- the following are unnecessary
  sense   <- tryMaybeWarn (warn "Warning: initializeWordNet: cannot open file index.sense")
                        $ openFileEx (makePath [searchdir, "index.sense"]) (BinaryMode ReadMode)
  cntlst  <- tryMaybeWarn (warn "Warning: initializeWordNet: cannot open file cntlist.rev")
                        $ openFileEx (makePath [searchdir, "cntlist.rev"]) (BinaryMode ReadMode)
  keyidx  <- tryMaybeWarn (warn "Warning: initializeWordNet: cannot open file index.key")
                        $ openFileEx (makePath [searchdir, "index.key"  ]) (BinaryMode ReadMode)
  rkeyidx <- tryMaybeWarn (warn "Warning: initializeWordNet: cannot open file index.key.rev")
                        $ openFileEx (makePath [searchdir, "index.key.rev"]) (BinaryMode ReadMode)
  vsent   <- tryMaybeWarn (warn "Warning: initializeWordNet: cannot open sentence files (sentidx.vrb and sents.vrb)")
                        $ do
               idx <- openFileEx (makePath [searchdir, "sentidx.vrb"]) (BinaryMode ReadMode)
               snt <- openFileEx (makePath [searchdir, "sents.vrb"  ]) (BinaryMode ReadMode)
               return (idx, snt)
  mHands <- mapM (\pos -> openFileEx 
                            (makePath [searchdir, partName pos ++ ".exc"])
                            (BinaryMode ReadMode)) allPOS
  return $ WordNetEnv 
              { dataHandles = listArray (Noun, Adv) dHands,
                excHandles  = listArray (Noun, Adv) mHands,
                senseHandle = sense,
                countListHandle = cntlst,
                keyIndexHandle = keyidx,
                revKeyIndexHandle = rkeyidx,
                vSentHandle = vsent,
                wnReleaseVersion = version,
                dataDirectory = searchdir,
                warnAbout = warn
              }
  where
    getDefaultSearchDir = do
      Just searchdir <- tryMaybe (getEnv "WNSEARCHDIR") >>= \m1 ->
                        tryMaybe (getEnv "WNHOME") >>= \m2 ->
                        return (m1 `mplus` 
                                liftM (++dictDir) m2 `mplus` 
                                Just defaultPath)
      return searchdir

-- | closeWordNet is not strictly necessary.  However, a 'WordNetEnv' tends to
-- hog a few Handles, so if you run out of Handles and won't be using
-- your WordNetEnv for a while, you can close it and always create a new
-- one later.
closeWordNet :: WordNetEnv -> IO ()
closeWordNet wne = do
  mapM_ (\ (h1,h2) -> hClose h1 >> hClose h2) (elems (dataHandles wne))
  mapM_ hClose (elems (excHandles wne))
  mapM_ (\x -> when (isJust x) $ hClose (fromJust x))
        [senseHandle wne, countListHandle wne, keyIndexHandle wne,
         revKeyIndexHandle wne, liftM fst (vSentHandle wne), liftM snd (vSentHandle wne)]

-- | getIndexString takes a string and a part of speech and tries to find
-- that string (or something like it) in the database.  It is essentially
-- a cannonicalization routine and should be used before querying the
-- database, to ensure that your string is in the right form.
getIndexString :: WordNetEnv -> String -> POS -> IO (Maybe String)
getIndexString wne str partOfSpeech = getIndexString' . cannonWNString $ str
  where
    getIndexString' [] = return Nothing
    getIndexString' (s:ss) = do
      i <- binarySearch (fst (dataHandles wne ! partOfSpeech)) s
      if isJust i
        then return (Just s)
        else getIndexString' ss

-- | getSynsetForSense takes a sensekey and finds the appropriate Synset.  SenseKeys can
-- be built using indexToSenseKey.
getSynsetForSense :: WordNetEnv -> SenseKey -> IO (Maybe Synset)
getSynsetForSense wne _ | isNothing (senseHandle wne) = ioError $ userError "no sense dictionary"
getSynsetForSense wne key = do
  l <- binarySearch
         (fromJust $ senseHandle wne)
         (senseKeyString key) -- ++ " " ++ charForPOS (senseKeyPOS key))
  case l of
    Nothing -> return Nothing
    Just l  -> do offset <- maybeRead $ takeWhile (not . isSpace) $
                               drop 1 $ dropWhile (not . isSpace) l
                  ss <- readSynset wne (senseKeyPOS key) offset (senseKeyWord key)
                  return (Just ss)

-- | readSynset takes a part of speech, and an offset (the offset can be found
-- in another Synset) and (perhaps) a word we're looking for (this is optional)
-- and will return its Synset.
readSynset :: WordNetEnv -> POS -> Offset -> String -> IO Synset
readSynset wne searchPos offset w = do
  let h = snd (dataHandles wne ! searchPos)
  hSeek h AbsoluteSeek offset
  toks <- liftM words $ hGetLine h
  --print toks
  (ptrTokS:fnumS:posS:numWordsS:rest1) <- matchN 4 toks
  hiam <- maybeRead ptrTokS
  fn   <- maybeRead fnumS
  let ss1 = synset0 { hereIAm = hiam,
                      pos = readEPOS posS,
                      fnum = fn,
                      ssType = if readEPOS posS == Satellite then IndirectAnt else UnknownEPos
                   }
  let numWords = case readHex numWordsS of
                   (n,_):_ -> n
                   _       -> 0
--read numWordsS
  let (wrds,ptrCountS:rest2) = splitAt (numWords*2) rest1  -- words and lexids
  let ptrCount = 
        case readDec ptrCountS of
          (n,_):_ -> n
          _       -> 0
--  print (toks, ptrCountS, ptrCount)
  wrds' <- readWords ss1 wrds
  let ss2 = ss1 { ssWords = wrds',
                  whichWord = findIndex (==w) wrds }
  let (ptrs,rest3) = splitAt (ptrCount*4) rest2
  let (fp,ss3) = readPtrs (False,ss2) ptrs
  let ss4 = if fp && searchPos == Adj && ssType ss3 == UnknownEPos
              then ss3 { ssType = Pertainym }
              else ss3
  let (ss5,rest4) = 
        if searchPos /= Verb 
          then (ss4, rest3) 
          else let (fcountS:rest4) = rest3
                   (synPtrs, rest5) = splitAt (read fcountS * 3) rest4
               in  (ss4, rest5)

  let ss6 = ss5 { defn = unwords $ drop 1 rest4 }

  return ss6
  where
    readWords ss (w:lid:xs) = do
      let s = map toLower $ replaceChar ' ' '_' w
      idx  <- indexLookup wne s (fromEPOS $ pos ss)
--      print (w,st,idx)
      let posn = case idx of
                   Nothing -> Nothing
                   Just ix -> findIndex (==hereIAm ss) (indexOffsets ix)
      rest <- readWords ss xs
      return ((w, fst $ head $ readHex lid, maybe AllSenses SenseNumber posn) : rest)
    readWords _ _ = return []
    readPtrs (fp,ss) (typ:off:ppos:lexp:xs) = 
      let (fp',ss') = readPtrs (fp,ss) xs
          this = (getPointerType typ,
                  read off,
                  readEPOS ppos,
                  fst $ head $ readHex (take 2 lexp),
                  fst $ head $ readHex (drop 2 lexp))
      in  if searchPos == Adj && ssType ss' == UnknownEPos
            then if getPointerType typ == Antonym
                   then (fp' , ss' { forms = this : forms ss',
                                     ssType   = DirectAnt })
                   else (True, ss' { forms = this : forms ss' })
            else (fp', ss' { forms = this : forms ss' })
    readPtrs (fp,ss) _ = (fp,ss)

-- | indexToSenseKey takes an Index (as returned by, ex., indexLookup) and a sense
-- number and returns a SenseKey for that sense.
indexToSenseKey :: WordNetEnv -> Index -> Int -> IO (Maybe SenseKey)
indexToSenseKey wne idx sense = do
  let cpos = fromEPOS $ indexPOS idx
  ss1 <- readSynset wne cpos (indexOffsets idx !! (sense-1)) ""
  ss2 <- followSatellites ss1
  --print ss2
  case findIndex ((==indexWord idx) . map toLower) (map (\ (w,_,_) -> w) $ ssWords ss2) of
    Nothing -> return Nothing
    Just  j -> do
      let skey = if ssType ss2 == Satellite
                   then indexWord idx ++ "%" ++ show (fromEnum Satellite) ++ ":" ++
                        padTo 2 (show $ fnum ss2) ++ ":" ++ headWord ss2 ++ ":" ++
                        padTo 2 (show $ headSense ss2)
                   else indexWord idx ++ "%" ++ show (fromEnum $ pos ss2) ++ ":" ++
                        padTo 2 (show $ fnum ss2) ++ ":" ++ 
                        padTo 2 (show $ lexId ss2 j) ++ "::"
      return (Just $ SenseKey cpos skey (indexWord idx))
  where
    followSatellites ss 
        | ssType ss == Satellite =
            case find (\ (f,_,_,_,_) -> f == Similar) (forms ss) of
              Nothing -> return ss
              Just (f,offset,p,j,k) -> do
                adjss <- readSynset wne (fromEPOS p) offset ""
                case ssWords adjss of
                  (hw,_,hs):_ -> return (ss { headWord  = map toLower hw,
                                              headSense = hs })
                  _ -> return ss
        | otherwise = return ss

-- indexLookup takes a word and part of speech and gives back its index.
indexLookup :: WordNetEnv -> String -> POS -> IO (Maybe Index)
indexLookup wne w pos = do
  ml <- binarySearch (fst (dataHandles wne ! pos)) w
  case ml of
    Nothing -> return Nothing
    Just  l -> do
      (wdS:posS:ccntS:pcntS:rest1) <- matchN 4 (words l)
      isc <- maybeRead ccntS
      pc  <- maybeRead pcntS
      let idx1 = index0 { indexWord = wdS,
                          indexPOS  = readEPOS posS,
                          indexSenseCount = isc
                        }
      let (ptrs,rest2) = splitAt pc rest1
      let idx2 = idx1 { indexForms = map getPointerType ptrs }
      (ocntS:tcntS:rest3) <- matchN 2 rest2
      itc <- maybeRead tcntS
      otc <- maybeRead ocntS
      let idx3 = idx2 { indexTaggedCount = itc }
      let (offsets,_) = splitAt otc rest3
      io <- mapM maybeRead offsets
      return (Just $ idx3 { indexOffsets = io })

-- do binary search on an index file
binarySearch :: Handle -> String -> IO (Maybe String)
binarySearch h s = do
  hSeek h SeekFromEnd 0
  bot <- hTell h
  binarySearch' 0 bot (bot `div` 2)
  where
    binarySearch' :: Integer -> Integer -> Integer -> IO (Maybe String)
    binarySearch' top bot mid = do
      hSeek h AbsoluteSeek (mid-1)
      when (mid /= 1) readUntilNL
      eof <- hIsEOF h
      if eof 
        then if top >= bot-1 
               then return Nothing
               else binarySearch' top (bot-1) ((top+bot-1) `div` 2)
        else do
          l <- hGetLine h
          let key = takeWhile (/=' ') l
          if key == s 
            then return (Just l)
            else case (bot - top) `div` 2 of
                   0 -> return Nothing
                   d -> case key `compare` s of
                          LT -> binarySearch' mid bot (mid + d)
                          GT -> binarySearch' top mid (top + d)
    readUntilNL = do
      eof <- hIsEOF h
      if eof 
        then return ()
        else do hGetLine h; return ()
