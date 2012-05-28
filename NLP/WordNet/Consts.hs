module NLP.WordNet.Consts where

import Data.List (intersperse)
import System.FilePath (joinPath)

makePath = joinPath

#if defined (UNIX)
dictDir         = "/dict"
defaultPath     = "/usr/local/WordNet-2.0/dict"
defaultBin      = "/usr/local/WordNet-2.0/bin"

#elif defined (PC)
dictDir         = "\\dict"
defaultPath	= "c:\\WordNet 2.0\\dict"
defaultBin      = "c:\\WordNet 2.0\\bin"

#elif defined (MAC)
dictDir         = ":Database"
defaultPath     = ":Database"
defaultBin      = ":"

#else
-- guess unix style
dictDir         = "/dict"
defaultPath     = "/usr/local/WordNet-2.0/dict"
defaultBin      = "/usr/local/WordNet-2.0/bin"

#endif
