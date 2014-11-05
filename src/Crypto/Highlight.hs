module Crypto.Highlight where

import qualified Data.Foldable                as F
import           Data.Maybe
import           Text.PrettyPrint.ANSI.Leijen

-- Lookup characters and highlight found ones in blue.
substHL :: [(Char, Char)] -> Char -> Doc
substHL t c = maybe (char c) (blue . char) $ lookup c t

substCharsHL :: F.Foldable f => [(Char, Char)] -> f Char -> Doc
substCharsHL = F.foldMap . substHL

