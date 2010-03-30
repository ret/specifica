-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Pos
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Parsec compatibility module
-- 
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Pos
    ( SourceName,
      Line,
      Column,
-- NOTE kramer@acm.org reto -- export SourcePos constructor, 
-- previosuly opaque SourcePos type was exported only.
      SourcePos(SourcePos),
      sourceLine,
      sourceColumn,
      sourceName,
      incSourceLine,
      incSourceColumn,
      setSourceLine,
      setSourceColumn,
      setSourceName,
      newPos,
      initialPos,
      updatePosChar,
      updatePosString
    ) where
      

import Text.Parsec.Pos
