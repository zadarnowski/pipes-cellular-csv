> -- | Module:    Pipes.CSV.Syntax
> -- Description: Basic elements of the CSV lexical syntax
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- CSV syntax elements as defined in RFC 4180.

> {-# LANGUAGE PatternSynonyms, ScopedTypeVariables #-}

> module Pipes.CSV.Syntax (
>   pattern LF,
>   pattern CR,
>   pattern QC,
>   pattern CC,
>   isSpecial,
>   fieldDelimiter,
>   fieldSeparator,
>   recordSeparator,
>   quoteSequence
> ) where

> import Data.ByteString (ByteString)
> import Data.Word

> import qualified Data.ByteString as ByteString

> -- | line byte value
> pattern LF = 0x0A :: Word8

> -- | carriage return byte value
> pattern CR = 0x0D :: Word8

> -- | quote character byte value
> pattern QC = 0x22 :: Word8

> -- | comma character byte value
> pattern CC = 0x2C :: Word8

> -- | Identifies special byte values, which should never appear within unquoted CSV fields.
> isSpecial :: Word8 -> Bool
> isSpecial x = (x == QC || x == CC || x < 0x20 || x > 0x7E)

> -- | The field delimiter string (a quote character.)
> fieldDelimiter :: ByteString
> fieldDelimiter = ByteString.singleton QC

> -- | The field separator string (a comma character.)
> fieldSeparator :: ByteString
> fieldSeparator = ByteString.singleton CC

> -- | The field separator (CR+LF byte sequence.)
> recordSeparator :: ByteString
> recordSeparator = ByteString.pack [ CR, LF ]

> -- | An escaped quote string (double quote character.)
> quoteSequence :: ByteString
> quoteSequence = ByteString.pack [ QC, QC ]

