> -- | Module:    Pipes.CSV.Encoder
> -- Description: Streaming CSV encoder
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- Streaming CSV file decoder compliant with RFC 4180.
> -- All fields are quoted except for short fields consisting
> -- entirely of “safe” ASCII characters, i.e., printable 7-bit
> -- characters other than quote and comma. The maximum length
> -- of an unquoted field is supplied explicitly to the encoder,
> -- which allows us to decide whether a given field requires
> -- quoting without unbounded lookahead.

> module Pipes.CSV.Encoder (
>   encodeCSV, encodeLazyCSV
> ) where

> import Control.Monad
> import Data.ByteString (ByteString)
> import Data.Cell
> import Pipes
> import Pipes.CSV.Syntax

> import qualified Data.ByteString as ByteString
> import qualified Data.ByteString.Lazy as Lazy

> -- | @encodeCSV n@ is an infinite pipe that converts a stream
> --   of cells into a fragmented strict representation of of a CSV file,
> --   unconditionally quoting any field values with length greater than @n@.

> encodeCSV :: Monad m => Int -> Pipe (Cell ByteString) ByteString m ()
> encodeCSV n = encode1
>  where

>   encode1 = await >>= encode2

>   encode2 (Cell x EOP) = yield fieldDelimiter >> quotePart x >> await >>= encode3
>   encode2 (Cell x EOC) = quoteField x >> yield fieldSeparator >> encode1
>   encode2 (Cell x EOR) = quoteField x >> yield recordSeparator >> encode1
>   encode2 (Cell x EOT) = quoteField x >> yield recordSeparator >> encode1

>   encode3 (Cell x EOP) = quotePart x >> await >>= encode3
>   encode3 (Cell x EOC) = quotePart x >> yield fieldDelimiter >> yield fieldSeparator >> encode1
>   encode3 (Cell x EOR) = quotePart x >> yield fieldDelimiter >> yield recordSeparator >> encode1
>   encode3 (Cell x EOT) = quotePart x >> yield fieldDelimiter >> yield recordSeparator >> encode1

>   quoteField x
>     | requiresQuoting x = yield fieldDelimiter >> quotePart x >> yield fieldDelimiter
>     | otherwise = yield' x

>   requiresQuoting x = (ByteString.length x > n || ByteString.any isSpecial x)

> -- | @encodeLazyCSV n@ is an infinite pipe that converts a stream
> --   of cells into a fragmented lazy representation of of a CSV file,
> --   unconditionally quoting any field values with length greater than @n@.

> encodeLazyCSV :: Monad m => Int -> Pipe (Cell Lazy.ByteString) ByteString m ()
> encodeLazyCSV n = encode1
>  where

>   encode1 = await >>= encode2

>   encode2 (Cell x EOP) = yield fieldDelimiter >> quoteLazyPart x >> await >>= encode3
>   encode2 (Cell x EOC) = quoteLazyField x >> yield fieldSeparator >> encode1
>   encode2 (Cell x EOR) = quoteLazyField x >> yield recordSeparator >> encode1
>   encode2 (Cell x EOT) = quoteLazyField x >> yield recordSeparator >> encode1

>   encode3 (Cell x EOP) = quoteLazyPart x >> await >>= encode3
>   encode3 (Cell x EOC) = quoteLazyPart x >> yield fieldDelimiter >> yield fieldSeparator >> encode1
>   encode3 (Cell x EOR) = quoteLazyPart x >> yield fieldDelimiter >> yield recordSeparator >> encode1
>   encode3 (Cell x EOT) = quoteLazyPart x >> yield fieldDelimiter >> yield recordSeparator >> encode1

>   quoteLazyField x
>     | requiresQuoting n cs = yield fieldDelimiter >> quoteChunks cs >> yield fieldDelimiter
>     | otherwise = mapM_ yield' cs
>    where
>     cs = Lazy.toChunks x

>   quoteLazyPart = quoteChunks . Lazy.toChunks

>   quoteChunks = mapM_ quotePart

>   requiresQuoting n' (c:cs) = (m > n' || ByteString.any isSpecial c || requiresQuoting (n - m) cs)
>    where m = fromIntegral (ByteString.length c)
>   requiresQuoting _ [] = False

> quotePart x =
>   case ByteString.elemIndex QC x of
>     Just i -> yield' (ByteString.take i x) >> yield quoteSequence >> quotePart (ByteString.drop (i + 1) x)
>     Nothing -> yield' x

> yield' x = unless (ByteString.null x) (yield x)
