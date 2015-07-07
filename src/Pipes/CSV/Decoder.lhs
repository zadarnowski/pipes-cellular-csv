> -- | Module:    Pipes.CSV.Decoder
> -- Description: Streaming CSV decoder
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- Streaming CSV file decoder compliant with RFC 4180.
> -- It follows the RFC 4180 quite strictly, with the following
> -- minor extensions:
> --
> -- * arbitrary characters, including ASCII control codes and non-ASCII code points
> --   are accepted anywhere in the input,
> -- * CR and LF are accepted as row separators in addition to the standard CR+LF,
> -- * rows can have varying number of fields,
> -- * within quoted field, a quote character that is not followed by another quote,
> --   comma or line break is accepted literally.

> module Pipes.CSV.Decoder (
>   decodeCSV, decodeLazyCSV
> ) where

> import Data.ByteString (ByteString)
> import Data.Cell
> import Data.Int
> import Pipes
> import Pipes.CSV.Syntax
> import Pipes.ByteString.Chunks

> import qualified Data.ByteString as ByteString
> import qualified Data.ByteString.Lazy as Lazy

> -- | An infinite pipe that parses a fragmented lazy representation of
> --   of a CSV file into a stream of cells.

> decodeLazyCSV ::  Monad m => Pipe Lazy.ByteString (Cell ByteString) m ()
> decodeLazyCSV = toChunks >-> decodeCSV

> -- | An infinite pipe that parses a fragmented strict representation of
> --   of a CSV file into a stream of cells.

> decodeCSV :: Monad m => Pipe ByteString (Cell ByteString) m ()
> decodeCSV = await >>= decodeC
>  where

    At the beginning of a new cell:

>   decodeC s =
>     case ByteString.uncons s of
>       Just (c, xs) ->
>         case c of
>           QC -> decodeQ xs
>           CC -> yield (Cell ByteString.empty EOC) >> decodeC xs
>           CR -> yield (Cell ByteString.empty EOR) >> decodeCr xs
>           LF -> yield (Cell ByteString.empty EOR) >> decodeC xs
>           _  -> decodeU1 s 1 xs
>       Nothing -> await >>= decodeC

    After decode1 detects a CR:

>   decodeCr s =
>     case ByteString.uncons s of
>       Just (c, xs) ->
>         case c of
>           QC -> decodeQ xs
>           CC -> yield (Cell ByteString.empty EOC) >> decodeC xs
>           CR -> yield (Cell ByteString.empty EOR) >> decodeCr xs
>           LF -> decodeC xs
>           _  -> decodeU1 s 1 xs
>       Nothing -> await >>= decodeCr

    Decode a quoted cell:

>   decodeQ s = decodeQ1 s 0 s

    Decode a quoted cell, beginning the search for the next
    quote at a given string position:

>   decodeQ1 s i s' =
>     case ByteString.elemIndex QC s' of
>       Just i' -> let i'' = i + i' in i'' `seq` decodeQ2 s i'' (ByteString.drop (i' + 1) s')
>       Nothing
>         | ByteString.null s -> await >>= decodeQ
>         | otherwise -> await >>= decodeR s

    Decode a part of a quoted cell after a quote character has been located at @s!i@:

>   decodeQ2 s i s' =
>     case ByteString.uncons s' of
>       Just (c, xs) ->
>         case c of
>           QC -> decodeR (ByteString.take (i + 1) s) xs
>           CC -> yield (Cell (ByteString.take i s) EOC) >> decodeC xs
>           CR -> yield (Cell (ByteString.take i s) EOR) >> decodeCr xs
>           LF -> yield (Cell (ByteString.take i s) EOR) >> decodeC xs
>           _  -> let i' = i + 2 in i' `seq` decodeQ2 s i' xs -- accept the quote character literally, as an extension to RFC 4180
>       Nothing -> await >>= decodeQ3 s

    Same as @decodeQ2@, when the quote ends up at the end of the chunk @qs@:

>   decodeQ3 qs s =
>     case ByteString.uncons s of
>       Just (c, xs) ->
>         case c of
>           QC -> decodeR qs xs
>           CC -> yield (Cell (ByteString.init qs) EOC) >> decodeC xs
>           CR -> yield (Cell (ByteString.init qs) EOR) >> decodeCr xs
>           LF -> yield (Cell (ByteString.init qs) EOR) >> decodeC xs
>           _  -> decodeR1 qs s 1 xs -- accept the quote character literally, as an extension to RFC 4180
>       Nothing -> await >>= decodeQ3 qs

    Decode more quoted cell parts after a non-empty part @ps@ has been identified:

>   decodeR ps s = decodeR1 ps s 0 s

    Decode more quoted cell parts after a non-empty part @ps@ has been identified,
    beginning the search for the next quote at a given string position:

>   decodeR1 ps s i s' =
>     case ByteString.elemIndex QC s' of
>       Just i' -> let i'' = i + i' in i'' `seq` decodeR2 ps s i'' (ByteString.drop (i' + 1) s')
>       Nothing
>         | ByteString.null s -> await >>= decodeR ps
>         | otherwise -> yield (Cell ps EOP) >> await >>= decodeR s

    Decode a part of a quoted cell after a non-empty part @ps@ has been identifier
    and after a quote character has been located at @s!i@:

>   decodeR2 ps s i s' =
>     case ByteString.uncons s' of
>       Just (c, xs) ->
>         case c of
>           QC -> yield (Cell ps EOP) >> decodeR (ByteString.take (i + 1) s) xs
>           CC -> yield2 ps s i EOC >> decodeC xs
>           CR -> yield2 ps s i EOR >> decodeCr xs
>           LF -> yield2 ps s i EOR >> decodeC xs
>           _  -> decodeR2 ps s (i + 2) xs -- accept the quote character literally, as an extension to RFC 4180
>       Nothing -> await >>= decodeR3 ps s i

    Same as @decodeR2@, when the quote ends up at the end of the chunk @qs@:

>   decodeR3 ps qs i s =
>     case ByteString.uncons s of
>       Just (c, xs) ->
>         case c of
>           QC -> yield (Cell ps EOP) >> decodeR qs xs
>           CC -> yield2 ps s i EOC >> decodeC xs
>           CR -> yield2 ps s i EOR >> decodeCr xs
>           LF -> yield2 ps s i EOR >> decodeC xs
>           _  -> yield (Cell ps EOP) >> decodeR1 qs s 1 xs -- accept the quote character literally, as an extension to RFC 4180
>       Nothing -> await >>= decodeR3 ps qs i

    Decode an unquoted field:

>   decodeU1 s i s' =
>     case ByteString.uncons s' of
>       Just (c, xs) ->
>         case c of
>           CC -> yield (Cell (ByteString.take i s) EOC) >> decodeC xs
>           CR -> yield (Cell (ByteString.take i s) EOR) >> decodeCr xs
>           LF -> yield (Cell (ByteString.take i s) EOR) >> decodeC xs
>           _  -> let i' = i + 1 in i' `seq` decodeU1 s i' xs
>       Nothing -> await >>= decodeV s

    Decode an unquoted field after a non-empty part @ps@ has been identified:

>   decodeV ps s = decodeV1 ps s 0 s

    Decode more characters of an unquoted field,
    after a non-empty part @ps@ has been identified:

>   decodeV1 ps s i s' =
>     case ByteString.uncons s' of
>       Just (c, xs) ->
>         case c of
>           CC -> yield2 ps s i EOC >> decodeC xs
>           CR -> yield2 ps s i EOR >> decodeCr xs
>           LF -> yield2 ps s i EOR >> decodeC xs
>           _  -> let i' = i + 1::Int64 in i' `seq` decodeV1 ps s i' xs
>       Nothing
>         | (i > 0) -> yield (Cell ps EOC) >> await >>= decodeV s
>         | otherwise -> await >>= decodeV ps

    Yield a non-empty cell part and a possibly empty final cell part:

>   yield2 ps s i e
>     | (i > 0) = yield (Cell ps EOP) >> yield (Cell s e)
>     | otherwise = yield (Cell ps e)
