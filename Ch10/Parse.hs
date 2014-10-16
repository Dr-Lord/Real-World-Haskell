-- File: Real World Haskell\Ch10\Parse.hs
import PNM
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Word (Word8)
import Control.Applicative ((<$>)) -- infix version of fmap
import Data.Char


data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

newtype Parse a = Parse {
        runParse :: ParseState -> Either String (a, ParseState)
    }
    
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result
        
        
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset }
    
    
parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err


(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState

                
-- class Functor f where
    -- fmap :: (a -> b) -> f a -> f b

    -- Functor Laws
-- fmap id       ==  id
-- fmap (f . g)  ==  fmap f . fmap g

instance Functor Parse where
    fmap f parser = parser ==> \result -> identity (f result)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
                if mp == Just True
                 then parseByte ==> \b ->
                    (b:) <$> parseWhile p
                 else identity []

parseWhileVerbose p =
    peekByte ==> \mc ->
    case mc of
      Nothing -> identity []
      Just c | p c ->
                 parseByte ==> \b ->
                 parseWhileVerbose p ==> \bs ->
                 identity (b:bs)
             | otherwise ->
                 identity []

{- Code saving version in Exercise 1
parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")
-}

  
parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
            then bail "no more input"
            else let n = read digits
                in if n < 0
                    then bail "integer overflow"
                    else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h


-- 1
data ASCIIGreymap = ASCIIGreymap {
    aGreyWidth  :: Int
  , aGreyHeight :: Int
  , aGreyMax    :: Int
  , aGreyData   :: String
  } deriving (Eq)

instance Show ASCIIGreymap where
  show ( ASCIIGreymap w h m _ ) = "ASCIIGreymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

parsePGMHeader :: String -> Parse (Int, Int, Int)
parsePGMHeader hspec =
    parseWhileWith w2c (not . isSpace) ==> \header -> skipSpaces ==>&
    assert (header == hspec) ("invalid " ++ hspec ++ " header") ==>&
    skipComments ==>&       skipSpaces ==>&
    parseNat ==> \width ->  skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey -> 
    parseByte ==>&
    identity (width,height,maxGrey)

parseASCIIPGM :: Parse ASCIIGreymap
parseASCIIPGM =
    parsePGMHeader "P2" ==> \(width, height, maxGrey) ->
    parseBytes (width * height) ==> \bitmap ->
    identity (ASCIIGreymap width height maxGrey (map w2c $ L.unpack bitmap))

parseRawPGM :: Parse Greymap
parseRawPGM = 
    parsePGMHeader "P5" ==> \(width, height, maxGrey) ->
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)


skipComments :: Parse ()
skipComments =
    parseWhileWith w2c (`notElem` "\r\n") ==> \comment ->
     if isComment comment
      then skipSpaces ==>& identity ()
      else getState ==> \st ->
        let st' = st {
                offset = offset st - fromIntegral (length comment), 
                string = (L8.pack comment) `L8.append` string st
            }
        in putState st' ==>& identity () 

isComment :: String -> Bool
isComment ('#':rest) = True
isComment otherwise  = False


-- 2
parseRaster :: Int -> Parse L.ByteString 
parseRaster n
    | n == 0 = identity L8.empty
    | n >  0 = parseWhileWith w2c (isDigit) ==> \digits ->
        skipSpaces ==>& parseRaster (n-1) ==> \result -> 
        identity $ (chr . read) digits `L8.cons` result

parseMultiByteRawPGM :: Parse Greymap
parseMultiByteRawPGM = 
    parsePGMHeader "P5" ==> \(width, height, maxGrey) ->
    parseRaster (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)


-- 3
data AnyGraymap = AnyGraymap {
      aWidth  :: Int
    , aHeight :: Int
    , aMax    :: Int
    , aData   :: Either String L.ByteString
    } deriving (Eq)

instance Show AnyGraymap where
  show ( AnyGraymap w h m _ ) = "AnyGraymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

parseAnyPGMHeader :: Parse AnyGraymap
parseAnyPGMHeader =
    parseWhileWith w2c (not . isSpace) ==> \header -> skipSpaces ==>&
    skipComments ==>&       skipSpaces ==>&
    parseNat ==> \width ->  skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey -> 
    parseByte ==>&
    parseRaster (width * height) ==> \bitmap -> case header of
        "P2" -> identity (AnyGraymap width height maxGrey (Left (map w2c $ L.unpack bitmap)))
        "P5" -> identity (AnyGraymap width height maxGrey (Right bitmap))
        _    -> bail "Unrecognised type"


-- Run on either test file, expect: Right AnyGraymap 10x10 255
testAnyPGM name = do
    str <- L8.readFile name
    print (parse parseAnyPGMHeader str)
