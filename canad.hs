{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import Prelude hiding (lines, words, init)
import Control.Monad (forM, when)
import Control.Exception (assert)
import System.IO (openFile, IOMode(..), Handle)
import Data.Maybe (fromJust)
import Data.List (foldl', findIndex)
import Data.Text hiding (foldl', map, findIndex, length)
import Data.ByteString.Lex.Double (unsafeReadDouble)
import Data.Text.IO hiding (putStrLn)
import Data.Text.Encoding (encodeUtf8)
import System.Environment
import Control.Monad
import OSGeo.GDAL
import OSGeo.OSR
import qualified Data.Set as S
import MyGIS.Data.Error
import MyGIS.Data.Context

import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

main =  setQuietErrorHandler >> registerAllDrivers
     >> getArgs >>= mapM_ extractFile

extractFile input = do
    h <- openFile input ReadMode
    f <- mkFile h
    let Right ctx = getContext f
        opts = [("compress","deflate"), ("zlevel", "9"), ("predictor", "3"),
                ("tiled", "yes")]
        nx = width (shape ctx)
        ny = height (shape ctx)
        dx = width (envelope ctx) / fromIntegral nx
        dy = height (envelope ctx) / fromIntegral ny
        Box x0 _ _ x1 = envelope ctx
        gt = Geotransform x0 dx 0 x1 0 (-dy)
        lat = getValue f "LAT"
        lon = getValue f "LON"
        genAr fun = do
           v <- Stm.new (nx*ny)
           forM_ (rows f) $ \r ->
              let p = Point (lon r - dx/2) (lat r + dy/2)
                  Pixel i j = forward ctx p
                  off = i * nx + j
              in assert (off>=0 && off<(nx*ny)) $
                 Stm.unsafeWrite v off (fun r)
           St.unsafeFreeze v
    forM_ (header f) $ \field ->
      when(field /= "LAT" && field /= "LON") $ do
        let fname = input ++ "." ++ (unpack field) ++ ".tif"
            Right srs' = fromProj4 (srs ctx)
        putStrLn $ "Generando " ++ fname
        ds <- create "GTIFF" fname nx ny 1 opts :: IO (RWDataset Float)
        setDatasetProjection ds (show srs')
        setDatasetGeotransform ds gt
        withBand ds 1 $ \b ->
          genAr (getValue f field) >>= writeBand b 0 0 nx ny nx ny 0 0

data AemetFile = AemetFile {
    header :: [Text]
  , nRows  :: !Int
  , nCols  :: !Int
  , values :: St.Vector Double
}

rows f = [0..(nRows f)-1]

mkFile :: Handle -> IO (AemetFile)
mkFile h = do
    (h:rs) <- liftM lines $ hGetContents h
    let values = St.concat $ map (St.fromList . parseRow . encodeUtf8) rs
        header = words h
        parseRow r
          = case unsafeReadDouble r of
             Just (d,r') -> d : parseRow r'
             Nothing     -> []
    return $ AemetFile { header = header
                       , nCols  = length header
                       , nRows  = length rs
                       , values = values}

getContext :: AemetFile -> EitherError Context
getContext f
  = mkContext (mkEnvelope (x0-dx/2) (y0-dy/2) (x1+dx/2) (y1+dy/2)) 
              (mkShape nx ny) pj
  where
    x0 = S.findMin xs
    x1 = S.findMax xs
    y0 = S.findMin ys
    y1 = S.findMax ys
    dx = (x1-x0) / fromIntegral nx
    dy = (y1-y0) / fromIntegral ny
    nx = S.size xs
    ny = S.size ys
    xl = getValues f "LON"
    yl = getValues f "LAT"
    xs = foldl' (flip S.insert) S.empty xl
    ys = foldl' (flip S.insert) S.empty yl
    pj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

getValue :: AemetFile -> Text -> Int -> Double
getValue f name row
   = (values f) St.! (row*(nCols f) + col)
   where col = fromJust (findIndex (==name) (header f))

getValues :: AemetFile -> Text -> [Double]
getValues f name
  = map (getValue f name) (rows f)
