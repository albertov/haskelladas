{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import Prelude hiding (lines, words, init)
import Control.Monad (forM, when)
import Control.Exception (assert)
import System.IO (openFile, IOMode(..), Handle)
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Data.Text hiding (map, findIndex, length)
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

-- | Main del programa.
--   Registra los drivers de gdal y llama a extractFile con los argumentos
--   que se pasan por linea de comando (sys.argv[1:])
main = withAllDriversRegistered $ do
  files <- getArgs
  mapM_ extractFile files

-- | extractFile.
--   Recibe un nombre de fichero de aemet y extrao los campos a gtiffs comprimmidos
extractFile :: FilePath -> IO ()
extractFile input = do
    -- Abre el fichero de entrada en modo lectura. La sintaxis <- dentro de un bloque
    -- "do" extrae un valor puro de una accion de E/S (IO). En este caso, el tipo
    -- de la funcion openFile es FilePath -> IOMode -> IO (Handle). Es decir, que recibe
    -- una ruta de fichero, una "enumeracion" con el tipo de acceso al fichero y devuelve
    -- un objeto que cuando se ejecute en un bloque "do" realizará la acción de E/S y devolverá
    -- un Handle al fichero.
    h <- openFile input ReadMode
    -- Parsea un AemetFile desde el fichero. Tambien es una accion de entrada y salida
    -- (pues tiene que leer el fichero). Se extrae su valor con la sintaxis <-
    f <- mkFile h

    -- En un bloque let dentro de un bloque "do" se defined valores puros
    -- (es decir, que no hacen entrada/salida, que no son de tipo IO).
    -- Con el fichero parseado que se han obtenido anteriormente podemos
    -- definir los siguientes valores:
    let -- | Define (asignando a ctx) el contexto extraido del fichero parseado
        --   Lo del Right es para desempaquetar el valor del tipo Either
        --   que lo envuelve (el cual, en caso de error devuelve un Left error)
        Right ctx = getContext f
        -- | Asigna a opts las opciones que se le pasan a driver gtiff
        opts = [("compress","deflate"), ("zlevel", "9"), ("predictor", "3"),
                ("tiled", "yes")]
        -- | Numero de columnas del raster, es el ancho del shape del contexto
        nx = width (shape ctx)
        -- | Numero de filas del raster, es la altura del shape del contexto
        ny = height (shape ctx)
        -- | Resolucion del eje X
        dx = width (envelope ctx) / fromIntegral nx
        -- | Resolucion del eje Y
        dy = height (envelope ctx) / fromIntegral ny
        -- | Desempaqueta el minx y maxt del bbox del contexto
        Box x0 _ _ x1 = envelope ctx
        -- | Define la matriz de geotransformacion del raster
        gt = Geotransform x0 dx 0 x1 0 (-dy)
        -- | Define un "getter" para extraer la latitud de una fila
        lat = getValue f "LAT"
        -- | Define un "getter" para longitud la latitud de una fila
        lon = getValue f "LON"
        -- | genAr es una accion de E/S (ya que genera un array mutándolo in-place)
        --  que, dado un getter a la columna, genera un array (vector)
        genAr fun = do
           -- Pide memoria para el vector mutable
           v <- Stm.new (nx*ny)
           -- por cada indice de fila, llama a la función lambda con dicho índice
           forM_ (rows f) $ \r ->
              -- dentro de la funcion lambda.
              -- define un Point con las coordenadas que extrae del fichero
              let p = Point (lon r - dx/2) (lat r + dy/2)
                  -- llama a forward con el contexto y el punto para definir el
                  -- pixel correspondiente
                  Pixel i j = forward ctx p
                  -- dado el pixel, calcula el offset en el array lineal (orden C)
                  off = i * nx + j
              -- despues de asegurar que no indexamos fuera del array, escribimos en
              -- el array lo que devuelva el getter (fun)
              in assert (off>=0 && off<(nx*ny)) $
                 Stm.unsafeWrite v off (fun r)
           -- terminado de mutar el vector, lo "congelamos" para converirlo en un valor
           -- puro (inmutable) que puedan consumir funciones puras
           St.unsafeFreeze v

    -- Una vez definidas las cosas que necesitamos estamos de vuelta en IO (se ve por la indentacion)
    -- Ahora, por cada columna de la cabecera, cuando esta no es LAT o LON, ejecutara repetidamente
    -- el bloque do (E/S)
    forM_ (header f) $ \field ->
      when(field /= "LAT" && field /= "LON") $ do
        -- Define el nombre de fichero
        let fname = input ++ "." ++ (unpack field) ++ ".tif"
            -- y desmpaqueta una SpatialReference a partir de una cadena proj
            -- Esta, otra vez, esta envuelta en un Either para denotar que puede
            -- fallar (por no poder parsearla, por ejemplo).
            -- Asumimos que no va a fallar (en codigo de produccion es conveniente
            -- no desempaquetar cosas así ya que de no ser un Right lanzará excepción)
            Right srs' = fromProj4 (srs ctx)
        -- Imprime por pantalla el nombre de fichero
        putStrLn $ "Generando " ++ fname
        -- Crea el dataset de GDAL
        ds <- create "GTIFF" fname nx ny 1 opts :: IO (RWDataset Float)
        -- establece proyección (pasando la SpatialReference a String via "show")
        setDatasetProjection ds (show srs')
        -- establece la matriz de geotrans
        setDatasetGeotransform ds gt
        -- Llama a la funcion anónima con la banda. Esto es necesario ya que tiene que
        -- garantizarse que el dataset esté vivo mientras se este ejecutando la acción
        -- IO que usa la banda
        withBand ds 1 $ \b -> do
          -- dentro de la lambda, llama a genAr con un getter para el campo..
          array <- genAr (getValue f field)
          -- y escribe el array en la banda
          writeBand b 0 0 nx ny nx ny 0 0 array

-- | Tipo de datos para representar un fichero de Aemet parseado
data AemetFile = AemetFile {
    -- | Tiene una cabecera que es una lista de cadenas (una por cada columna)
    header :: [Text]
    -- | El numero de filas
  , nRows  :: !Int
    -- | El numero de columnas
  , nCols  :: !Int
    -- | Un array de Doubles con los datos
  , values :: St.Vector Double
}

-- | Rows devuelve una lista de indices de filas validas para un AemetFile
rows :: AemetFile -> [Int]
rows f = [0..(nRows f)-1]

-- | mkFile es una acción de IO que parsea un AemetFile a partir de un Handle a fichero
mkFile :: Handle -> IO (AemetFile)
mkFile h = do
     -- Parte en lineas (lines), dentro de una accion de E/S (hGetContents h),
     -- lo que se lea del Handle. El liftM es para "elevar" la funcion pura "lines"
     -- a dentro de la accion IO. es equivalente a:
     --  do contenido <- hGetContents h
     --     let (h:rs) = lines contenido
     -- El resultado se desempaqueta, la head (primera linea) en "h" y el resto en
     -- "rs"
    (h:rs) <- liftM lines $ hGetContents h
    -- values es un vector de doubles con todos los valores. Se define como el
    -- resultado de aplicar sobre todas las lineas (map) de valores (rs) la composicion
    -- de tres funciones que:
    --  1) Convierte de Text -> ByteString (adaptacion de impedancia) con encodeUtf8
    --  2) parsea el ByteString de la fila a una lista de [Doubles] con los valores
    --     de la fila (mediante parseRow)
    --  3) Convierte la lista a un Vector
    -- El resultado es una lista de vectores, uno por cada fila, este se le pasa
    -- a St.concat para que genere un vector grande con todos los vectores concatenados
    let values = St.concat $ map (St.fromList . parseRow . encodeUtf8) rs
        -- La cabecera se define como la fila de cabecera partida en palabras (s.split())
        header = words h
        -- parseRow rompe el ByteString en una cadena de Doubles llamando a unsafeReadDouble
        -- (unsafe porque es de bajo nivel y asume que las cadenas (que son de C) terminan en
        -- nulo)
        parseRow r
          = case unsafeReadDouble r of
             Just (d,r') -> d : parseRow r'
             Nothing     -> []
    -- Con todo lo definido, ya podemos construir el AemetFile y "envolverlo" en IO para sacarlo de
    -- la accion
    return $ AemetFile { header = header
                       , nCols  = length header
                       , nRows  = length rs
                       , values = values}

-- | getContext intenta extraer un Context con la georeferenciacion desde un
--   AemetFile.
getContext :: AemetFile -> EitherError Context
getContext f
  = mkContext (mkEnvelope (x0-dx/2) (y0-dy/2) (x1+dx/2) (y1+dy/2)) 
              (mkShape nx ny) pj
  where
    -- x0 el el valor minimo del conjunto de xs
    x0 = S.findMin xs
    -- x1 el el valor maximo del conjunto de xs
    x1 = S.findMax xs
    -- Simétrico para y...
    y0 = S.findMin ys
    y1 = S.findMax ys
    -- La resolucion...
    dx = (x1-x0) / fromIntegral nx
    dy = (y1-y0) / fromIntegral ny
    nx = S.size xs
    ny = S.size ys
    xl = getValues f "LON"
    yl = getValues f "LAT"
    -- xs es un conjunto (Set) a partir de la lista xl
    xs = S.fromList xl
    -- lo mismo para las ys
    ys = S.fromList yl
    -- la proyeccion a piñon porque la sabemos y no cambia
    pj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

-- | getValue extrae un valor de una columna (pasada por nombre)
--   y una fila (pasada por indice entero) de un AemetFile
--   El orden de los parametros se escoge así para que sea facil
--   aplicarla parcialmente (para construir getters)
---  Con un poco de practica sale muy natural lo del orden de los params...
getValue :: AemetFile -> Text -> Int -> Double
getValue f name row
   = (values f) St.! (row*(nCols f) + col)
   where col = fromJust (findIndex (==name) (header f))

-- | getValues devuelve una lista con todos los valores de una columna
getValues :: AemetFile -> Text -> [Double]
getValues f name
  -- la definicion es immediata: aplicar getValue sobre todos los indices
  = map (getValue f name) (rows f)
