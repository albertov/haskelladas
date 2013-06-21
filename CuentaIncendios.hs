import System.Environment
import System.IO
import Text.CSV (parseCSVFromFile, CSV)
import Data.List (findIndex)
import Data.Binary (encode)
import Data.ByteString.Lazy (hPut)
import qualified Data.Vector as V

-- | main es el punto de entrada al programa. Como indica su tipo,
--   (el "main :: IO ()"), es una funcion que no recibe parametros
--   y devuelve un tipo IO (). Es decir, puede hacer entrada/salida
--   con el resto del mundo o llamar a funciones que la hagan.
main :: IO ()
main = do
    -- Obtiene los argumentos con los que se ha llamado al programa (ie: sys.argv)
    -- y los desempaqueta. Ojo, no es robusto: el programa cascara si no se pasan
    -- extactamente 3 argumentos pero para usar y tirar vale
    [csv_fname, box_, shape_] <- getArgs
    -- parsea el fichero csv y desempaqueta el valor correcto en la variable "csv"
    -- Lo mismo, no es robusto, si el parseo falla devolvera "Left <error>" lo cual
    -- no estamos gestionando (por cierto, el compilador si le pides te avisa de estas
    -- cosas!). Solo gestionamos "Right <valor_correcto>"
    Right csv <- parseCSVFromFile csv_fname
    -- Ya hemos traido (mediante <-) del mundo real (IO) los datos que necesitamos.
    -- Ahora trabajamos con ellos. Dentro de la notacion "do" podemos usar "let"
    -- para asignar el resultado de expresiones puras a variables. Todas las nombres
    -- definidos en un let se "ven" los unos a los otros y no importa en orden en que se
    -- definan (se evaluara en el orden que al compilador le de la gana)

    -- Traduzo a castellano:

    -- points son los puntos cargados del csv
    let points  = loadPoints csv  
        -- box es la caja que nos pasaron por argv parseada
        -- (no robusto, si pasan basura el programa cascara con excepcion ya que no la atrapamos)
        box     = let (x0,y0,x1,y1) = read ("(" ++ box_ ++ ")") in Box x0 y0 x1 y1
        -- (nx,ny) es la dimension del raster. leida de argv no-robustamente
        (nx,ny) = read ("(" ++ shape_ ++ ")")
        -- raster es lo que nos ineteresa realmente.. lo declaramos de tipo "vector de enteros"
        -- para asegurarnos de que el compilador no infiera otro tipo ya que lo vamos a serializar
        -- en un fichero binario
        raster :: V.Vector Int
        -- Para generar el raster llamamos a la funcion "generate" de Vector. Esta recibe el numero
        -- de elementos y una funcion de tipo "Int -> a" que recibe un entero con el indice del pixel
        -- y devuelve el valor para ese punto.
        raster = V.generate (nx*ny) gFun
        -- gFun es la funcion que calcula el valor de cada punto. La notacion es un poco rara fuera
        -- de haskell pero muy comun. Se llama point-free style y viene a significar que el argumento
        -- se omite ya que entra igual por el otro lado. por ejemplo "f a = g a" (f es una funcion que
        -- recibe "a" y devuleve lo que devuelve g de a) se puede escribir como "f = g" y sigue siendo
        -- cierto. En este caso estamos diciendo que gFun es a composicion de 4 funciones, las enumero
        -- y explico en el orden que se van aplicando (al reves de como se escribe, como en mates)
        --    - pixelFromIndex  : traduce el indice que nos pasa generate a un (Pixel x y)
        --    - (pixel2point gt): es la funcion que hace la trasformacion afin de pixel a punto
        --                        parcialmente aplicada con la matriz de trans. Suena a chino
        --                        per conceptualmenete es sencillo: En haskell se puede llamar
        --                        a una funcion de 2 argumentos con solo uno y devolvera una funcion
        --                        que acepta un argumento (el que le falta) y devuelve lo que tenia
        --                        que devolver. Esto es MUY potente.
        --   - pointsClose      : Devuelve un vector de puntos que esten a menos de 1000 metros del
        --                        punto que recibe como parametro
        --   - V.length         : delvuelve la longitud de un vector
        -- en castellano: gFun es la longitud del vector de puntos cercanos al punto que corresponde al
        --                pixel que corresponde al indice
        gFun = V.length . pointsClose . (pixel2point gt) . pixelFromIndex
        --  gt es la matriz de cambio de sistema de coordenadas. construida del bbox y el tamanio del raster
        gt = mkGeoTransform box (Shape nx ny)
        -- pixelFromIndex es un a funcion que traduce el indice en el array al pixel 2d que corresponde.
        -- es otra composicion: primero saca el (divisor,cociente) de dividir entre nx (es decir, la (fila,columna))
        -- y luego construye el pixel. uncurry es para que una funcion de dos args (el constructor de un pixel)
        -- la podamos llamar con un solo argumento que sea una 2-tupla de sus dos args.
        pixelFromIndex = uncurry Pixel . (`divMod` nx)
        -- Otra funcion parcialemente aplicada: recibe un punto y devuelve los puntos de 'points' que esten a menos
        -- de 1000 metros
        pointsClose = pointsCloserThan points 1000
    -- Ahora fuera del 'let' volvemos a IO a hacer entrada y salida.. escribimos un par de cosas por pantall...
    print (box, (nx, ny))
    putStrLn ("Procesando " ++ (show . V.length $ points) ++ " puntos...")
    -- abrimos el fichero de salida
    f <- openFile "salida.bin" WriteMode
    -- y ahora: por cada punto del raster (V.forM_) aplica la funcion de E/S: "escribe en el fichero el punto serializado"
    V.forM_ raster $ \v -> hPut f (encode v)
    -- cierra el fichero
    hClose f
    -- y di que todo OK
    putStrLn "Ok"


--
-- Ahora vienen las funciones que llamamos desde main pra hacer trabajo
            
-- | pointsCloserThan devuelve los puntos de un vector `ps` que este a menos
--   de `dist` metros del punto `p`
pointsCloserThan :: V.Vector Point -> Double -> Point -> V.Vector Point
pointsCloserThan ps dist p = V.filter (\p' -> distance p p' < dist) ps

-- distance calcula los valores mas rentables en los que invertir en balsa
distance :: Point -> Point -> Double
distance (Point x0 y0) (Point x1 y1) = sqrt ((x1-x0)**2 + (y1-y0)**2)

-- Tipos de datos para punto y pixel. Les ponemos nombres y apellidos
-- en vez de usar tuplas anonimas de dos elementos ya que es gratis
-- (una linea en vez de definir una clase de varias lineas en c++/java/python)
-- y expresamos mejor la semantica del programa de tal manera que el compilador
-- nos ayude. por ejemplo, no nos dejara pasar sin querer a una func. que espere
-- un pixel y viceversa ayudando a prevenir errores
data Point = Point Double Double deriving (Eq, Show)
data Pixel = Pixel Int Int deriving (Eq, Show)

-- | loadPoints carga los puntos del CSV. Se define en varias partes
--   (como en mates)
loadPoints :: CSV -> V.Vector Point
-- intentar cargar puntos de un CSV vacio es un vector vacio
loadPoints [] = V.empty
-- intentar cargar puntos de un CSV con solo la cabecera pero sin
-- datos es igualmente interesante
loadPoints [_] = V.empty
-- esto es interesante: cargar puntos de una cabecera y una lista de puntos
-- que le siguen. Se define como "el vector resultante (V.fromList) de aplicar
-- extraePoint (map extraPoint) sobre las lineas que son validas (filter validLine)
-- del resto (lo que no es cabecera)"
loadPoints (cabecera:resto) =
    V.fromList . map extraePoint . filter validLine $ resto
    where extraePoint r   = Point (getX r) (getY r)
          getX r          = read (r!!ixX)
          getY r          = read (r!!ixY)
          validLine l     = ixX < length l &&  ixY < length l
          -- Aha! parece entrada y salida pero no lo es! Resulta que
          -- la notacion "do" no solo es valida para IO sino para cualquier
          -- monada (es mates y lleva acento en la o). Resulta que IO
          -- es una monada (acento en la ooo) pero no la unica. En general,
          -- cualquier computacion que implique pasar estado de un paso a otro
          -- (lo que implica secuencialidad) se puede modelar como una monada
          -- y usarse la notacion "do". En este caso la monada es "Maybe" que viene
          -- a ser un tipo que pude ser bien "Nothing" (ausencia de valor) o "Just v"
          -- (valor v). La secuencia que se implica es que:
          --  el primer findIndex devuelve el indice de "X" en la cabecera o Nothing si no hay
          --  "X" en la cabecera. Si es Nothing, sal del "do" y devuelve Nothing, si no sigue..."
          -- el segundo findIndex devuelve el indice de "Y". Si es Nothing sal y devuleve Nothing,
          -- sino sal y devuelve Just (x,y), que es lo que se desempaqueta y asigna a (ixX,ixY)
          Just (ixX, ixY) = do
                x <- findIndex (=="X") cabecera
                y <- findIndex (=="Y") cabecera
                return (x, y)

-- Tipo para la matriz de transformacion
data GeoTransform = GeoTransform Double Double Double Double Double Double
    deriving (Eq, Show)

-- Algebra lineal para proyectar un pixel a un punto
pixel2point :: GeoTransform -> Pixel -> Point
pixel2point (GeoTransform gt0 gt1 gt2 gt3 gt4 gt5) (Pixel ln col) = Point px py
    where px   = gt0 + col'*gt1 + ln'*gt2
          py   = gt3 + col'*gt4 + ln'*gt5
          col' = fromIntegral col
          ln'  = fromIntegral ln
    
-- Mas algebra para proyectar un punto a un pixel
point2pixel :: GeoTransform -> Point -> Pixel
point2pixel (GeoTransform gt0 gt1 gt2 gt3 gt4 gt5) (Point x y) =
    Pixel (floor ln) (floor col)
    where ln     = detA1B  / detA
          col    = detA2B  / detA
          detA   = gt1*gt5 - gt2*gt4
          x0     = x - gt0
          y0     = y - gt3
          detA1B = gt1*y0   + gt4*x0
          detA2B = gt2*y0   + gt5*x0

-- una bbox de toda la vida
data Box = Box
    Double -- x0
    Double -- y0
    Double -- x1
    Double -- y1
  deriving (Eq,Show)

-- definimos la altura y ancho de una caja
width, height :: Box -> Double
width  (Box x0  _ x1  _) = x1 - x0
height (Box  _ y0  _ y1) = y1 - y0

-- mas tipos! que son gratis y ayudan mucho
data Shape = Shape Int Int deriving (Eq, Show)
data PixelShape = PixelShape Double Double deriving (Eq, Show)

-- para construir una matriz de transformacion a partir de
-- un bbox y forma del raster
mkGeoTransform :: Box -> Shape -> GeoTransform
mkGeoTransform b@(Box x0 _ _ y1) (Shape nx ny) =
    GeoTransform x0 dx 0 y1 0 (-dy)
    where dx = width b / fromIntegral nx
          dy = height b / fromIntegral ny
