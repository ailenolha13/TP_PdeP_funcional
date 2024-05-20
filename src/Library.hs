module Library where
import PdePreludat

-- Creamos el tipo ValorCiudad 
type ValorCiudad = Number
-- Creamos el tipo Anio 
type Anio = Number
-- Creamos el tipo Atraccion
type Atraccion = String

-- Modelamos ciudades
-- Nos interesa conocer su nombre, el año de fundación, las atracciones principales y su costo de vida
data Ciudad = UnaCiudad {
    nombre :: String
    , anioFundacion :: Number
    , atraccionesPrincipales :: [String]
    , costoVida :: Number
} deriving (Show)

-- Funcion para obtener el valor de una ciudad en base a 3 condiciones mencionadas en el enunciado del TP
obtieneValor :: Ciudad -> ValorCiudad
obtieneValor ciudad | fueFundadaAntesDe 1800 ciudad = es5VecesLaDiferencia 1800 (anioFundacion ciudad)
                    | noTieneAtracciones ciudad = doble (costoVida ciudad)
                    | otherwise = triple (costoVida ciudad)

-- Funcion para saber si la ciudad fue fundanda antes del anio 1800
fueFundadaAntesDe :: Anio -> Ciudad -> Bool
fueFundadaAntesDe anio ciudad = anioFundacion ciudad < anio

-- Funcion para calcular 5 veces la diferencia entre 1800 y el anio de fundacion
es5VecesLaDiferencia :: Anio -> Anio -> Number
es5VecesLaDiferencia anio anioFundacion = quintuple (resta anio anioFundacion)

-- Funcion para saber si la ciudad tiene o no atracciones
noTieneAtracciones :: Ciudad -> Bool
noTieneAtracciones = null . atraccionesPrincipales

-- Funcion para saber si la ciudad tiene una atraccion copada
tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada = any esVocal . map head . atraccionesPrincipales

-- Funcion para saber si un caracter es vocal o no
esVocal :: Char -> Bool
esVocal caracter = caracter `elem` "aeiouAEIOU"

------------ TO-DO Punto 2 ------------
-- CIUDAD SOBRIA:

-- CIUDAD CON NOMBRE RARO:

---------------------------------------

-- Como no hay efecto, cuando queremos agregar una nueva atraccion a una ciudad recibimos la ciudad y devolvemos una ciudad nueva
agregarNuevaAtraccion :: Atraccion -> Ciudad -> Ciudad
agregarNuevaAtraccion nueva ciudad = UnaCiudad {
    nombre = nombre ciudad
    , anioFundacion = anioFundacion ciudad
    , atraccionesPrincipales = nueva : atraccionesPrincipales ciudad
    , costoVida = costoVida ciudad * 1.2
}

------------ TO-DO Punto 3 ------------
atraviesaCrisis :: Ciudad -> Ciudad
atraviesaCrisis ciudad = UnaCiudad {
    nombre = nombre ciudad
    , anioFundacion = anioFundacion ciudad
    , atraccionesPrincipales = init (atraccionesPrincipales ciudad)
    , costoVida = costoVida ciudad * 0.9 
}

-- REMODELACION:

-- REEVALUACION:

------------ IN PROGRESS Punto 4 ------------
{-- 

Para que una ciudad tenga una nueva atraccion, en la consola GHCI debemos usar la funcion agregarnuevaAtraccion.
La funcion agregarnuevaAtraccion recibe como primer parametro la atraccion a agregar,
y como segundo parametro recibe la ciudad a la que le vamos a agregar la atraccion.

Para agregar la atraccion "Balneario Municipal Alte. Guillermo Brown" a la ciudad azul escribimos en la consola:
agregarNuevaAtraccion "Balneario Municipal Alte. Guillermo Brown" azul

La consola nos devuelve la ciudad azul con la atraccion agregada:
UnaCiudad
    { nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales =
        [ "Balneario Municipal Alte. Guillermo Brown"
        , "Teatro Español"
        , "Parque Municipal Sarmiento"
        , "Costanera Cacique Catriel"
        ]
    , costoVida = 228
    }

--}

-- Funciones utiles
doble :: Number -> Number
doble numero = numero + numero
triple :: Number -> Number
triple numero = 3 * numero
quintuple :: Number -> Number
quintuple numero = 5 * numero
resta :: Number -> Number -> Number
resta numero1 numero2 = numero1 - numero2

-- Instancia Eq para el tipo Ciudad para en los test poder comparar 2 valores de tipo Ciudad, sino el ShouldBe tira error
instance Eq Ciudad where
  (UnaCiudad n1 af1 ap1 cv1) == (UnaCiudad n2 af2 ap2 cv2) =
    n1 == n2 && af1 == af2 && ap1 == ap2 && cv1 == cv2
-- Ciudades para los test
baradero :: Ciudad
baradero = UnaCiudad {
        nombre = "Baradero"
        , anioFundacion = 1615
        , atraccionesPrincipales = ["Parque del Este", "Museo Alejandro Barbich"]
        , costoVida = 150 }
nullish :: Ciudad
nullish = UnaCiudad {
        nombre = "Nullish"
        , anioFundacion = 1800
        , atraccionesPrincipales = []
        , costoVida = 140 }
caletaOlivia :: Ciudad
caletaOlivia = UnaCiudad {
        nombre = "Caleta Olivia"
        , anioFundacion = 1901
        , atraccionesPrincipales = ["El Gorosito", "Faro Costanera"]
        , costoVida = 120 }
azul :: Ciudad
azul = UnaCiudad {
    nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"]
    , costoVida = 190 }
azulNueva :: Ciudad
azulNueva = UnaCiudad {
    nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Balneario Municipal Alte. Guillermo Brown", "Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"]
    , costoVida = 228 }
azulCrisis :: Ciudad
azulCrisis = UnaCiudad {
    nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Teatro Español", "Parque Municipal Sarmiento"]
    , costoVida = 171 }