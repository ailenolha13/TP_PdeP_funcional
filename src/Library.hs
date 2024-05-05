module Library where
import PdePreludat

-- Creamos el tipo ValorCiudad 
type ValorCiudad = Number
-- Creamos el tipo Anio 
type Anio = Number

-- Modelamos ciudades
-- Nos interesa conocer su nombre, el año de fundación, las atracciones principales y su costo de vida
data Ciudad = UnaCiudad {
    nombre :: String,
    anioFundacion :: Number,
    atraccionesPrincipales :: [String],
    costoVida :: Number
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

-- Funciones utiles
doble :: Number -> Number
doble numero = numero + numero
triple :: Number -> Number
triple numero = 3 * numero
quintuple :: Number -> Number
quintuple numero = 5 * numero
resta :: Number -> Number -> Number
resta numero1 numero2 = numero1 - numero2

-- Ciudades creadas solo con el proposito de poder probar las funciones rapidamente - DESPUES HAY QUE BORRARLAS:
buenosAires:: Ciudad
buenosAires = UnaCiudad {nombre = "Buenos Aires", anioFundacion = 1536, atraccionesPrincipales = ["Obelisco"], costoVida= 50}
salta:: Ciudad
salta = UnaCiudad {nombre = "Salta", anioFundacion = 1530, atraccionesPrincipales = ["Tren de las nubes", "Cafayate", "Obispo", "Empanadas"], costoVida= 30}
mendoza :: Ciudad
mendoza = UnaCiudad {nombre = "Mendoza", anioFundacion = 1850, atraccionesPrincipales = [], costoVida= 20}
sanLuis :: Ciudad
sanLuis = UnaCiudad {nombre = "San Luis", anioFundacion = 1950, atraccionesPrincipales = ["Rapel", "Trekking", "Tirolesa"], costoVida= 100}
