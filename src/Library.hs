module Library where
import PdePreludat

------------ PUNTO 1 PARTE 1 ------------

type ValorCiudad = Number

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

-- Funcion para saber si la ciudad fue fundanda antes de un año configurable
fueFundadaAntesDe :: Number -> Ciudad -> Bool
fueFundadaAntesDe anio ciudad = anioFundacion ciudad < anio

-- Funcion para calcular 5 veces la diferencia entre un año configurable y el anio de fundacion
es5VecesLaDiferencia :: Number -> Number -> Number
es5VecesLaDiferencia anio anioFundacion = quintuple (resta anio anioFundacion)

-- Funcion para saber si la ciudad tiene o no atracciones
noTieneAtracciones :: Ciudad -> Bool
noTieneAtracciones = null . atraccionesPrincipales

-- Funciones utiles
doble :: Number -> Number
doble numero = numero + numero
triple :: Number -> Number
triple numero = 3 * numero
quintuple :: Number -> Number
quintuple numero = 5 * numero
resta :: Number -> Number -> Number
resta numero1 numero2 = numero1 - numero2

------------ PUNTO 2 PARTE 1 ------------

-- Funcion para saber si la ciudad tiene una atraccion copada
tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada = any esVocal . map head . atraccionesPrincipales

-- Funcion para saber si un caracter es vocal o no
esVocal :: Char -> Bool
esVocal caracter = caracter `elem` "aeiouAEIOU"

-- CIUDAD SOBRIA:
esSobria :: Number -> Ciudad -> Bool
esSobria xLetras ciudad | noTieneAtracciones ciudad = False
                        | otherwise = all ((> xLetras) . length) (atraccionesPrincipales ciudad)

-- CIUDAD CON NOMBRE RARO:
tieneNombreRaro :: Ciudad -> Bool
tieneNombreRaro ciudad = length (nombre ciudad) < 5

------------ PUNTO 3 PARTE 1 ------------

type Atraccion = String
type Incremento = Number

-- Como no hay efecto de lado, cuando queremos agregar una nueva atraccion a una ciudad recibimos la ciudad y devolvemos una ciudad nueva
agregaNuevaAtraccion :: Atraccion -> Ciudad -> Ciudad
agregaNuevaAtraccion nueva ciudad = UnaCiudad {
    nombre = nombre ciudad
    , anioFundacion = anioFundacion ciudad
    , atraccionesPrincipales = nueva : atraccionesPrincipales ciudad
    , costoVida = costoVida ciudad * 1.2
}

-- Como no hay efecto de lado, cuando una ciudad atraviesa una crisis recibimos la ciudad y devolvemos una ciudad nueva
atraviesaCrisis :: Ciudad -> Ciudad
atraviesaCrisis ciudad = UnaCiudad {
    nombre = nombre ciudad
    , anioFundacion = anioFundacion ciudad
    , atraccionesPrincipales = init (atraccionesPrincipales ciudad)
    , costoVida = costoVida ciudad * 0.9 
}

-- Como no hay efecto de lado, cuando queremos remodelar una ciudad recibimos la ciudad y devolvemos una ciudad nueva remodelada
remodelaCiudad :: Incremento -> Ciudad -> Ciudad
remodelaCiudad incremento ciudad = UnaCiudad {
    nombre = "New " ++ nombre ciudad
    , anioFundacion = anioFundacion ciudad
    , atraccionesPrincipales = atraccionesPrincipales ciudad
    , costoVida = costoVida ciudad * (1 + incremento / 100)
}

-- REEVALUACION:
reevaluarCiudad :: Number -> Ciudad -> Ciudad
reevaluarCiudad n ciudad | esSobria n ciudad = ciudad { costoVida = costoVida ciudad * 1.1}
                         | otherwise = ciudad { costoVida = costoVida ciudad - 3}

------------ PUNTO 4 PARTE 1 ------------
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

{-- 

Para que una ciudad tenga una remodelacion, en la consola GHCI debemos usar la funcion remodelaCiudad.
La funcion remodelaCiudad recibe como primer parametro el incremento de su costo de vida configurable (x%),
y como segundo parametro la ciudad a remodelar.

Para remodelar la ciudad azul escribimos en la consola:
remodelaCiudad 50 azul

La consola nos devuelve la ciudad azul con la atraccion agregada:
UnaCiudad
    { nombre = "New Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales =
        [ "Teatro Espaniol"
        , "Parque Municipal Sarmiento"
        , "Costanera Cacique Catriel"
        ]
    , costoVida = 285
    }

--}

{-- 

Para que una ciudad tenga una crisis, en la consola GHCI debemos usar la funcion atraviesaCrisis.
La funcion atraviesaCrisis recibe como parametro la ciudad que atraviesa la crisis.

Para que la ciudad azul tenga una crisis escribimos en la consola:
atraviesaCrisis azul

La consola nos devuelve la ciudad azul con un 10% menos en su costo de vida y sin la ultima atraccion que tenia:
UnaCiudad
    { nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales =
        [ "Teatro Español"
        , "Parque Municipal Sarmiento"
        ]
    , costoVida = 171
    }

--}

{--

Para que una ciudad tenga una reevaluacion, en la consola GHCI debemos usar la funcion reevaluarCiudad.
La funcion reevaluarCiudad recibe como primer parametro el numero de letras (configurable),
y como segundo parametro la ciudad a reevaluar.

Para que la ciudad azul tenga una reevaluacion escribimos en la consola:
reevaluarCiudad 14 azul

La consola nos devuelve lo siguiente:
UnaCiudad
    { nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales =
        [ "Teatro Espanol"
        , "Parque Municipal Sarmiento"
        , "Costanera Cacique Catriel"
        ]
    , costoVida = 187
    }

--}

------------ PUNTO 4.1 PARTE 2 ------------

type Evento = Ciudad -> Ciudad

-- Modelamos un año, donde queremos saber el numero que le corresponde y una serie de eventos que se produjeron en ese año
data Anio = UnAnio {
    numero :: Number
    , eventos :: [Evento]
} deriving (Show)

-- Reflejamos el paso de un año para una ciudad
reflejaAnioCiudad :: Anio -> Ciudad -> Ciudad
reflejaAnioCiudad anio ciudad = foldl (\ciudad evento -> evento ciudad) ciudad (eventos anio)

------------ PUNTO 4.2 PARTE 2 ------------

type Criterio = Ciudad -> Number

criterioCostoVida :: Criterio
criterioCostoVida = costoVida

criterioCantidadAtracciones :: Criterio
criterioCantidadAtracciones = length . atraccionesPrincipales

-- Función que nos dice si la ciudad tras un evento subió respecto a un criterio de comparacion 
subeRespectoCriterio :: Ciudad -> Criterio -> Evento -> Bool
subeRespectoCriterio ciudad criterio evento =  criterio ciudad < criterio (evento ciudad)

------------ PUNTO 4.3 PARTE 2 ------------

--Funcion que aplica eventos que hagan que el costo de vida de la ciudad suba
aumentanCostoVida :: Anio -> Ciudad -> Ciudad
aumentanCostoVida anio ciudad = foldl (\ciudad evento -> evento ciudad) ciudad (filter (subeRespectoCriterio ciudad criterioCostoVida) (eventos anio))

------------ PUNTO 4.4 PARTE 2 ------------

-- Función que nos dice si la ciudad tras un evento bajó respecto a un criterio de comparacion 
bajaRespectoCriterio :: Ciudad -> Criterio -> Evento -> Bool
bajaRespectoCriterio ciudad criterio evento =  criterio ciudad > criterio (evento ciudad)

--Funcion que aplica eventos que hagan que el costo de vida de la ciudad baje
bajanCostoVida :: Anio -> Ciudad -> Ciudad
bajanCostoVida anio ciudad = foldl (\ciudad evento -> evento ciudad) ciudad (filter (bajaRespectoCriterio ciudad criterioCostoVida) (eventos anio))

------------ PUNTO 4.5 PARTE 2 ------------

--Funcion que aplica eventos que hagan que el valor de la ciudad suba
aumentanValor :: Anio -> Criterio -> Ciudad -> Ciudad
aumentanValor anio criterio ciudad = foldl (\ciudad evento -> evento ciudad) ciudad (filter (subeRespectoCriterio ciudad criterio ) (eventos anio))

------------ PUNTO 5.1 PARTE 2 ------------

-- Funcion que indica si los eventos de un anio estan ordenados segun si incrementan el costo de vida respecto del anterior evento
estanOrdenadosLosEventos :: Anio -> Ciudad -> Bool
estanOrdenadosLosEventos (UnAnio numero []) ciudad = True
estanOrdenadosLosEventos (UnAnio numero [x]) ciudad = True
estanOrdenadosLosEventos (UnAnio numero (x : xs)) ciudad = obtieneDiferenciaCostoVida x ciudad < obtieneDiferenciaCostoVida (head xs) ciudad && estanOrdenadosLosEventos (UnAnio numero xs) ciudad

-- Funcion que obtiene la diferencia del cambio del costo de vida despues de aplicar un evento
obtieneDiferenciaCostoVida :: Evento -> Ciudad -> Number
obtieneDiferenciaCostoVida evento ciudad = costoVida (evento ciudad) - costoVida ciudad

------------ PUNTO 5.2 PARTE 2 ------------

--CIUDADES ORDENADAS
ciudadesOrdenadas :: Evento -> [Ciudad] -> Bool
ciudadesOrdenadas _ [] = True
ciudadesOrdenadas _ [_] = True
ciudadesOrdenadas evento (x : xs) = obtieneDiferenciaCostoVida evento x <= obtieneDiferenciaCostoVida evento (head xs) && ciudadesOrdenadas evento xs

------------ TO-DO PUNTO 5.3 PARTE 2 ------------

------------ PUNTO 6 Una serie de eventos interminables PARTE 2 ------------

-- Definimos el anio 2024
anio2024 :: Anio
anio2024 = UnAnio {
    numero = 2024
    , eventos = [atraviesaCrisis, reevaluarCiudad 7] ++ generaListaRemodelacionInfinita 1
}

-- Generamos una lista infinita de remodelaciones en la cual va aumentando de a 1 el incremento
generaListaRemodelacionInfinita :: Number -> [Evento]
generaListaRemodelacionInfinita x = remodelaCiudad x : generaListaRemodelacionInfinita  (x + 1)

------------  PUNTO 6 Eventos ordenados PARTE 2 ------------

{--

El resultado de aplicar la funcion estanOrdenadosLosEventos para el año 2024 devolveria False,
ya que sin importar que la lista infinita de remodelaCiudad vaya en aumento,
al aplicar los 3 primeros eventos (AtraviesaCrisis, reevaluarCiudad 7, remodelaCiudad 1) no estan ordenados de acuerdo al criterio de
aumento del costo de vida respecto del evento anterior. El evento remodelaCiudad 1 baja el costo de vida que tenia la ciudad despues de
aplicar el segundo evento reevaluarCiudad 7.

--}

------------ TO-DO PUNTO 6 Años ordenados  PARTE 2 ------------


------------ UTIL PARA EJECUTAR LOS TEST ------------

-- Instancia Eq para el tipo Ciudad para en los test poder comparar 2 valores de tipo Ciudad, sino el ShouldBe tira error
instance Eq Ciudad where
  (UnaCiudad n1 af1 ap1 cv1) == (UnaCiudad n2 af2 ap2 cv2) = n1 == n2 && af1 == af2 && ap1 == ap2 && cv1 == cv2

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
    , atraccionesPrincipales = ["Teatro Espanol", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"]
    , costoVida = 190 }
azulNuevaAtraccion :: Ciudad
azulNuevaAtraccion = UnaCiudad {
    nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Balneario Municipal Alte. Guillermo Brown", "Teatro Espanol", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"]
    , costoVida = 228 }
maipu :: Ciudad
maipu = UnaCiudad {
    nombre = "Maipu"
    , anioFundacion = 1878
    , atraccionesPrincipales = ["Fortín Kakel"]
    , costoVida = 115 
}
azulCrisis :: Ciudad
azulCrisis = UnaCiudad {
    nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Teatro Espanol", "Parque Municipal Sarmiento"]
    , costoVida = 171 }
azulRemodelada :: Ciudad
azulRemodelada = UnaCiudad {
    nombre = "New Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Teatro Espanol", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"]
    , costoVida = 285 }
azul2022 :: Ciudad
azul2022 = UnaCiudad {
    nombre = "New Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Teatro Espanol", "Parque Municipal Sarmiento"]
    , costoVida = 197.505 }
azul2015 :: Ciudad
azul2015 = UnaCiudad {
    nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Teatro Espanol", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"]
    , costoVida = 190 }
azulConMasCostoVida :: Ciudad
azulConMasCostoVida = UnaCiudad {
    nombre = "New Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Teatro Espanol", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"]
    , costoVida = 219.45
}
nullishConMasValor :: Ciudad
nullishConMasValor = UnaCiudad {
    nombre = "New Nullish"
    , anioFundacion = 1800
    , atraccionesPrincipales = []
    , costoVida = 147
}
azulConMenosCostoVida :: Ciudad
azulConMenosCostoVida = UnaCiudad {
    nombre = "Azul"
    , anioFundacion = 1832
    , atraccionesPrincipales = ["Teatro Espanol", "Parque Municipal Sarmiento"]
    , costoVida = 171
}

--- Anios para los test
anio2022 :: Anio
anio2022 = UnAnio {
    numero = 2022
    , eventos = [atraviesaCrisis, remodelaCiudad 5, reevaluarCiudad 7]
}
anio2015 :: Anio
anio2015 = UnAnio {
    numero = 2015
    , eventos = []
}
anio2023 :: Anio
anio2023 = UnAnio {
    numero = 2023
    , eventos = [atraviesaCrisis, agregaNuevaAtraccion "Parque", remodelaCiudad 10, remodelaCiudad 20]
}