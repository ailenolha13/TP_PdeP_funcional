module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  -- Test "Valor de una ciudad "
  describe "Test de la funcion obtieneValor" $ do
    it "El valor de la ciudad Baradero es 925" $ do
      obtieneValor baradero
        `shouldBe` 925
    it "El valor de la ciudad Nullish es 280" $ do
      obtieneValor nullish
        `shouldBe` 280
    it "El valor de la ciudad Caleta Olivia es 360" $ do
      obtieneValor caletaOlivia
        `shouldBe` 360

  -- Test "Alguna atraccion copada"
  describe "Test de la funcion tieneAtraccionCopada" $ do
    it "La ciudad Baradero no tiene atraccion copada" $ do
      tieneAtraccionCopada baradero
        `shouldBe` False
    it "La ciudad Nullish no tiene atraccion copada" $ do
      tieneAtraccionCopada nullish
        `shouldBe` False
    it "La ciudad Caleta Olivia tiene atraccion copada" $ do
      tieneAtraccionCopada caletaOlivia
        `shouldBe` True
    
  -- Test "Ciudad sobria"
  describe "Test de la funcion esSobria" $ do
    it "La ciudad Baradero con atracciones de mas de 14 letras es sobria" $ do
      esSobria 14 baradero
        `shouldBe` True
    it "La ciudad Baradero con atracciones de mas de 15 letras no es sobria" $ do
      esSobria 15 baradero
        `shouldBe` False
    it "La ciudad Nullish sin atracciones y con 5 letras no es sobria" $ do
      esSobria 5 nullish
        `shouldBe` False

  -- Test "Ciudad con nombre raro"
  describe "Test de la funcion tieneNombreRaro" $ do
    it "La ciudad Maipu no tiene nombre raro" $ do
      tieneNombreRaro maipu
        `shouldBe` False
    it "La ciudad azul tiene nombre raro" $ do
      tieneNombreRaro azul
        `shouldBe` True

  -- Test "Agregar una nueva atraccion"
  describe "Test de la funcion agregaNuevaAtraccion" $ do
    it "La ciudad Azul tiene 4 atracciones principales y su costo de vida es 228" $ do
      agregaNuevaAtraccion "Balneario Municipal Alte. Guillermo Brown" azul
      `shouldBe` azulNuevaAtraccion -- Creamos una instancia Eq en Library.hs para poder comparar 2 valores de tipo Ciudad en este test

  -- Test "Crisis"
  describe "Test de la funcion atraviesaCrisis" $ do
    it "La ciudad Azul tiene 2 atracciones principales y su costo de vida es 171" $ do
      atraviesaCrisis azul
      `shouldBe` azulCrisis -- Creamos una instancia Eq en Library.hs para poder comparar 2 valores de tipo Ciudad en este test

  -- Test "Remodelacion"
  describe "Test de la funcion remodelaCiudad" $ do
    it "La ciudad Azul remodelada pasa a llamarse New Azul y su costo de vida es 285" $ do
      remodelaCiudad 50 azul
      `shouldBe` azulRemodelada -- Creamos una instancia Eq en Library.hs para poder comparar 2 valores de tipo Ciudad en este test

  -- Test "Los anios pasan..."
  describe "Test de la funcion reflejaAnioCiudad" $ do
    it "La ciudad Azul refleja el paso del anio 2022 quedando con el nombre New Azul, su costo de vida en 197.505 y las atracciones Teatro Espanio y Parque Municipal Sarmiento " $ do
      reflejaAnioCiudad anio2022 azul
      `shouldBe` azul2022
    it "La ciudad Azul refleja el paso del anio 2015 quedando con el mismo costo de vida" $ do
      reflejaAnioCiudad anio2015 azul
      `shouldBe` azul2015

   -- Test "Algo mejor"
  describe "Test de la funcion subeRespectoCriterio" $ do
    it "La ciudad Azul tras atravesar una crisis no subio su costo de vida" $ do
      subeRespectoCriterio azul criterioCostoVida atraviesaCrisis
      `shouldBe` False
    it "La ciudad Azul tras el evento Agregar atraccion 'Monasterio Trapense' subio su costo de vida" $ do
      subeRespectoCriterio azul criterioCostoVida (agregaNuevaAtraccion "Monasterio Trapense")
      `shouldBe` True
    it "La ciudad Azul tras el evento Agregar atraccion 'Monasterio Trapense' subio la cantidad de atracciones" $ do
      subeRespectoCriterio azul criterioCantidadAtracciones (agregaNuevaAtraccion "Monasterio Trapense")
      `shouldBe` True
  
  -- Test "Costo de vida que suba"
  describe "Test de la funcion aumentanCostoVida" $ do
    it "La ciudad Azul tras eventos ocurridos en el año 2022 que aumentan su costo de vida queda con el nombre New Azul y su costo de vida pasa a 219.45" $ do
      aumentanCostoVida anio2022 azul
      `shouldBe` azulConMasCostoVida

  -- Test "Costo de vida que baje  "
  describe "Test de la funcion bajanCostoVida" $ do
    it "La ciudad Azul tras eventos ocurridos en el año 2022 que bajan su costo de vida queda con el nombre Azul y su costo de vida pasa a 171" $ do
      bajanCostoVida anio2022 azul
      `shouldBe` azulConMenosCostoVida

  -- Test "Valor que suba"
  describe "Test de la funcion aumentanValor" $ do
    it "La ciudad Nullish tras eventos ocurridos en el año 2022 que aumentan su valor queda con el nombre New Nullish y su costo de vida es 147" $ do
      aumentanValor criterioCostoVida anio2022 nullish
      `shouldBe` nullishConMasValor

  -- Test "Eventos Ordenados"
  describe "Test de la funcion estanOrdenadosLosEventos" $ do
    it "El anio 2022 sobre la ciudad azul tiene los eventos ordenados" $ do
      estanOrdenadosLosEventos anio2022 azul
      `shouldBe` True
    it "El anio 2023 no tiene los eventos ordenados" $ do
      estanOrdenadosLosEventos anio2023 azul
      `shouldBe` False

  -- Test "Ciudades Ordenadas"
  describe "Test de la funcion ciudadesOrdenadas" $ do
    it "Dado el evento remodelacion del 10%, las ciudades Caleta Olivia, Nullish, Baradero y Azul estan ordenadas" $ do
      ciudadesOrdenadas (remodelaCiudad 10) [caletaOlivia, nullish, baradero, azul]
      `shouldBe` True
    it "Dado el evento remodelacion del 10%, las ciudades Caleta Olivia, Azul y Baradero no estan ordenadas" $ do
      ciudadesOrdenadas (remodelaCiudad 10) [caletaOlivia, azul, baradero]
      `shouldBe` False

  -- Test "Años Ordenados"
  describe "Test de la funcion estanOrdenadosLosAnios" $ do
    it "Los años 2021, 2022 y 2023 no estan ordenados para Baradero" $ do
      estanOrdenadosLosAnios [anio2021, anio2022, anio2023] baradero
      `shouldBe` False
    it "Los años 2022, 2021 y 2023 estan ordenados para Baradero" $ do
      estanOrdenadosLosAnios [anio2022, anio2021, anio2023] baradero
      `shouldBe` True