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
  --describe "Test de la funcion reflejaAnioCiudad" $ do
    --it "La ciudad Azul refleja el paso del anio 2022 quedando con el nombre New Azul, su costo de vida en 197.505 y las atracciones Teatro Espanio y Parque Municipal Sarmiento " $ do
      --reflejaAnioCiudad anio2022 azul
      --`shouldBe` azul2022
  describe "Test de la funcion reflejaAnioCiudad" $ do
    it "La ciudad Azul refleja el paso del anio 2015 quedando con el mismo costo de vida" $ do
      reflejaAnioCiudad anio2015 azul
      `shouldBe` azul2015 
