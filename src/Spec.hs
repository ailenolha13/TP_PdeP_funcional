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
  describe "Test de la funcion agregarNuevaAtraccion" $ do
    it "La ciudad Azul tiene 4 atracciones principales y su costo de vida es 228" $ do
      agregarNuevaAtraccion "Balneario Municipal Alte. Guillermo Brown" azul
      `shouldBe` azulNueva -- Creamos una instancia Eq en Library.hs para poder comparar 2 valores de tipo Ciudad en este test

