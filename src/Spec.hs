module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  -- Test "Valor de una ciudad "
  describe "Test de la funcion obtieneValor" $ do
    it "El valor de la ciudad Baradero es 925" $ do
      obtieneValor UnaCiudad {
        nombre = "Baradero"
        , anioFundacion = 1615
        , atraccionesPrincipales = ["Parque del Este", "Museo Alejandro Barbich"]
        , costoVida = 150 } 
        `shouldBe` 925
    it "El valor de la ciudad Nullish es 280" $ do
      obtieneValor UnaCiudad {
        nombre = "Nullish"
        , anioFundacion = 1800
        , atraccionesPrincipales = []
        , costoVida = 140 } 
        `shouldBe` 280
    it "El valor de la ciudad Caleta Olivia es 360" $ do
      obtieneValor UnaCiudad {
        nombre = "Caleta Olivia"
        , anioFundacion = 1901
        , atraccionesPrincipales = ["El Gorosito", "Faro Costanera"]
        , costoVida = 120 } 
        `shouldBe` 360
  -- Test "Alguna atraccion copada"
  describe "Test de la funcion tieneAtraccionCopada" $ do
    it "La ciudad Baradero no tiene atraccion copada" $ do
      tieneAtraccionCopada UnaCiudad {
        nombre = "Baradero"
        , anioFundacion = 1615
        , atraccionesPrincipales = ["Parque del Este", "Museo Alejandro Barbich"]
        , costoVida = 150 } 
        `shouldBe` False
    it "La ciudad Nullish no tiene atraccion copada" $ do
      tieneAtraccionCopada UnaCiudad {
        nombre = "Nullish"
        , anioFundacion = 1800
        , atraccionesPrincipales = []
        , costoVida = 140 } 
        `shouldBe` False
    it "La ciudad Caleta Olivia tiene atraccion copada" $ do
      tieneAtraccionCopada UnaCiudad {
        nombre = "Caleta Olivia"
        , anioFundacion = 1901
        , atraccionesPrincipales = ["El Gorosito", "Faro Costanera"]
        , costoVida = 120 } 
        `shouldBe` True