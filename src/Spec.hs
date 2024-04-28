module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de la funcion obtieneValor" $ do
    it "185 * 5 = 925" $ do
      obtieneValor UnaCiudad {
        nombre = "Baradero"
        , anioFundacion = 1615
        , atraccionesPrincipales = ["Parque del Este", "Museo Alejandro Barbich"]
        , costoVida= 150 } 
        `shouldBe` 925
    it "140 * 2 = 280" $ do
      obtieneValor UnaCiudad {
        nombre = "Nullish"
        , anioFundacion = 1800
        , atraccionesPrincipales = []
        , costoVida= 140 } 
        `shouldBe` 280
    it "120 * 3 = 360" $ do
      obtieneValor UnaCiudad {
        nombre = "Caleta Olivia"
        , anioFundacion = 1901
        , atraccionesPrincipales = ["El Gorosito", "Faro Costanera"]
        , costoVida= 120 } 
        `shouldBe` 360