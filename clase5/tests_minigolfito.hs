-- Enunciado:
-- https://docs.google.com/document/d/1DoAh_2jGWL5jPeM0K0lQ4V-8MEtqmKV1c3d9eqc9SJQ/
import Minigolfito
import Test.Hspec

habilidadDeEjemplo = Habilidad 5 30

deberiaIncluirPalo :: [(Habilidad -> Tiro)] -> (Habilidad -> Tiro) -> Expectation
deberiaIncluirPalo unosPalos unPalo = any (mismoPalo unPalo) unosPalos `shouldBe` True
mismoPalo unPalo otroPalo = unPalo habilidadDeEjemplo == otroPalo habilidadDeEjemplo

main = hspec $ do
  describe "Los palos que se usan en el minigolfito" $ do
    it "El putter genera un tiro rasante, lento y preciso" $ do
      putter habilidadDeEjemplo
        `shouldBe` (UnTiro {altura = 0, velocidad = 10, precision = 60})
    it "La madera genera un tiro elevado, rápido y poco preciso" $ do
      madera habilidadDeEjemplo
        `shouldBe` (UnTiro {altura = 5, velocidad = 100, precision = 15})

    context "Los hierros generan tiros que dependen de su número de palo" $ do
      let tiroConHierro n = hierro n habilidadDeEjemplo
      it "El tiro generado por un hierro depende de su número de palo" $ do
        tiroConHierro 5
          `shouldBe` (UnTiro {altura = 25, velocidad = 6, precision = 25})
      it "La velocidad de un tiro generado por un hierro es mayor para hierros más grandes" $ do
        tiroConHierro 3 `shouldSatisfy` ((< velocidad (tiroConHierro 5)).velocidad)
      it "La precision de un tiro generado por un hierro es menor para hierros más grandes" $ do
        tiroConHierro 3 `shouldSatisfy` ((> precision (tiroConHierro 5)).precision)
      it "La altura de un tiro generado por un hierro es mayor para hierros más grandes" $ do
        tiroConHierro 3 `shouldSatisfy` ((> altura (tiroConHierro 5)).altura)

    it "El golpe es el tiro resultante de cuando una persona usa un palo" $ do
      golpe bart putter
        `shouldBe` (UnTiro {velocidad = 10, precision = 120, altura = 0})

    it "La lista de palos incluye al putter" $ do
      palos `deberiaIncluirPalo` putter
    it "La lista de palos incluye a la madera" $ do
      palos `deberiaIncluirPalo` madera
    it "La lista de palos incluye al hierro número 7" $ do
      palos `deberiaIncluirPalo` hierro 7
