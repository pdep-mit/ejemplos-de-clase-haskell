-- Enunciado:
-- https://docs.google.com/document/d/1DoAh_2jGWL5jPeM0K0lQ4V-8MEtqmKV1c3d9eqc9SJQ/
import Minigolfito
import Test.Hspec

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (UnaHabilidad 25 60)
todd = UnJugador "Todd" "Ned" (UnaHabilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (UnaHabilidad 10 1)

habilidadDeEjemplo = UnaHabilidad 5 30

deberiaIncluirPalo :: [(Habilidad -> Tiro)] -> (Habilidad -> Tiro) -> Expectation
deberiaIncluirPalo unosPalos unPalo =
  any (mismoPalo unPalo) unosPalos `shouldBe` True

deberiaSerPalo :: (Habilidad -> Tiro) -> (Habilidad -> Tiro) -> Expectation
deberiaSerPalo unPalo otroPalo = mismoPalo unPalo otroPalo `shouldBe` True

mismoPalo unPalo otroPalo = unPalo habilidadDeEjemplo == otroPalo habilidadDeEjemplo

runTests = hspec $ do
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
          `shouldBe` (UnTiro {altura = 2, velocidad = 25, precision = 6})
      it "La velocidad de un tiro generado por un hierro es mayor para hierros más grandes" $ do
        tiroConHierro 3 `shouldSatisfy` ((< velocidad (tiroConHierro 5)).velocidad)
      it "La precision de un tiro generado por un hierro es menor para hierros más grandes" $ do
        tiroConHierro 3 `shouldSatisfy` ((> precision (tiroConHierro 5)).precision)
      it "La altura de un tiro generado por un hierro es mayor para hierros de 4 en adelante" $ do
        tiroConHierro 3 `shouldSatisfy` ((< altura (tiroConHierro 5)).altura)

    it "El golpe es el tiro resultante de cuando una persona usa un palo" $ do
      golpe bart putter
        `shouldBe` (UnTiro {velocidad = 10, precision = 120, altura = 0})

    it "La lista de palos incluye al putter" $ do
      palos `deberiaIncluirPalo` putter
    it "La lista de palos incluye a la madera" $ do
      palos `deberiaIncluirPalo` madera
    it "La lista de palos incluye al hierro número 7" $ do
      palos `deberiaIncluirPalo` hierro 7

  describe "Obstáculos del minigolfito" $ do
    context "Sobre los hoyos" $ do
      let tiroQuePasa = UnTiro {velocidad = 10, altura = 0, precision = 98}
      it "Un tiro muy lento no supera un hoyo" $ do
        tiroQuePasa {velocidad = 4} `shouldNotSatisfy` puedeSuperar hoyo
      it "Un tiro muy rápido no supera un hoyo" $ do
        tiroQuePasa {velocidad = 21} `shouldNotSatisfy` puedeSuperar hoyo
      it "Un tiro rasante y preciso puede superar un hoyo a la velocidad adecuada" $ do
        tiroQuePasa `shouldSatisfy` puedeSuperar hoyo
      it "Un tiro no rasante no supera un hoyo" $ do
        tiroQuePasa {altura = 1} `shouldNotSatisfy` puedeSuperar hoyo
      it "Un tiro poco preciso no supera un hoyo" $ do
        tiroQuePasa {precision = 95} `shouldNotSatisfy` puedeSuperar hoyo
      it "Un tiro luego de superar un hoyo se detiene" $ do
        superarObstaculo hoyo tiroQuePasa `shouldBe` UnTiro 0 0 0

    context "Sobre las lagunas" $ do
      let tiroQuePasa = UnTiro {velocidad = 90, altura = 4, precision = 20}
      it "Un tiro supera una laguna si la velocidad y altura son adecuadas" $ do
        tiroQuePasa `shouldSatisfy` puedeSuperar (laguna 3)
      it "Un tiro muy bajo no supera una laguna" $ do
        tiroQuePasa {altura = 0} `shouldNotSatisfy` puedeSuperar (laguna 3)
      it "Un tiro muy alto no supera una laguna" $ do
        tiroQuePasa {altura = 6} `shouldNotSatisfy` puedeSuperar (laguna 3)
      it "Un tiro lento no supera una laguna" $ do
        tiroQuePasa {velocidad = 80} `shouldNotSatisfy` puedeSuperar (laguna 3)
      it "Un tiro luego de superar una laguna el tiro tiene menos altura" $ do
        superarObstaculo (laguna 2) tiroQuePasa `shouldBe` tiroQuePasa {altura = 2}

    context "Sobre los túneles con rampita" $ do
      let tiroQuePasa = UnTiro {velocidad = 90, altura = 0, precision = 95}
      it "Un tiro rasante y preciso supera un túnel con rampita" $ do
        tiroQuePasa `shouldSatisfy` puedeSuperar tunelConRampita
      it "Un tiro poco preciso no supera un túnel con rampita" $ do
        tiroQuePasa {precision = 90} `shouldNotSatisfy` puedeSuperar tunelConRampita
      it "Un tiro no rasante no supera un túnel con rampita" $ do
        tiroQuePasa {altura = 0} `shouldNotSatisfy` puedeSuperar tunelConRampita
      it "Un tiro luego de superar un túnel con rampita es rasante, más veloz y muy preciso" $ do
        superarObstaculo tunelConRampita tiroQuePasa
          `shouldBe` UnTiro {precision = 100, altura = 0, velocidad = velocidad tiroQuePasa * 2}

  describe "Utilidad de los palos para los obstáculos" $ do
    it "A Bart le es útil el putter para superar un hoyo" $ do
      palosUtiles bart hoyo `deberiaIncluirPalo` putter
    it "A Bart le es útil un hierro número 5 para superar una laguna de 2 metros" $ do
      palosUtiles bart (laguna 2) `deberiaIncluirPalo` (hierro 5)

    it "Qué obstáculos consecutivos se puede superar con un tiro" $ do
      obstaculosConsecutivosSuperables (UnTiro {velocidad = 10, precision = 95, altura = 0})
        [tunelConRampita, tunelConRampita, hoyo] `shouldMatchList` [tunelConRampita, tunelConRampita]

    it "Cuál es el palo más útil para superar obstáculos consecutivos" $ do
      paloMasUtil bart [tunelConRampita, tunelConRampita, hoyo] `deberiaSerPalo` putter

  describe "El padre del niño que no gane..." $ do
    it "Si un jugador tiene menos puntos que el otro, pierde la apuesta su padre" $ do
      quienesPierdenLaApuesta [tunelConRampita, hoyo] [bart, rafa] `shouldMatchList ` ["Gorgory"]
    it "En caso de empate entre los que más puntos tienen, todos pierden la apuesta" $ do
      quienesPierdenLaApuesta [tunelConRampita, laguna 1, hoyo] [bart, todd] `shouldMatchList ` ["Homero", "Ned"]
