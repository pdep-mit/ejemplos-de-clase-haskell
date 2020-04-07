import Test.Hspec
import Clase1

main :: IO ()
main = hspec $ do
   describe "Test doble" $ do
      it "doble duplica" $ do
         (doble 2) `shouldBe` 4