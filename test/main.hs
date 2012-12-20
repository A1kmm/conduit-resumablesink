import Test.Hspec
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Data.Conduit.ResumableSink
import Data.IORef
import Control.Monad.IO.Class

main :: IO ()
main = hspec $ do
  describe "use resumable sink" $ do
    it "behaves like normal conduit when -++$$ used immediately" $ do
      r <- C.runResourceT $
             (C.sourceList ["hello", "world"]) -++$$ (newResumableSink C.consume)
      r `shouldBe` ["hello", "world"]

    it "sink can be resumed" $ do
      r <- C.runResourceT $ do
             Right r1 <- ((C.sourceList ["hello", "world"]) +$$ C.consume)
             (C.sourceList ["hello", "world"]) -++$$ r1
      r `shouldBe` ["hello", "world", "hello", "world"]

    it "does correct cleanup" $ do
      s <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
      r <- C.runResourceT $ do
             Right r1 <-
               ((C.addCleanup (const . liftIO $ modifyIORef s (\(a,b,c) -> (a + 1, b, c))) (C.sourceList ["hello", "world"])) +$$
                          C.addCleanup (const . liftIO $ modifyIORef s (\(a,b,c) -> (a,b,c+1))) (C.consume))
             ((C.addCleanup (const . liftIO $ modifyIORef s (\(a, b, c) -> (a, b + 1, c))) (C.sourceList ["hello", "world"]))) -++$$ r1
       `shouldBe` ["hello", "world", "hello", "world"]
      sfinal <- readIORef s
      sfinal `shouldBe` (1, 1, 1)
