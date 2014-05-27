import Test.Hspec
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Data.Conduit.ResumableSink
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource as R

main :: IO ()
main = hspec $ describe "use resumable sink" $ do
    it "behaves like normal conduit when -++$$ used immediately" $ do
      r <- R.runResourceT $
             C.sourceList ["hello", "world"] -++$$ newResumableSink C.consume
      r `shouldBe` ["hello", "world"]

    it "sink can be resumed" $ do
      r <- R.runResourceT $ do
             Right r1 <- C.sourceList ["hello", "world"] +$$ C.consume
             C.sourceList ["hello", "world"] -++$$ r1
      r `shouldBe` ["hello", "world", "hello", "world"]

    it "does correct cleanup" $ do
      s <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
      let clean f _ = liftIO $ modifyIORef s f
      
      r <- R.runResourceT $ do
             Right r1 <-
                C.addCleanup (clean incA) (C.sourceList ["hello", "world"]) 
                   +$$ C.addCleanup (clean incC) C.consume
             C.addCleanup (clean incB) (C.sourceList ["hello", "world"]) 
                   -++$$ r1
      r `shouldBe` ["hello", "world", "hello", "world"]
      sfinal <- readIORef s
      sfinal `shouldBe` (1, 1, 1)

incA, incB, incC :: (Int, Int, Int) -> (Int, Int, Int)      
incA (a,b,c) = (a+1,b,c)
incB (a,b,c) = (a,b+1,c)
incC (a,b,c) = (a,b,c+1)
