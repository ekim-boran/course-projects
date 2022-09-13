module A05_Check where

import A02_Defs
import A03_Defs
import A05
import A05_Defs
import Control.Concurrent
import Control.Monad.State.Lazy
import Data.Bits
import Data.List
import Data.Map as Map
import Data.Maybe
import Data.Ord
import Data.Word
import System.IO.Unsafe
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC hiding
  ( (.&.),
  )
import Test.Tasty.SmallCheck as SC
import Text.Printf

forkOSResult :: IO a -> IO (MVar a)
forkOSResult act = do
  var <- newEmptyMVar
  forkOS $ do
    value <- act
    putMVar var value
  return var

main :: IO ()
main = defaultMain (localOption (mkTimeout 5000000) tests)

tests :: TestTree
tests = testGroup "A05" [smokeTests, casTests]

smokeTests =
  testGroup
    "smoke tests"
    [ testCase "smokeTest disjoint" $
        unsafePerformIO (smokeTestDisjoint 16 1024)
          @?= True,
      testCase "smokeTest invariant" $
        unsafePerformIO (smokeTestInvariant 16 (16 * 1024))
          @?= True
    ]

casTests =
  testGroup
    "cas tests"
    [ testCase "casTest faa multi-locs" $
        unsafePerformIO (casTestFaa 128 1)
          @?= True,
      testCase "casTest faa multi-locs" $
        unsafePerformIO (casTestFaa 128 512)
          @?= True
    ]

smokeTestDisjoint :: Word32 -> Word32 -> IO Bool
smokeTestDisjoint nthread nloc = do
  mem <- newShrMem

  result <- forM [0 .. (nthread - 1)] $ \tid -> forkOSResult $ do
    result <- forM [0 .. (nloc - 1)] $ \lid -> do
      let loc = Loc $ Val $ tid * nloc + lid

      test1 <- loadShrMem loc mem
      storeShrMem loc (Val tid) mem
      test2 <- loadShrMem loc mem
      test3 <- casShrMem loc (Val tid) (Val $ tid + 1) mem
      test4 <- casShrMem loc (Val tid) (Val $ tid + 1) mem

      return $
        isNothing test1
          && test2
          == Just (Val tid)
          && test3
          == Just (True, Val tid)
          && test4
          == Just (False, Val $ tid + 1)

    return $ and result

  result <- mapM takeMVar result
  return $ and result

smokeTestInvariant :: Word32 -> Word32 -> IO Bool
smokeTestInvariant nthread nloc = do
  mem <- newShrMem

  forM_ [0 .. (nloc - 1)] $ \lid -> do
    let loc = Loc $ Val lid
    storeShrMem loc (Val (lid `rem` nthread)) mem

  result <- forM [0 .. (nthread - 1)] $ \tid -> forkOSResult $ do
    result <- forM [0 .. (nloc - 1)] $ \lid -> do
      let loc = Loc $ Val lid
      let ntid = lid `rem` nthread
      if tid == ntid
        then do
          casShrMem loc (Val tid) (Val $ tid + nthread) mem
          return True
        else do
          test <- loadShrMem loc mem
          return $
            test == Just (Val ntid) || test
              == Just
                (Val $ ntid + nthread)

    return $ and result

  result <- mapM takeMVar result
  return $ and result

casTestFaa :: Word32 -> Word32 -> IO Bool
casTestFaa nthread nloc = do
  mem <- newShrMem
  forM_ [0 .. (nloc - 1)] $ \lid -> do
    storeShrMem (Loc $ Val lid) (Val 0) mem

  result <- forM [0 .. (nthread - 1)] $ \tid -> forkOSResult $ do
    forM [0 .. (nloc - 1)] $ \lid -> do
      jitter <- randomIO :: IO Word8
      threadDelay $ fromIntegral jitter

      let loc = Loc $ Val lid
      val <- loadShrMem loc mem
      case val of
        Nothing -> return (-1)
        Just (Val val) -> loop loc val mem

  result <- mapM takeMVar result
  return $ transpose result |> all (\r -> sort r == [0 .. (nthread - 1)])
  where
    loop loc val mem = do
      result <- casShrMem loc (Val val) (Val $ val + 1) mem
      case result of
        Nothing -> return (-1)
        Just (success, Val oldval) ->
          if success then return oldval else loop loc oldval mem
