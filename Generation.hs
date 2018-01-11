{-#LANGUAGE MultiWayIf#-}
module Generation where

import Control.Monad.State
import System.Random

import Sentence
import Logics

-- COMPUTATION STATE DEFINITIONS --

data GenState = GenState
  { rand :: StdGen
  , model :: Model
  , depth :: Int
  }

getRand :: State GenState Int
getRand = do
  st <- get
  let (out, rg) = next $ rand st
  put (st{rand = rg})
  return out

getRandRange :: Int -> Int -> State GenState Int
getRandRange a b = fmap ((+a) . (`mod` (b - a))) getRand

withDecrDepth :: State GenState a -> State GenState a
withDecrDepth st = do
  d <- gets depth
  modify $ \s -> s{depth = d - 1}
  a <- st
  modify $ \s -> s{depth = d}
  return a

-- GENERATION --

generateSentence :: StdGen -> Bool3 -> Int -> Model -> Sentence
generateSentence randGen value depth model =
  evalState (recGenerate value) (GenState randGen model depth)

recGenerate :: Bool3 -> State GenState Sentence
recGenerate value = do
  d <- gets depth
  if d == 0 then finalize value
    else do
    rn <- fmap (`mod`10) getRand
    swap <- fmap (==0) (getRandRange 0 5)
    case value of
      True3 -> if swap then Not <$> genFalse else genTrue
      False3 -> if swap then Not <$> genTrue else genFalse
      Unknown3 -> (if swap then Not else id) <$> genUnknown

nextIter :: Bool3 -> State GenState Sentence
nextIter val = withDecrDepth $ recGenerate val

finalize :: Bool3 -> State GenState Sentence
finalize value = do
  rn <- getRand
  (t, f, u) <- gets model
  return . PlainSentence $ case value of
                             Unknown3 -> u !! (rn `mod` length u)
                             True3 -> t !! (rn `mod` length t)
                             False3 -> f !! (rn `mod` length f)

genFalse :: State GenState Sentence
genFalse = do
  rn <- getRandRange 0 10
  (t, f, u) <- gets model
  if | rn < 2 -> finalize False3
     | rn < 4 -> -- OR FALSE
         Or <$> nextIter False3 <*> nextIter False3
     | rn < 6 -> do -- AND FALSE
         s <- getRandRange 0 5
         let (lv, rv) = case s of
               0 -> (True3, False3)
               1 -> (False3, True3)
               2 -> (False3, False3)
               3 -> (False3, Unknown3)
               4 -> (Unknown3, False3)
         p <- nextIter lv
         q <- nextIter rv
         return $ And p q
     | rn < 8 -> -- IMPL FALSE
         Impl <$> nextIter True3 <*> nextIter False3
     | otherwise -> do -- IFF FALSE
         s <- fmap (fromBool . (==0)) (getRandRange 0 1)
         Iff <$> nextIter s <*> nextIter (not3 s)

genTrue  :: State GenState Sentence
genTrue = do
  rn <- getRandRange 0 10
  (t, f, u) <- gets model
  if | rn < 2 -> finalize True3
     | rn < 4 -> do -- OR TRUE
         s <- getRandRange 0 5
         let (lv, rv) = case s of
               0 -> (True3, False3)
               1 -> (False3, True3)
               2 -> (True3, True3)
               3 -> (True3, Unknown3)
               4 -> (Unknown3, True3)
         p <- nextIter lv
         q <- nextIter rv
         return $ Or p q
     | rn < 6 -> -- AND TRUE
         And <$> nextIter True3 <*> nextIter True3
     | rn < 8 -> do -- IMPL TRUE
         s <- getRandRange 0 5
         let (lv, rv) = case s of
               0 -> (False3, False3)
               1 -> (False3, True3)
               2 -> (True3, True3)
               3 -> (False3, Unknown3)
               4 -> (Unknown3, True3)
         p <- nextIter lv
         q <- nextIter rv
         return $ Impl p q
     | otherwise -> do -- IFF TRUE
         s <- fmap (fromBool . (==0)) (getRandRange 0 1)
         Iff <$> nextIter s <*> nextIter s

genUnknown  :: State GenState Sentence
genUnknown = do
  rn <- getRandRange 0 10
  (t, f, u) <- gets model
  if | rn < 2 -> finalize Unknown3
     | rn < 4 -> do -- AND UNKNOWN
         s <- getRandRange 0 3
         let (lv, rv) = case s of
               0 -> (True3, Unknown3)
               1 -> (Unknown3, True3)
               2 -> (Unknown3, Unknown3)
         p <- nextIter lv
         q <- nextIter rv
         return $ And p q
     | rn < 6 -> do -- OR UNKNOWN
         s <- getRandRange 0 3
         let (lv, rv) = case s of
               0 -> (False3, Unknown3)
               1 -> (Unknown3, False3)
               2 -> (Unknown3, Unknown3)
         p <- nextIter lv
         q <- nextIter rv
         return $ Or p q
     | rn < 8 -> do -- IMPL UNKNOWN
         s <- getRandRange 0 3
         let (lv, rv) = case s of
               0 -> (True3, Unknown3)
               1 -> (Unknown3, False3)
               2 -> (Unknown3, Unknown3)
         p <- nextIter lv
         q <- nextIter rv
         return $ Impl p q
     | otherwise -> do -- IFF UNKNOWN
         s <- getRandRange 0 2
         let (lv, rv) = case s of
               0 -> (True3, Unknown3)
               1 -> (Unknown3, False3)
         p <- nextIter lv
         q <- nextIter rv
         return $ Iff p q

