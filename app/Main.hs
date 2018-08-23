module Main where

import Control.Concurrent (forkIO, threadDelay)
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.IORef (atomicModifyIORef, atomicModifyIORef', newIORef, readIORef)
import Debug.Trace (trace)

foo :: Int -> Int
foo n = trace (">> working on " ++ show n) n
--foo = id

memoize :: (Eq a, Hashable a) => (a -> b) -> IO (a -> IO b)
memoize f = do
  cache <- newIORef HM.empty
  return $ \n -> do
    v <- HM.lookup n <$> readIORef cache
    case v of
      Just v' -> return v'
      Nothing -> do
          atomicModifyIORef' cache $ \c ->
            let w = HM.lookup n c in
              case w of
                Just w' -> (c,w')
                Nothing -> let v = f n in (HM.insert n v c, v)


memoize' :: (Eq a, Hashable a) => (a -> b) -> IO (a -> IO b)
memoize' f = do
  cache <- newIORef HM.empty
  return $ \n ->
    atomicModifyIORef' cache $ \c ->
      let w = HM.lookup n c in
        case w of
          Just w' -> (c, w')
          Nothing -> let v = f n in (HM.insert n v c, v)

main :: IO ()
main = do
  w <- memoize' foo
  let action = forkIO $ w 42 >>= print
      !actions = take 100 $ repeat action
  sequence_ actions
  threadDelay 2000000
