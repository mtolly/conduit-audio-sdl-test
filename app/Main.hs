{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent
import Data.Int (Int16)
import qualified SDL
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Data.Conduit (($$), await)
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.IO.Class (liftIO)

bufSize :: (Num a) => a
bufSize = 4096 -- in frames

player :: (V.Storable a, Num a) => AudioSource (ResourceT IO) a -> MVar (V.Vector a) -> IO (IO ())
player src var = do
  let len = bufSize * channels src
      mt = V.replicate len 0
  stop <- newEmptyMVar
  stopped <- newEmptyMVar
  _ <- forkIO $ runResourceT $ source (reorganize bufSize src) $$ let
    sink = do
      await >>= liftIO . putMVar var . \case
        Just v -> if V.length v == len then v else v V.++ V.replicate (len - V.length v) 0
        Nothing -> mt
      liftIO (tryTakeMVar stop) >>= \case
        Nothing -> sink
        Just () -> liftIO $ tryTakeMVar var >> putMVar stopped ()
    in sink
  return $ putMVar stop () >> takeMVar stopped

main :: IO ()
main = do
  SDL.initializeAll
  var <- newEmptyMVar :: IO (MVar (V.Vector Int16))
  (device, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate 44100
    , SDL.openDeviceFormat = SDL.Mandate SDL.Signed16BitNativeAudio
    , SDL.openDeviceChannels = SDL.Mandate SDL.Stereo
    , SDL.openDeviceSamples = bufSize
    , SDL.openDeviceCallback = let
      fill iov = tryTakeMVar var >>= \case
        Nothing -> MV.set iov 0
        Just v -> V.copy iov v
      in \case
        SDL.Signed16BitLEAudio -> fill
        SDL.Signed16BitBEAudio -> fill
        SDL.Signed16BitNativeAudio -> fill
        _ -> error "invalid audio format"
    , SDL.openDeviceUsage = SDL.ForPlayback
    , SDL.openDeviceName = Nothing
    }

  src <- sourceSnd "roots.flac"
  stop <- player src var
  SDL.setAudioDevicePlaybackState device SDL.Play
  _ <- getLine
  stop

  _ <- getLine

  src2 <- sourceSndFrom (Seconds 30) "roots.flac"
  stop2 <- player src2 var
  _ <- getLine
  stop2
