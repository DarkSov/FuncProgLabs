{-# LANGUAGE OverloadedStrings #-}

import Codec.Picture
import Codec.Picture.Gif
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word

hideMessage :: BS.ByteString -> BS.ByteString -> BS.ByteString
hideMessage secretData coverData =
  BS.pack $ zipWith hideByte (BS.unpack secretData) (BS.unpack coverData)
  where
    hideByte :: Word8 -> Word8 -> Word8
    hideByte secretByte coverByte = (coverByte .&. 0xFE) .|. (secretByte `shiftR` 7)

revealMessage :: BS.ByteString -> BS.ByteString
revealMessage image = BS.pack $ map revealByte (BS.unpack image)
  where
    revealByte :: Word8 -> Word8
    revealByte byte = byte `shiftL` 1 .&. 0xFE

saveHiddenGif :: BS.ByteString -> FilePath -> IO ()
saveHiddenGif hiddenData outputPath = do
  eitherImage <- readGifImage outputPath
  case eitherImage of
    Left err -> putStrLn $ "Error reading GIF: " ++ err
    Right (GifImage {..}) -> do
      let framesWithHiddenData = map (applyToGifPixels $ hideMessage hiddenData) gifImages
      writeGifImages outputPath framesWithHiddenData

revealHiddenGif :: FilePath -> IO ()
revealHiddenGif imagePath = do
  eitherImage <- readGifImage imagePath
  case eitherImage of
    Left err -> putStrLn $ "Error reading GIF: " ++ err
    Right (GifImage {..}) -> do
      let revealedData = BS.pack $ concatMap (applyToGifPixels revealMessage) gifImages
      putStrLn $ "Revealed Data: " ++ show revealedData

main :: IO ()
main = do
  let coverImagePath = "cover.gif"
      secretData = "Hello, Haskell!" :: BS.ByteString
      outputImagePath = "hidden.gif"

  saveHiddenGif secretData coverImagePath outputImagePath
  putStrLn $ "Hidden Data saved to " ++ outputImagePath

  putStrLn "Revealing Hidden Data..."
  revealHiddenGif outputImagePath
