module RakeCliSupportSpec where

import Data.ByteString qualified as BS
import RakeCliSupport
import Relude
import Test.Hspec

spec :: Spec
spec = describe "RakeCliSupport" $ do
    describe "imageDimensionsFromBytes" $ do
        it "detects PNG dimensions" $
            imageDimensionsFromBytes (pngBytes 832 1248)
                `shouldBe` Just ImageDimensions{imageWidth = 832, imageHeight = 1248}

        it "detects JPEG dimensions" $
            imageDimensionsFromBytes (jpegBytes 1408 768)
                `shouldBe` Just ImageDimensions{imageWidth = 1408, imageHeight = 768}

        it "detects GIF dimensions" $
            imageDimensionsFromBytes (gifBytes 320 240)
                `shouldBe` Just ImageDimensions{imageWidth = 320, imageHeight = 240}

pngBytes :: Int -> Int -> BS.ByteString
pngBytes width height =
    BS.pack [137, 80, 78, 71, 13, 10, 26, 10]
        <> BS.pack [0, 0, 0, 13, 73, 72, 68, 82]
        <> word32BE width
        <> word32BE height

jpegBytes :: Int -> Int -> BS.ByteString
jpegBytes width height =
    BS.pack [255, 216, 255, 192]
        <> word16BE 11
        <> BS.singleton 8
        <> word16BE height
        <> word16BE width
        <> BS.pack [1, 1, 17, 0]

gifBytes :: Int -> Int -> BS.ByteString
gifBytes width height =
    BS.pack [71, 73, 70, 56, 57, 97]
        <> word16LE width
        <> word16LE height

word32BE :: Int -> BS.ByteString
word32BE value =
    BS.pack
        [ fromIntegral (value `div` 16777216)
        , fromIntegral (value `div` 65536)
        , fromIntegral (value `div` 256)
        , fromIntegral value
        ]

word16BE :: Int -> BS.ByteString
word16BE value =
    BS.pack
        [ fromIntegral (value `div` 256)
        , fromIntegral value
        ]

word16LE :: Int -> BS.ByteString
word16LE value =
    BS.pack
        [ fromIntegral value
        , fromIntegral (value `div` 256)
        ]
