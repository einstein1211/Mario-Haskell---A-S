import qualified Data.ByteString as BS
import Codec.Picture
import Codec.Picture.Types (PixelRGBA8(..))
import System.Environment

-- Convert an image file to a Haskell String in Byte format
imageToHaskellString :: FilePath -> IO String
imageToHaskellString filePath = do
    -- Read the image from file
    eitherImg <- readImage filePath
    case eitherImg of
        Left err -> error err
        Right dynImg -> do
            let img     = convertRGBA8 dynImg
                width   = imageWidth img
                height  = imageHeight img

            -- Extract pixel data in BottomToTop order
            let pixels = [PixelRGBA8 r g b a | y <- [0 .. height - 1], 
                                                x <- [0 .. width - 1],
                                                let PixelRGBA8 r g b a = pixelAt img x (height - 1 - y)]
            -- Create a String representation of the pixel data
            let pixelStrings = concatMap (\(PixelRGBA8 r g b a) -> show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show a ++",") pixels
            -- let pixelStrings = concatMap (\(PixelRGBA8 r g b a) -> show a ++ ", " ++ show b ++ ", " ++ show g ++ ", " ++ show r ++", ") pixels
            return $ "[" ++ pixelStrings ++ "]"

-- Example usage
main :: IO ()
main = do
    -- let imagePath = "./images/LittleGoomba2.png"  -- Replace with your image path
    input <- getArgs 
    let imagePath = concat input
    -- return ()
    imageString <- imageToHaskellString imagePath
    writeFile "output.txt" imageString  -- Write String to Haskell file
    putStrLn "Converted image to Haskell String successfully."