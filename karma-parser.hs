import System.IO
import Control.Monad

main :: IO ()
main = do
    -- std in/out tweaking
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    -- Error reporting if we need
    hSetBuffering stderr LineBuffering


    -- Robustify this to handle EOF
    forever $ do
        input <- hGetLine stdin

        -- Aeson process the json

        hPutStrLn stdout "{'karma': 'this'}"
