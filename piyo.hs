import System.IO

convertLine :: Handle -> IO [[Int]]
convertLine handle = do
  eof <- hIsEOF handle
  result <- if eof then
              return []
            else
              do
                contents <- hGetLine handle
                next <- convertLine handle
                let converted = map (\ x -> read [x] :: Int ) contents
                return (converted : next)
  return result

convert = do
    withFile "hoge.txt" ReadMode $ \handle -> do
      convertLine handle
