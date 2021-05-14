import LifeGame
import qualified Data.Map as Map
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

initialData = do
  withFile "hoge.txt" ReadMode $ \handle -> do
    convertLine handle

convertFromInput f c acc = acc ++ [f c $ length acc]
genUnitData x c y = ([x, y], c)

piyo plots =
  Map.elems $ Map.foldrWithKey (\key value acc -> Map.insertWith (++) (key !! 0) [value] acc) Map.empty plots

output (index,plots) =
  withFile ("gen_" ++ show index ++ ".txt") WriteMode $ \handle -> do
    mapM ((hPutStrLn handle) . foldr (++) "" . map show) plots 
    return ()

huga = do
  input <- initialData
  let converted = Map.fromList $ foldr (++) [] $ foldr (convertFromInput (\c len -> foldr (convertFromInput $ genUnitData $ len) [] c)) [] input
  let result = take 10 $ iterate calcNextGen converted
  mapM output $ zip [1..] $ map piyo result
  return ()
