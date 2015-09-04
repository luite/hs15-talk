{- |
     Pandoc filter for lightweight inline Haskell code blocks
 -}
{-# LANGUAGE OverloadedStrings #-}

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk

import           Control.Monad

import           Data.IORef
import           Data.List (partition)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.Exit
import           System.IO
import           System.Process

collectCodeBlocks :: IORef [Text] -> Maybe Format -> Block -> IO [Block]
collectCodeBlocks blocks fmt (CodeBlock (ident, classes, attrs) code)
 | fmt == Just (Format "revealjs") && "runhaskell" `elem` classes = do
     let code'           = T.pack code
         (_,c2)   = T.breakOn "\n---" code'
         shownCode = if T.null c2 then code' else T.strip (T.drop 4 c2)
         classes'  = "haskell" : filter (/="runhaskell") classes
         (attrs', runattrs) = partition ((`elem` ["width", "height"]) . fst) attrs
     bs <- readIORef blocks
     modifyIORef blocks (code':)
     return [ CodeBlock (ident, classes', attrs') (T.unpack shownCode)
            , Div (T.unpack $ divId (length bs + 1), ["runCode", "sourceCode"], runattrs) []
            ]
collectCodeBlocks _ _ b = return [b]

divId :: Int -> Text
divId n = T.pack ("runhaskell" ++ show n)

modName :: Int -> Text
modName n = T.pack ("M" ++ show n)

writeBlockModule :: Int -> Text -> IO ()
writeBlockModule n xs = do
  tmpl <- T.readFile "module.hs.tmpl"
  T.writeFile ("src/" ++ T.unpack mn ++ ".hs") $
    T.replace "$modulename" mn (T.replace "$code" xs tmpl)
  where
    mn = modName n

writeMainModule :: Int -> IO ()
writeMainModule n = do
  tmpl <- T.readFile "main.hs.tmpl"
  T.writeFile "src/main.hs" $
    T.replace "$imports" imports $ T.replace "$runinit" runinit tmpl
  where
    runinit = "runInit = " <> T.intercalate " >> " (map (\n -> modName n <> ".start \"" <> divId n <> "\"") [1..n])
    imports = mconcat $ map (\n->"import qualified "<>modName n<>"\n") [1..n]

compileCode :: IO (ExitCode, String, String)
compileCode = readProcessWithExitCode "ghcjs" ["-O", "-isrc", "src/main.hs"] ""

main :: IO ()
main = do
  r <- newIORef []
  toJSONFilter (collectCodeBlocks r)
  mods <- readIORef r
  sequence_ (zipWith writeBlockModule [1..] (reverse mods))
  writeMainModule (length mods)
  (e, out, err) <- compileCode
  when (e /= ExitSuccess) $ do
    putStrLn out
    hPutStrLn stderr err
  exitWith e
