{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable
import Data.Function
import qualified Data.List as List
import Development.Shake
import Development.Shake.FilePath
import qualified LiterateX
import LiterateX.Renderer (Options (..))
import qualified LiterateX.Renderer as Renderer
import qualified LiterateX.Types.SourceFormat as SourceFormat
import qualified System.Directory as Directory
import qualified System.IO.Strict as Strict

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles = "_build"} $ do
  phony "process" $ do
    putInfo "[+] Processing files…"
    liftIO $ Directory.createDirectoryIfMissing True "./docs/.markdown"
    liftIO Directory.getCurrentDirectory
      >>= liftIO . putStrLn
    files' <- getDirectoryFiles "./docs/src" ["//*.hs"]
    let files = filter (/= "Main.hs") files'
    liftIO $ print files
    forM_ files $ \f -> do
      let fileName = toMarkdownFile f
      liftIO $
        LiterateX.transformFileToFile
          SourceFormat.DoubleDash
          literatexOptions
          ("./docs/src" </> f)
          fileName
      processFullProseModule fileName

literatexOptions :: Options
literatexOptions =
  Renderer.defaultOptions
    { codeLanguage = Just "haskell"
    , numberCodeLines = False
    }

toMarkdownFile :: FilePath -> FilePath
toMarkdownFile f =
  f
    & dropExtension
    & appendExtension ".md"
    & prependPath "./docs/.markdown"

{-| Prepend the second argument to the first.
 To be used in pipelines.

 > "foobar" & prependPath "book"
 "book/foobar"
-}
prependPath :: FilePath -> FilePath -> FilePath
prependPath prefixPath path = prefixPath </> path

{-| Append the first argument to the second as an extension.
 To be used in pipelines.

 > "foobar" & appendExtension ".md"
 "foobar.md"
-}
appendExtension :: FilePath -> FilePath -> FilePath
appendExtension extension path = path <.> extension

processFullProseModule :: (MonadIO m) => FilePath -> m ()
processFullProseModule filepath = do
  file <- liftIO $ Strict.readFile filepath
  when ("{-# ANN module False #-}" `List.isInfixOf` file) $
    liftIO $
      writeFile filepath (unlines $ List.drop 5 $ lines file)
