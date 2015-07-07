{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import qualified Hakyll
import Hakyll
    ( Configuration(..), Context, applyAsTemplate, compile
    , compressCssCompiler, constField, copyFileCompiler, create, dateField
    , fromList, getResourceBody, hakyllWith, idRoute
    , listField, loadAll, loadAndApplyTemplate, makeItem, match
    , pandocCompiler, recentFirst, relativizeUrls, route, setExtension
    , templateCompiler
    )


hakyllConfig :: Configuration
hakyllConfig = def { providerDirectory = "preprocessed-site"
                   , storeDirectory = ".hakyll-cache"
                   , tmpDirectory = ".hakyll-cache/tmp"
                   , destinationDirectory = "generated-site"
                   }

main :: IO ()
main = hakyllWith hakyllConfig $ do
    -- match "images/*" $ do
    --     route idRoute
    --     compile copyFileCompiler

    -- match "css/*" $ do
    --     route idRoute
    --     compile compressCssCompiler

    match "startbootstrap-clean-blog/img/*" $ do
        route idRoute
        compile copyFileCompiler

    match "startbootstrap-clean-blog/css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "startbootstrap-clean-blog/js/*" $ do
        route idRoute
        compile copyFileCompiler

    match "startbootstrap-clean-blog/fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    match "startbootstrap-clean-blog/templates/*" $ compile templateCompiler

    match "startbootstrap-clean-blog/index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = defaultContext
            indexBody <- getResourceBody
            indexWithContext <- applyAsTemplate indexCtx indexBody
            indexWithDefaultTemplate <-
                loadAndApplyTemplate "startbootstrap-clean-blog/templates/default.html" indexCtx indexWithContext
            relativizeUrls indexWithDefaultTemplate

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                             constField "title" "Archives"            `mappend`
                             defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = listField "posts" postCtx (return posts) `mappend`
                           constField "title" "Home"                `mappend`
                           defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

defaultContext :: Context String
defaultContext = Hakyll.defaultContext

defaultContextWithTitle :: String -> Context String
defaultContextWithTitle title = constField "title" title `mappend`
                                defaultContext
