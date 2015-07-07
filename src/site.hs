{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import Data.Maybe (maybe)
import qualified Hakyll
import Hakyll
    ( Compiler, Configuration(..), Context, Identifier, Item, applyAsTemplate
    , compile, compressCssCompiler, constField, copyFileCompiler, create
    , dateField, fromList, getResourceBody, hakyllWith, idRoute, listField
    , loadAll, loadAndApplyTemplate, makeItem, match, pandocCompiler
    , recentFirst, relativizeUrls, route, setExtension, templateCompiler
    )


hakyllConfig :: Configuration
hakyllConfig = def { providerDirectory = "preprocessed-site"
                   , storeDirectory = ".hakyll-cache"
                   , tmpDirectory = ".hakyll-cache/tmp"
                   , destinationDirectory = "generated-site"
                   }

main :: IO ()
main = hakyllWith hakyllConfig $ do

    -- templates for other routes
    match "startbootstrap-clean-blog/templates/*" $ compile templateCompiler

    -- images
    match "startbootstrap-clean-blog/img/*" $ do
        route idRoute
        compile copyFileCompiler

    -- CSS
    match "startbootstrap-clean-blog/css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Javascript
    match "startbootstrap-clean-blog/js/*" $ do
        route idRoute
        compile copyFileCompiler

    -- web fonts
    match "startbootstrap-clean-blog/fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    -- index.html
    match "startbootstrap-clean-blog/index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = listField "posts" postCtx (return posts) `mappend`
                           defaultContext
            indexBody <- getResourceBody
            indexWithContext <- applyAsTemplate indexCtx indexBody
            applyDefaultTemplate indexCtx indexWithContext

    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let indexCtx = listField "posts" postCtx (return posts) `mappend`
    --                        constField "title" "Home"                `mappend`
    --                        defaultContext
    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

    -- blog posts
    match "startbootstrap-clean-blog/posts/*" $ do
        route $ setExtension "html"
        compile $ do
            let subHeadingCtx = field "subHeading" createSubHeadingForPost postCtx
            pandocCompiler
            >>= loadAndApplyTemplate "startbootstrap-clean-blog/templates/post.html" subHeadingCtx
            >>= applyDefaultTemplate subHeadingCtx

    ---------------------------------------
    --           Stock routes            --
    -- Left for documentational purposes --
    ---------------------------------------

    -- match (fromList ["about.rst", "contact.markdown"]) $ do
    --     route $ setExtension "html"
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx = listField "posts" postCtx (return posts) `mappend`
    --                          constField "title" "Archives"            `mappend`
    --                          defaultContext
    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls

    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let indexCtx = listField "posts" postCtx (return posts) `mappend`
    --                        constField "title" "Home"                `mappend`
    --                        defaultContext
    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

defaultContext :: Context String
defaultContext = Hakyll.defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

defaultContextWithTitle :: String -> Context String
defaultContextWithTitle title = constField "title" title `mappend`
                                defaultContext

applyDefaultTemplate :: Context a -> Item a -> Compiler (Item String)
applyDefaultTemplate context preTemplateItem = do
    postTemplateItem <- loadAndApplyTemplate defaultTemplate context preTemplateItem
    relativizeUrls postTemplateItem

defaultTemplate :: Identifier
defaultTemplate = "startbootstrap-clean-blog/templates/default.html"

createSubHeadingContentForPost :: Item a -> Compiler String
createSubHeadingContentForPost item = do
        ident <- itemIdentifier item
        maybeSubHeading <- getMetadataField ident "subHeading"
        maybePostedBy <- getMetadataField ident "postedBy"
        date <- getMetadataField' ident "date"
        let subHeadingLine = fromMaybe "" maybeSubHeading
            subHeadingHtml = "<h2 class=\"subheading\">" ++ subHeadingLine ++ "</h2>"
            postedByLine = fromMaybe "" maybePostedBy
            postedByHtml = "<span class=\"meta\">Posted by " ++ postedByLine ++ " on " ++ date ++ "</span>"
        return $ subHeadingHtml ++ postedByHtml
-- subHeading: Problems look mighty small from 150 miles up
-- postedBy: Dennis Gosnell
-- headingContent: <div class="post-heading">
--                     <h1>Man must explore, and this is exploration at its greatest</h1>
--                     <h2 class="subheading">Problems look mighty small from 150 miles up</h2>
--                     <span class="meta">Posted by <a href="#">Start Bootstrap</a> on July 10, 2015</span>
