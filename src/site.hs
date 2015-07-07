{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import Data.Maybe (fromMaybe)
import qualified Hakyll
import Hakyll
    ( Compiler, Configuration(..), Context, Identifier, Item, applyAsTemplate
    , compile, compressCssCompiler, constField, copyFileCompiler, create
    , dateField, debugCompiler, field, fromList, getMetadata, getMetadataField, getMetadataField'
    , getResourceBody, hakyllWith, idRoute, itemIdentifier, listField
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
            posts <- recentFirst =<< loadAll "startbootstrap-clean-blog/posts/*"
            let indexCtx = listField "posts" postCtx (return posts) `mappend`
                           defaultContext
            indexBody <- getResourceBody
            indexWithContext <- applyAsTemplate indexCtx indexBody
            applyDefaultTemplate indexCtx indexWithContext

    -- blog posts
    match "startbootstrap-clean-blog/posts/*" $ do
        route $ setExtension "html"
        compile $ do
            let subHeadingCtx =
                    field "subHeadingContent" createSubHeadingContentForPost `mappend`
                    postCtx
            pandocOut <- pandocCompiler
            postTemplateOut <- loadAndApplyTemplate "startbootstrap-clean-blog/templates/post.html" subHeadingCtx pandocOut
            applyDefaultTemplate subHeadingCtx postTemplateOut

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

defaultContext :: Context String
defaultContext = Hakyll.defaultContext

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend`
          defaultContext

applyDefaultTemplate :: Context a -> Item a -> Compiler (Item String)
applyDefaultTemplate context preTemplateItem = do
    postTemplateItem <- loadAndApplyTemplate defaultTemplate context preTemplateItem
    relativizeUrls postTemplateItem

defaultTemplate :: Identifier
defaultTemplate = "startbootstrap-clean-blog/templates/default.html"

createSubHeadingContentForPost :: Item a -> Compiler String
createSubHeadingContentForPost item = do
        let ident = itemIdentifier item
        maybeSubHeading <- getMetadataField ident "subHeading"
        maybePostedBy <- getMetadataField ident "postedBy"
        let subHeading = fromMaybe "" maybeSubHeading
            subHeadingHtml = "<h2 class=\"subheading\">" ++ subHeading ++ "</h2>"
            postedBy = fromMaybe "" maybePostedBy
            postedByHtml = "<span class=\"meta\">Posted by " ++ postedBy ++ "</span>"
        return $ subHeadingHtml ++ postedByHtml
