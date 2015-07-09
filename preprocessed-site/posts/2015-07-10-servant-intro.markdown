---
title: ARoW.info Blog -- Type-level Things, Type Families, and Servant
headingBackgroundImage: ../img/post-bg.jpg
headingDivClass: post-heading
heading: Type-level Things, Type Families, and Servant
subHeading: A look at advanced GHC features used in Servant
postedBy: Dennis Gosnell
---

Servant is a really nice library for building REST APIs in Haskell.  However,
it uses advanced GHC features which may not be familiar to some Haskell
programmers.  In this article, I explain type-level strings, type-level lists,
type-level operators, and type families.  Finally, I use code from
servant-server to explain how these features are used in practice.

This article is aimed at people who have a basic familiarity with Haskell.
This includes understanding things like typeclasses, applicatives, monads,
monad transformers, pointfree style, ghci, etc.

## Servant Example

Here is a simple example of using servant-server.  This code will be referred
to throughout the article.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
    ( (:>), (:<|>)((:<|>)), Get, JSON, Proxy(..), ServantErr, ServerT, serve )

-- | A representation of our REST API at the type level.
--
-- This defines two routes:
--   * /dogs -- Responds to HTTP GET with a list of integers in JSON format.
--   * /cats -- Responds to HTTP GET with a list of Strings in JSON format.
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]

-- | A Warp 'Application' that will serve our API.
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

-- | Our entire API.  You can see that it is a combination of the 'dogNums'
-- handler and the 'cats' handler.
myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats

-- | A handler for the /dogs route.  It just returns a list of the integers
-- one to four.
dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

-- | A handler for the /cats route.
cats :: EitherT ServantErr IO [String]
cats = return ["long-haired", "short-haired"]

-- | Run our 'app' as a Warp 'Application'.
main :: IO ()
main = run 32323 $ logStdoutDev app
```

The example project can be found [on
Github](https://gist.github.com/cdepillabout/c2b8b1807e1f571fdb45#file-example-hs).
The comments in the code should give a good idea as to what is going on, but
if you would like a better introduction, the [Servant
tutorial](http://haskell-servant.github.io/tutorial/) is very good.

The following steps can be used to download and run the code.  The
[stack](https://github.com/commercialhaskell/stack) build tool is used.

```bash
$ git clone https://gist.github.com/c2b8b1807e1f571fdb45.git
$ mv c2b8b1807e1f571fdb45 small-servant-example
$ cd small-servant-example
$ stack build
$ stack exec servant-notes
```

This runs a Warp server on port 32323.  With the server running, `curl` can be
used to test the API.

```bash
$ curl http://localhost:32323/dogs
[1,2,3,4]
$ curl http://localhost:32323/cats
["long-haired","short-haired"]
$
```

The code can also be opened in `ghci`.

```haskell
$ stack ghci
ghci> :load example.hs
ghci> :info app
app :: Application      -- Defined at example.hs:17:1
ghci>
```

## Type-Level Strings

Recent versions of GHC support [type-level
strings](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/type-level-literals.html).
What's a type-level string?  Well, let's play around with it in the REPL.

First, the DataKinds language extension needs to be enabled.

```haskell
ghci> :set -XDataKinds
ghci>
```

Let's try to get the kind of a type-level string:

```haskell
ghci> :kind "hello"
"hello" :: GHC.TypeLits.Symbol
ghci>
```

Hmm, the type-level string appears to be of kind
[GHC.TypeLits.Symbol](https://hackage.haskell.org/package/base-4.8.0.0/docs/GHC-TypeLits.html#t:Symbol).
What can be done with this?

Looking at the GHC.TypeLits module, there appears to be a
[symbolVal](https://hackage.haskell.org/package/base-4.8.0.0/docs/GHC-TypeLits.html#v:symbolVal)
function. It can be used to get back the **value** of the type-level string.

Let's try this out in ghci.  `symbolVal` and
[Data.Proxy.Proxy](https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Proxy.html#t:Proxy)
need to be imported.  `Proxy` is used to "proxy" the type-level literal.

```haskell
ghci> import GHC.TypeLits
ghci> import Data.Proxy
ghci> symbolVal (Proxy :: Proxy "hello")
"hello"
ghci>
```

This is really cool!  We are able to get back the **concrete value** of
something that only exists on the **type level**!

How does servant use this?  Recall the `MyAPI` type defined near the
top of this article:

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

`"dogs"` and `"cats"` are type-level strings.  At the end of this article we will
look at some servant-server code and confirm that it is using `symbolVal` to get
the value of the type-level strings.

## Type-Level Lists

Just like type-level strings, type-level lists can also be defined.

First, the `DataKinds` language extension needs to be enabled.

```haskell
ghci> :set -XDataKinds
ghci>
```

Let's look at the kind of a type-level empty list:

```haskell
ghci> :kind! []
[] :: * -> *
ghci>
```

No, wait, that's not right.  That's just the kind of the normal list
construtor.  How do we write a type-level list?

Take quick peek at the GHC page on [datatype
promotion](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/promotion.html).
The first section is pretty interesting, as is the section on the [promoted
list and tuple
types](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/promotion.html#promoted-lists-and-tuples).
There is a short example of a heterogeneous list (or `HList`).  A heterogeneous
list is a list that has elements of different types.  In the example, `foo2`
represents a heterogeneous list with two elements, `Int` and `Bool`.

From the example, you can see that type-level lists can be defined by
putting a quote in front of the opening bracket:

```haskell
ghci> :kind! '[]
'[] :: [k]
ghci>
```

Type-level lists can also be defined with multiple elements:

```haskell
ghci> :kind! '[Int, Bool, String]
'[Int, Bool, String] :: [*]
ghci>
```

Going back to the `MyAPI` example from above, servant is using
type-level lists to represent the available content-type encodings of the
response.

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

Servant is only willing to send back responses in JSON.  (Because JSON is the
only type in the type-level list).

Additional content types could also be specified:

```haskell
type MyAPI = "dogs" :> Get '[JSON, FormUrlEncoded] [Int]
        :<|> "cats" :> Get '[JSON, PlainText] Text
```

(However, to get this to compile, there would need to be an instance of
`ToFormUrlEncoded [Int]`.)  The `/dogs` route will return either JSON or
form-encoded values.  The `/cats` route will return either JSON or plain text.

I'm not going to go into how type-level lists are used in servant-server, but
if you're interested you may want to start with reading the [Get instance for
HasServer](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L230),
which will take you to the
[methodRouter](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L123)
function, which will take you to the
[AllCTRender](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant/src/Servant/API/ContentTypes.hs#L158)
typeclass.  The AllCTRender typeclass/instance is where the real magic starts happening.

Oliver Charles has an [interesting
post](https://ocharles.org.uk/blog/posts/2014-08-07-postgresql-simple-generic-sop.html)
on the generics-sop package where he talks a little about heterogeneous lists.


## Type-Level Operators

In the servant example code above, there are two type-level operators being
used:
[`(:>)`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant/src/Servant/API/Sub.hs#L17)
and
[`(:<|>)`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant/src/Servant/API/Alternative.hs#L19).
Type-level operators are similar to normal data types---they are just composed
of symbols instead of letters.

Let's look at how `(:>)` and `(:<|>)` are defined in servant:

```haskell
data path :> a

data a :<|> b = a :<|> b
```

If we didn't want to write them infix, they could be writen like this:

```haskell
data (:>) path a

data (:<|>) a b = (:<|>) a b
```

In fact, if these data types were writen with letters instead of symbols,
they would look something like this:

```haskell
data Foo path a

data Bar a b = Bar a b
```

You can see that `(:>)` and `(:<|>)` are just normal datatype definitions. They
only look weird because they are made of symbols and written infix.

Type operators help when writing long type definitions.  They keep the long
type definition easy to understand.  Take the following API definition:

```haskell
type MyAPI = "foo" :> "bar" >: Get '[JSON] [Int]
```

This defines the route `/foo/bar`.  Rewriting this prefix would look like this:

```haskell
type MyAPI = (:>) "foo" ((>:) "bar" (Get '[JSON] [Int]))
```

You can see how much easier the infix style is to read!

**NOTE**: The `TypeOperators` language extension is needed to use the above code.

The GHC manual has a section about
[type-operators](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/data-type-extensions.html#type-operators).

You may be thinking, "*These type operators are pretty neat, but how are they
actually used?  They just look like confusing data types!*"  Well, we'll get to
that in a minute.  Before we can jump into the servant code, we need to get a
basic understanding of type families.

## Type Families

Type families are a relatively simple addition to Haskell that allow the user
to do some computation at the type level.  However, if you google for [type
families](https://www.google.co.jp/search?q=haskell+type+families&ie=utf-8&oe=utf-8&gws_rd=cr&ei=OzuSVZSMA6S-mAX044aoCQ),
it's easy to get scared.

The first result is the [GHC/Type
families](https://wiki.haskell.org/GHC/Type_families) article on the Haskell
Wiki.  This is written with an advanced Haskeller in mind.  Don't worry if it's
too hard.  (The other problem is that most of their examples use data families
instead of type synonym families--which I introduce below.  Most of the real world
Haskell code I've seen uses type synonym families much more than data families).

The second link is to the [type-families
page](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-families.html)
in the GHC manual.  It's good if you already know about type families and just
a refresher, but it's not good as an introduction to type families.

The third result is an
[article](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/type-families-and-pokemon)
on FP Complete.  It gets points for being about Pokemon, but the
setup/motivation for using type families is way too long.

The fourth result is an introduction to [type
families](https://ocharles.org.uk/blog/posts/2014-12-12-type-families.html) by
Oliver Charles.  It's the best of the bunch, but it is slightly hard to follow
if you've never used MVars, IORefs, etc.

I wrote a super simple *tl;dr* [presentation about type
families](https://cdepillabout.github.io/haskell-type-families-presentation).
Originally I wrote it in Japanese for a Haskell Lightening Talk in Tokyo, but I
recently translated it to English upon the request from someone in the **#haskell**
room in the [functional programming slack community](http://fpchat.com/).  If
you aren't sure about type families, please read that presentation and then
proceed to the next section.

## Servant

Now we come to the interesting section.  How does servant actually uses these things?  Let's go back to the
example code at the top of this blog post:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
    ( (:>), (:<|>)((:<|>)), Get, JSON, Proxy(..), ServantErr, ServerT, serve )

type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]

app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

cats :: EitherT ServantErr IO [String]
cats = return ["long-haired", "short-haired"]

main :: IO ()
main = run 32323 $ logStdoutDev app
```

The two interesting functions are `serve` and `myAPI`.  [`serve`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#v:serve) is provided by
[servant-server](https://hackage.haskell.org/package/servant-server),
while `myAPI` is written by us.

Let's look at the type of serve:

```haskell
ghci> import Servant.Server
ghci> :type serve
serve :: HasServer layout => Proxy layout
                          -> Server layout
                          -> Network.Wai.Application 
```

Let's start with the easy things.  It returns a
[`Network.Wai.Application`](https://hackage.haskell.org/package/wai-3.0.3.0/docs/Network-Wai.html#t:Application).
This represents an application that can that can be served by Warp (i.e.
something that can be passed to the [`run`](https://hackage.haskell.org/package/warp-3.0.13.1/docs/Network-Wai-Handler-Warp.html#v:run) function provided by Warp).

The first argument is `Proxy layout`.  The `serve` function uses this to figure
out what the API type is.  You might be asking, "*If we are also passing the
`layout` type variable to the
[`Server`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#t:Server)
type constructor, why do we additionally need to pass a `Proxy layout`?
Surely, we don't need to pass `layout` twice?*".  That
will be covered later.

(If you don't understand this, look at the type of the `serve` function again:

```haskell
serve :: HasServer layout => Proxy layout
                          -> Server layout
                          -> Network.Wai.Application 
```

`layout` is specified twice, when it should only have to be specified once,
right?)

Now look at the second argument, `Server layout`.  What is `Server`?

```haskell
ghci> :info Server
type Server layout = ServerT layout (EitherT ServantErr IO)
```

`Server` looks like it is a specialization of
[ServerT](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#t:ServerT)
around the
[`EitherT`](https://hackage.haskell.org/package/either-4.4.1/docs/Control-Monad-Trans-Either.html#t:EitherT)
monad transformer.  This similar to how the
[`Reader`](https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Reader.html#t:Reader)
monad is a specialization of the
[`ReaderT`](https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Reader.html#t:ReaderT)
monad:

```haskell
newtype ReaderT r m a = ...
type Reader r a = ReaderT r Identity a
```

Okay, so `Server` is just a specialization of `ServerT`.  Then what is `ServerT`?

```haskell
ghci> :info! ServerT
class HasServer (layout :: k) where
  type family ServerT (layout :: k) (m :: * -> *) :: *
...
```

...a type family!  This is what we've been waiting for!  `ServerT` is a type
family.  It's a function that computes a type.  Let's take a look at the
[`HasServer`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#t:HasServer)
typeclass before really diving into `ServerT`.

```haskell
class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (ServerT layout (EitherT ServantErr IO))) -> Router
```

`HasServer` takes one type parameter, `layout`.  `ServerT` is
a type family that takes two parameters, `layout` and `m`.

There is one function in this typeclass,
[`route`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#v:route).
It takes a `Proxy layout` and an `IO` of a `RouteResult` of a `ServerT` with
the `m` parameter specialized to `EitherT ServantErr IO`.  Quite a mouthful.
Let's abbreviate part of the type to make it easier to digest:

```haskell
route :: Proxy layout -> IO (RouteResult (ServerT ...)) -> Router
```

Basically route takes an `IO` of a `RouteResult` of a `ServerT` and returns a `Router`.
Let's go back real quick and look at the implementation of the `serve` function:

```haskell
serve :: HasServer layout => Proxy layout -> ServerT layout (EitherT ServantErr IO) -> Application
serve proxy server = toApplication (runRouter (route proxy (return (RR (Right server)))))
```

First off, the type of the `serve` function looks pretty similar to the `route` function:

```haskell
serve :: HasServer layout => Proxy layout ->                 (ServerT ...)  -> Application
route ::                     Proxy layout -> IO (RouteResult (ServerT ...)) -> Router
```

So how does the `serve` function work?  It's basically taking our `myAPI` (the `server` argument below)
argument (which is of type `ServerT`), wrapping it in a base `RouteResult` and `IO`, then passing it to the
`route` function.

```haskell
serve :: HasServer layout => Proxy layout -> (ServerT ...) -> Application
serve proxy server = toApplication (runRouter (route proxy (return (RR (Right server)))))
                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                                           look at all this wrapping!!!
```

It takes the resulting `Router` from the `route` function, passes it to
`runRouter`, and then passes that to
[`toApplication`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#v:toApplication)
to get the Wai application.  Pretty easy!  Let's see it point free!

```haskell
serve :: HasServer layout => Proxy layout -> (ServerT ...) -> Application
serve proxy = toApplication . runRouter . route proxy . return . RR . Right
                                                        ^^^^^^^^^^^^^^^^^^^
                                                  look at this pointfree wrapping!!!
```

Understanding `serve` isn't strictly necessary to understanding the rest of
this article, but it is interesting.

## `HasServer`, one more time

Let's go back to the `HasServer` typeclass.  Here it is again:

```haskell
class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (ServerT layout (EitherT ServantErr IO))) -> Router
```

This typeclass specifies things for which `Router`s can be created.  These
`Router`s can then be turned into a Wai application.

So what instances are available for the `HasServer` typeclass?  Let's ask ghci.

```haskell
ghci> :info! HasServer
...
instance AllCTRender ctypes a => HasServer (Get ctypes a)
...
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout)
instance (HasServer a, HasServer b) => HasServer (a :<|> b)
ghci>
```

there are instances defined for
[`Get`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L230),
[`(:>)`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L715),
[`(:<|>)`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L79).
I know where we've seen those before!  The `MyAPI` type!

Let's take a look at the `MyAPI` type defined earlier in the example code:

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

Remember how type-level operators can be rewritten to prefix form?  Well, rewriting `(:<|>)` to prefix form becomes this:

```haskell
type MyAPI = (:<|>) ("dogs" :> Get '[JSON] [Int]) ("cats" :> Get '[JSON] [String])
```

The inner `(:>)` could also be rewritten to prefix form and it will get *even uglier*:

```haskell
type MyAPI = (:<|>) ((:>) "dogs" (Get '[JSON] [Int])) ((:>) "cats" (Get '[JSON] [String]))
```

Okay, so here's where the explanation starts to get a little difficult.  Remember the `app` function?

```haskell
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

cats :: EitherT ServantErr IO [Int]
cats = return ["long-haired", "short-haired"]
```

It's basically just calling serve and passing it two things.

* a `Proxy` with the `MyAPI` type.

* the `myAPI` function, which is the actual implementation of the API.

You remember what `serve` does, right?

```haskell
serve :: HasServer layout => Proxy layout -> ServerT layout (EitherT ServantErr IO) -> Application
serve proxy server = toApplication (runRouter (route proxy (return (RR (Right server)))))
```

It basically calls `route` with our proxy and the implementation of our api.

Now for the interesting part.  Since `HasServer` is a typeclass, what `route`
function actually gets called?  If we look at the `HasServer` typeclass once
again, we can see that it depends on the type of `layout` (which gets pass to
`route` as `Proxy layout`).

```haskell
class HasServer <b>layout</b> where
  type ServerT <b>layout</b> (m :: * -> *) :: *

  route :: <b>Proxy layout</b> -> IO (RouteResult (ServerT layout ...)) -> Router
```

layout originally comes from our app function.

```haskell
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI
```

Here it's MyAPI.  What's the prefix form of MyAPI?

```haskell
type MyAPI = (:<|>) ((:>) "dogs" (Get '[JSON] [Int])) ((:>) "cats" (Get '[JSON] [String]))
```

Okay, great!  So we need the HasServer instance for (:<|>)!  What does that look like?

```haskell
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy server = choice (route pa (extractL <$> server))
                              (route pb (extractR <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
```

So what's going on here?  Well, first you notice that the value the ServerT
type family becomes ServerT a m :<|> ServerT b m.

```haskell
type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m
```

So what's the significance of this?  Two things.  One, we can figure out the specialized type of route:

```haskell
route :: Proxy layout -> IO (RouteResult (ServerT layout ...)) -> Router
-- becomes
route :: Proxy layout -> IO (RouteResult (<b>ServerT a ... :<|> ServerT b ...</b>)) -> Router
```

And two, we can change the type of our myAPI function to this, and our example
program will still compile.  Before, we had this:

```haskell
myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

But we could change it to this:

```haskell
myAPI :: ServerT ("dogs" :> Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT ("cats" :> Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

It still compiles!  That's great!

## One Level Deeper

Going back to the HasServer instance for (:<|>), we see that the route function
basically calls itself recursively on both arguments to (:<|>).  So, which
route function will be called?

```haskell
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy server = choice (<b>route pa (extractL <$> server)</b>)
                              (<b>route pb (extractR <$> server)</b>)
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
```

Lets take a look at the first argument to (:<|>).

```haskell
type MyAPI = <b>"dogs" :> Get '[JSON] [Int]</b>
        :<|> "cats" :> Get '[JSON] [String]

myAPI :: <b>ServerT ("dogs" :> Get '[JSON] [Int]) (EitherT ServantErr IO)</b>
    :<|> ServerT ("cats" :> Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = <b>dogNums</b> :<|> cats

<b>dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]</b>
```

You can probaby see where this is going.  The route function for the (:>)
instance of HasServer will be called.  This corresponds to the ("dogs" :> Get
'[JSON] [Int]) portion of MyAPI:

```haskell
type MyAPI = <b>"dogs" :> Get '[JSON] [Int]</b>
        :<|> "cats" :> Get '[JSON] [String]
```

Here is the HasServer instance for (:>):

```haskell
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where

  type ServerT (path :> sublayout) m = ServerT sublayout m

  route Proxy subserver = StaticRouter $
    M.singleton (cs (symbolVal proxyPath))
                (route (Proxy :: Proxy sublayout) subserver)
    where proxyPath = Proxy :: Proxy path
```

Similarly to (:<|>), you can see that the value of the ServerT type family
becomes ServerT sublayout m.  The path portion is basically ignored.

Just like above, we can change the type of myAPI to match this.  After our last
change, we had this:

```haskell
myAPI :: ServerT ("dogs" :> Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT ("cats" :> Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

Because the path portion is ignored, we can change it to this:

```haskell
myAPI :: ServerT (Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT (Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

Still compiles!  Great!

If path is ignored in the type family, what is it actually used for?

symbolVal is called to get the value of path at the value level!  It's using the value of path to do the routing.

```haskell
route Proxy subserver = StaticRouter $
    M.singleton (cs (<b>symbolVal proxyPath</b>))
                (route (Proxy :: Proxy sublayout) subserver)
  where proxyPath = Proxy :: Proxy path
```

route is then called recursively on the subsever (which has type sublayout).

```haskell
route Proxy subserver = StaticRouter $
    M.singleton (cs (symbolVal proxyPath))
                (<b>route (Proxy :: Proxy sublayout) subserver</b>)
  where proxyPath = Proxy :: Proxy path
```

In this case, subserver will be our dogNums function, and the sublayout type
will be Get '[JSON] [Int].

```haskell
type MyAPI = "dogs" :> <b>Get '[JSON] [Int]</b>
        :<|> "cats" :> Get '[JSON] [String]

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]
```

## Red Pill, Blue Pill, Bottom of the Rabit Hole

What route function will be called in this case?  The one defined for the Get
instance of HasServer!

```haskell
instance ( AllCTRender ctypes a ) => HasServer (Get ctypes a) where
  type ServerT (Get ctypes a) m = m a

  route Proxy = methodRouter methodGet (Proxy :: Proxy ctypes) ok200
```

You can see that the ServerT type family becomes m a.  For us, m is EitherT
ServantErr IO, and a is [Int].  So it becomes EitherT ServantErr IO [Int].
That's why dogNums type is EitherT ServantErr IO [Int].

```haskell
dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]
```

Just like we did above, we can manually rewrite the type of `myAPI` and it will
still compile:

```haskell
myAPI :: EitherT ServantErr IO [Int]
    :<|> EitherT ServantErr IO [String]
myAPI = dogNums :<|> cats
```

We won't go into how the route function is implemented here, but you are
welcome to look at the implementation of
[methodRouter](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L123)
if you're interested.

## Conclusion

At a very high-level, the HasServer typeclass, ServerT type family, and route function are used to peal away levels of MyAPI:

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

First, (:<|>) is pealed away and we are left with "dogs" :> Get '[JSON] [Int].
Then (:>) is pealed away and we are left with Get '[JSON] [Int].  This gets
turned into the actual type of the function we will be calling (dogNums).

```haskell
dogNums :: <b>EitherT ServantErr IO [Int]</b>
dogNums = return [1,2,3,4]
```

If you liked this tutorial, you may also like the servant tutorial itself
(http://haskell-servant.github.io/tutorial/), or a tutorial about using servant
with persistent
(http://www.parsonsmatt.org/programming/2015/06/07/servant-persistent.html) by
Matt Parsons (http://www.parsonsmatt.org/).
