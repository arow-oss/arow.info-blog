---
title: ARoW.info Blog -- Servant Intro
headingBackgroundImage: ../img/post-bg.jpg
headingDivClass: post-heading
heading: Here is a title
subHeading: Problems look mighty small from 150 miles up
postedBy: Dennis Gosnell
---

Servant is a really nice library for building REST APIs in Haskell.  However,
it uses advanced GHC features which may not be familiar to some Haskell
programmers.  In this article, I explain type-level strings, type-level lists,
type-level operators, and type families.  Finally, I use code from
servant-server to explain how these features are used in practice.

Servant Example
---------------

Here is a simple example of using servant-server.  We will refer to this code
throughout this article.

```
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

This example project can be found at the gist
https://gist.github.com/cdepillabout/c2b8b1807e1f571fdb45#file-example-hs

The following steps can be used to download and run the code.  The stack build
tool is used.

```
$ git clone https://gist.github.com/c2b8b1807e1f571fdb45.git
$ mv c2b8b1807e1f571fdb45 small-servant-example
$ cd small-servant-example
$ stack build
$ stack exec servant-notes
```

This runs a Warp server on port 32323.  With the server running, you can use
curl to test the API.

```
$ curl http://localhost:32323/dogs
[1,2,3,4]
$ curl http://localhost:32323/cats
["long-haired","short-haired"]
$
```

You can also open the code in ghci.

```
$ stack ghci
ghci> :load example.hs
ghci> :info app
app :: Application      -- Defined at example.hs:17:1
ghci>
```

Type-Level Strings
------------------

Recent versions of GHC support [type-level
strings](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/type-level-literals.html).
What's a type-level string?  Well, let's play around with it in the REPL.

First, we need to enable the DataKinds language extension.

```
ghci> :set -XDataKinds
ghci>
```

Now, let's try to get the kind of a type-level string:

```
ghci> :kind "hello"
"hello" :: GHC.TypeLits.Symbol
ghci>
```

Hmm, our type-level string appears to be of kind
[GHC.TypeLits.Symbol](https://hackage.haskell.org/package/base-4.8.0.0/docs/GHC-TypeLits.html#t:Symbol).
So what can we do with this?

Looking at the
[GHC.TypeLits](https://hackage.haskell.org/package/base-4.8.0.0/docs/GHC-TypeLits.html)
module, there appears to be a
[symbolVal](https://hackage.haskell.org/package/base-4.8.0.0/docs/GHC-TypeLits.html#v:symbolVal)
function. We can use it to get back the <i>value</i> of our type-level string.

Let's take a look at this in ghci.  First we need to import
GHC.TypeLits.symbolVal, and then import Data.Proxy.Proxy, We will be using
Proxy to "proxy" the type-level literal.

```
ghci> import GHC.TypeLits
ghci> import Data.Proxy
ghci> symbolVal (Proxy :: Proxy "hello")
"hello"
ghci>
```

This is really cool!  We are able to get back a <i>concrete value</i> of
something that only exists on the <i>type-level</i>!

How does servant use this?  Take a look at the MyAPI type we defined near the
top of this article:

```
type MyAPI = <b>"dogs"</b> :> Get '[JSON] [Int]
        :<|> <b>"cats"</b> :> Get '[JSON] [String]
```

"dogs" and "cats" are type-level strings.  At the end of this article we will
look at some servant-server code and confirm that it is using symbolVal to get
the value of the type-level strings.

Type-Level Lists
----------------

Just like we can have type-level strings, we can also have type-level lists.

We need to enable the DataKinds language extension.

```
ghci> :set -XDataKinds
ghci>
```

Let's look at the kind of a type-level empty list:

```
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
There is a short example of a heterogeneous list (or HList).  A heterogeneous
list is a list that has elements of different types.  In the example, you can
see that foo2 represents a heterogeneous list with two elements, Int and Bool.

Looking at the example, we see that we can define type-level lists by putting a
quote in front of the opening bracket in the list:

```
ghci> :kind! '[]
'[] :: [k]
ghci>
```

We can also define type-level lists with multiple elements:

```
ghci> :kind! '[Int, Bool, String]
'[Int, Bool, String] :: [*]
ghci>
```

Going back to the MyAPI example from above, you can see that servant is using
type-level lists to represent the available content-type encodings of the
response.

```
type MyAPI = "dogs" :> Get <b>'[JSON]</b> [Int]
        :<|> "cats" :> Get <b>'[JSON]</b> [String]
```

Servant is only willing to send back responses in JSON.

Additional content types could also be specified:

```
type MyAPI = "dogs" :> Get <b>'[JSON, FormUrlEncoded]</b> [Int]
        :<|> "cats" :> Get <b>'[JSON, PlainText]</b> Text
```

(However, to get this to compile, we would need to have an instance of
ToFormUrlEncoded [Int].)

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


Type-Level Operators
--------------------

In our servant example code above, there are two type-level operators being
used: (:>) and (:<|>).  Type-level operators are similar to normal data types.
They are just composed of symbols instead of letters.

Let's look at how (:>) and (:<|>) are defined in servant:

```
data path :> a

data a :<|> b = a :<|> b
```

If we didn't want to write them infix, we could write them like this:

```
data (:>) path a

data (:<|>) a b = (:<|>) a b
```

In fact, if we were to write these data types with letters instead of symbols,
it would look something like this:

```
data Foo path a

data Bar a b = Bar a b
```

You can see that (:>) and (:<|>) are just normal datatype definitions. They
look weird because they are made of symbols and written infix.

Type operators help us write long type definitions that are easy to understand.
Take the following API definition:

```
type MyAPI = "foo" :> "bar" >: Get '[JSON] [Int]
```

Rewriting this prefix would look like this:

```
type MyAPI = (:>) "foo" ((>:) "bar" (Get '[JSON] [Int]))
```

You can see how much easier the infix style is to read!

NOTE:  You need the TypeOperators language extension enabled to use the above code.

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/data-type-extensions.html#type-operators

You may be thinking, "These type operators are pretty neat, but how are they
actually used?  They just look like confusing data types!"  Well, we'll get to
that in a minute.  First  we need to take a look at type families.

Type Families
-------------

Type families are a relatively simple addition to Haskell that allow the user
to do some computation at the type level.  However, if you google for [type
families](https://www.google.co.jp/search?q=haskell+type+families&ie=utf-8&oe=utf-8&gws_rd=cr&ei=OzuSVZSMA6S-mAX044aoCQ),
it's easy to get scared.

The first result is the GHC/Type families article on the Haskell wiki
(https://wiki.haskell.org/GHC/Type_families).  This is written with an advanced
Haskeller in mind.  Don't worry if it's too hard.  (The other problem is that
most of their examples use data families instead of type families--which I
introduce below.  Most of the realworld Haskell code I've seen uses type
families much more than data families).

The second link is to the GHC manual.  It's good if you already know about type
families and just want a refresher, but it's not good as an introduction to
type families.
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-families.html

The third result is
https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/type-families-and-pokemon
an article on fpcomplete.  It gets points for being about Pokemon, but the
setup/motivation for using type families is way too long.

The fourth result is an introduction to type families from O'Charles
(https://ocharles.org.uk/blog/posts/2014-12-12-type-families.html).  It's the
best of the bunch, but it is slightly hard to follow if you've never used
MVars, IORefs, etc.

I wrote a super simple tl;dr [presentation about type
families](https://cdepillabout.github.io/haskell-type-families-presentation).
Originally I wrote it in Japanese for a Haskell Lightening Talk in Tokyo, but I
recently translated it to English upon the request from someone in the #haskell
room in the functional programming slack community (http://fpchat.com/).  If
you aren't sure about type families, please read that presentation and then
proceed to the next section.  We will be discussing the ServantT type familiy.

Servant
-------

So now we come to how servant actually uses these things.  Let's go back to the
example code at the top of this blog post:

```
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

The two interesting functions are serve and myAPI.  serve is provided by
servant-server, while myAPI is written by us.

Let's look at the type of serve:

```
ghci> import Servant.Server
ghci> :type serve
serve :: HasServer layout => Proxy layout
                          -> Server layout
                          -> Network.Wai.Application 
```

Let's start with the easy things.  You can see that it returns a
'Network.Wai.Application'.  This represents an application that can that can be
served by Warp (i.e. something that can be passed to the 'run' function
provided by Warp).

The first argument is Proxy layout.  This is how we tell the serve function
what our API type is.  You might be asking, "If we are also passing the layout
type variable to the Server type constructor, why do we additionally need to
pass a Proxy layout?  Surely, we don't need to pass it twice?".  That will be
covered later.

(If you don't understand this, look at the type of the serve function again:

```
serve :: HasServer layout => Proxy <b>layout</b>
                          -> Server </b>layout</b>
                          -> Network.Wai.Application 
```

layout is specified twice, when it should only have to be specified once,
right?)

Now look at the second argument, Server layout.  What is Server?

```
ghci> :info Server
type Server layout =
    ServerT layout (EitherT ServantErr IO)
```

Server looks like it is a specialization of ServerT around the EitherT monad
transformer.  This similar to how the Reader monad is a specialization of the
ReaderT monad:

```
newtype ReaderT r m a = ...
type Reader r a = ReaderT r Identity a
```

Okay, so Server is just a specialization of ServerT.  So then what is ServerT?

```
ghci> :info! Server
class HasServer (layout :: k) where                                                                                                              type family ServerT (layout :: k) (m :: * -> *) :: *
...
```

...a type family!  This is what we've been waiting for!  ServerT is a type
family.  It's a function that computes a type.  Let's take a look at the
HasServer typeclass before really diving into ServerT.

```
class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (ServerT layout (EitherT ServantErr IO))) -> Router
```

We see that HasServer takes one parameter, layout.  We also see that ServerT is
a typeclass that takes two parameters, layout and m.  There is one function in
this typeclass, route.  It takes a Proxy layout and an IO of a RouteResult of a
ServerT with the m parameter specialized to EitherT ServantErr IO.  Quite a
mouthful.  Let's abbreviate part of the type to make it easier to diget:

```
route :: Proxy layout -> IO (RouteResult (ServerT ...)) -> Router
```

Basically route takes an IO of a RouteResult of a ServerT and returns a Router.
Let's go back real quick and look at the implementation of the serve function:

```
serve :: HasServer layout => Proxy layout -> ServerT layout (EitherT ServantErr IO) -> Application
serve p server = toApplication (runRouter (route p (return (RR (Right server)))))
```

First off, the type of the function looks pretty similar to the route funtion:

```
serve :: HasServer layout => Proxy layout ->                 (ServerT ...)  -> Application
route ::                     Proxy layout -> IO (RouteResult (ServerT ...)) -> Router
```

So how does the serve function work?  It's basically taking our server
argument, wrapping it in a default RouteResult and IO, then passing it to the
route function.

```
serve :: HasServer layout => Proxy layout -> (ServerT ...) -> Application
serve p server = toApplication (runRouter (route p (return (RR (Right server)))))
                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                                   look at all this wrapping!!!
```

It takes the resulting Router from the route function, passes it to runRouter,
and then passes that to toApplication to get our Wai application.  Pretty easy!


HasServer, one more time
------------------------

Let's go back to the HasServer typeclass.  Here it is again:

```
class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (ServerT layout (EitherT ServantErr IO))) -> Router
```

This typeclass basically specifies things that we can create Router's for.  We
can then turn these routers into a Wai application.

So what instances are available for the HasServer typeclass?  Let's ask ghci.

```
ghci> :info! HasServer
...
instance AllCTRender ctypes a => HasServer (Get ctypes a) 
...
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout)
instance (HasServer a, HasServer b) => HasServer (a :<|> b)
ghci>
```

It looks like there are instances for Get, (:>), (:<|>).  I know where we've seen those before!  The MyAPI type!

Let's take a look at the MyAPI type we defined earlier as our example code:

```
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

Remember how we can rewrite type-level operators to prefix form?  Well, if we do that for (:<|>), our MyAPI will look like this:

```
type MyAPI = (:<|>) ("dogs" :> Get '[JSON] [Int]) ("cats" :> Get '[JSON] [String])
```

We could do it again for (:>) and it will get even uglier:

```
type MyAPI = (:<|>) ((:>) "dogs" (Get '[JSON] [Int])) ((:>) "cats" (Get '[JSON] [String]))
```

Okay, so here's where the explanation starts to get a little difficult.  Remember our app function?

```
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

cats :: EitherT ServantErr IO [Int]
cats = return ["long-haired", "short-haired"]
```

It's basically just calling serve and passing it two things.  1) a Proxy with
the MyAPI type.  2) the myAPI function, which is the actual implementation of
our API.  You remember what serve does, right?

```
serve :: HasServer layout => Proxy layout -> ServerT layout (EitherT ServantErr IO) -> Application
serve p server = toApplication (runRouter (route p (return (RR (Right server)))))
```

It basically just calls route with our proxy and the implementation of our api.

Now for the interesting part.  Since HasServer is a typeclass, what route
function actually gets called?  If we look at the HasServer typeclass once
again, we can see that it depends on the type of layout (which gets pass to
route as Proxy layout).

```
class HasServer <b>layout</b> where
  type ServerT <b>layout</b> (m :: * -> *) :: *

  route :: <b>Proxy layout</b> -> IO (RouteResult (ServerT layout ...)) -> Router
```

layout originally comes from our app function.

```
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI
```

Here it's MyAPI.  What's the prefix form of MyAPI?

```
type MyAPI = (:<|>) ((:>) "dogs" (Get '[JSON] [Int])) ((:>) "cats" (Get '[JSON] [String]))
```

Okay, great!  So we need the HasServer instance for (:<|>)!  What does that look like?

```
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy server = choice (route pa (extractL <$> server))
                              (route pb (extractR <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
```

So what's going on here?  Well, first you notice that the value the ServerT
type family becomes ServerT a m :<|> ServerT b m.

```
type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m
```

So what's the significance of this?  Two things.  One, we can figure out the specialized type of route:

```
route :: Proxy layout -> IO (RouteResult (ServerT layout ...)) -> Router
-- becomes
route :: Proxy layout -> IO (RouteResult (<b>ServerT a ... :<|> ServerT b ...</b>)) -> Router
```

And two, we can change the type of our myAPI function to this, and our example
program will still compile.  Before, we had this:

```
myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

But we could change it to this:

```
myAPI :: ServerT ("dogs" :> Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT ("cats" :> Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

It still compiles!  That's great!

One Level Deeper
----------------

Going back to the HasServer instance for (:<|>), we see that the route function
basically calls itself recursively on both arguments to (:<|>).  So, which
route function will be called?

```
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy server = choice (<b>route pa (extractL <$> server)</b>)
                              (<b>route pb (extractR <$> server)</b>)
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
```

Lets take a look at the first argument to (:<|>).

```
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

```
type MyAPI = <b>"dogs" :> Get '[JSON] [Int]</b>
        :<|> "cats" :> Get '[JSON] [String]
```

Here is the HasServer instance for (:>):

```
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

```
myAPI :: ServerT ("dogs" :> Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT ("cats" :> Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

Because the path portion is ignored, we can change it to this:

```
myAPI :: ServerT (Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT (Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

Still compiles!  Great!

If path is ignored in the type family, what is it actually used for?

symbolVal is called to get the value of path at the value level!  It's using the value of path to do the routing.

```
route Proxy subserver = StaticRouter $
    M.singleton (cs (<b>symbolVal proxyPath</b>))
                (route (Proxy :: Proxy sublayout) subserver)
  where proxyPath = Proxy :: Proxy path
```

route is then called recursively on the subsever (which has type sublayout).

```
route Proxy subserver = StaticRouter $
    M.singleton (cs (symbolVal proxyPath))
                (<b>route (Proxy :: Proxy sublayout) subserver</b>)
  where proxyPath = Proxy :: Proxy path
```

In this case, subserver will be our dogNums function, and the sublayout type
will be Get '[JSON] [Int].

```
type MyAPI = "dogs" :> <b>Get '[JSON] [Int]</b>
        :<|> "cats" :> Get '[JSON] [String]

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]
```

Red Pill, Blue Pill, Bottom of the Rabit Hole
---------------------------------------------

What route function will be called in this case?  The one defined for the Get
instance of HasServer!

```
instance ( AllCTRender ctypes a ) => HasServer (Get ctypes a) where
  type ServerT (Get ctypes a) m = m a

  route Proxy = methodRouter methodGet (Proxy :: Proxy ctypes) ok200
```

You can see that the ServerT type family becomes m a.  For us, m is EitherT
ServantErr IO, and a is [Int].  So it becomes EitherT ServantErr IO [Int].
That's why dogNums type is EitherT ServantErr IO [Int].

```
dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]
```

Just like we did above, we can manually rewrite the type of myAPI and it will
still compile:

```
myAPI :: EitherT ServantErr IO [Int]
    :<|> EitherT ServantErr IO [String]
myAPI = dogNums :<|> cats
```

We won't go into how the route function is implemented here, but you are
welcome to look at the implementation of
[methodRouter](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L123)
if you're interested.

Conclusion
----------

At a very high-level, the HasServer typeclass, ServerT type family, and route function are used to peal away levels of MyAPI:

```
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

First, (:<|>) is pealed away and we are left with "dogs" :> Get '[JSON] [Int].
Then (:>) is pealed away and we are left with Get '[JSON] [Int].  This gets
turned into the actual type of the function we will be calling (dogNums).

```
dogNums :: <b>EitherT ServantErr IO [Int]</b>
dogNums = return [1,2,3,4]
```

If you liked this tutorial, you may also like the servant tutorial itself
(http://haskell-servant.github.io/tutorial/), or a tutorial about using servant
with persistent
(http://www.parsonsmatt.org/programming/2015/06/07/servant-persistent.html) by
Matt Parsons (http://www.parsonsmatt.org/).
