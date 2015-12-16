---
title: ARoW.info Blog -- PureScript for the Haskeller
draft: yes
headingBackgroundImage: ../img/post-bg.jpg
headingDivClass: post-heading
heading: PureScript for the Haskeller
subHeading: Where to get started in PureScript for the Haskell programmer
postedBy: <a href="http://functor.tokyo">Dennis Gosnell</a>
---

This article is an introduction to PureScript for someone that knows Haskell
(but may not be familiar with JavaScript).  PureScript is very similar to
Haskell.  If you know Haskell, PureScript will be very easy to learn.

This article first gives an overview of the differences between PureScript and
Haskell, and then dives into things specific to PureScript like the build
systems and DOM manipulation libraries.

## Differences Between Haskell and PureScript

There are only a few key differences between Haskell and PureScript.  Here is a
[comprehensive
list](https://github.com/purescript/purescript/wiki/Differences-from-Haskell).
You should read this first.

Here are a couple things that often came up in the code I was writing:

- [`forall`](https://github.com/purescript/purescript/wiki/Differences-from-Haskell#explicit-forall)
  is explicit in PureScript.  This is somewhat annoying, but easy to get used
  to.  In some future version of PureScript, [it might  not be
  required](https://github.com/purescript/purescript/issues/766).
- Instances are
  [named](https://github.com/purescript/purescript/wiki/Differences-from-Haskell#named-instances).
- Orphan instances are [completely
  disallowed](https://github.com/purescript/purescript/wiki/Differences-from-Haskell#orphan-instances).
- No functional dependencies or type families.  This makes type checking in
  PureScript worse than Haskell in [some
  cases](https://github.com/purescript/purescript-transformers/issues/64).
- There is no [special
  syntax](https://github.com/purescript/purescript/wiki/Differences-from-Haskell#tuples)
  for tuples.
- The composition operator is not `(.)`, but `(<<<)`.
- `undefined` is not provided in the `Prelude`, but it can be
  [emulated](https://github.com/purescript/purescript/wiki/Differences-from-Haskell#error-and-undefined).
- PureScript's `Prelude` is somewhat different from Haskell's.
    - It has to be imported in every module that uses it.
    - It doesn't export things like
      [`Maybe`](https://github.com/purescript/purescript-maybe) and
      [`Either`](https://github.com/purescript/purescript-either).  They live
      in separate modules.
    - The typeclass hierarchy is much more find-grained ([see below](#typeclass-hierarchy)).
    - Alternative Preludes that export many modules are more common in
      Purescript.  See the
      [`purescript-batteries`](https://github.com/tfausak/purescript-batteries)
      package, for example.
- PureScript uses extensible records ([see below](#extensible-records)).
- PureScript doesn't use a `IO` monad, but instead extensible effects with the
  `Eff` Monad ([see below](#eff-monad)).

### Typeclass Hierarchy

The typeclass hierarchy provided by PureScript's `Prelude` is much more
fine-grained than Haskell.  It is worth reading through PureScript's
[`Prelude`](https://github.com/purescript/purescript-prelude/blob/master/src/Prelude.purs)
to see how it is split up.  It is not very long.

A lot of classes and types provided by Haskell's `base` package are separate
packages in PureScript.  This includes
[`Maybe`](https://github.com/purescript/purescript-maybe),
[`Either`](https://github.com/purescript/purescript-either),
[`Foldable`](https://github.com/purescript/purescript-foldable-traversable),
[`Traversable`](https://github.com/purescript/purescript-foldable-traversable),
[`Identity`](https://github.com/purescript/purescript-identity),
[`List`](https://github.com/purescript/purescript-lists),
[`Monoid`](https://github.com/purescript/purescript-monoid/blob/master/docs/Data/Monoid.md),
etc.  When looking for which package contains a class or type you are used to
from Haskell, you may want to use Pursuit ([see below](#pursuit)).

There is also a list of [recommended PureScript
libraries](https://github.com/purescript/purescript/wiki/Recommended-Libraries)
on the PureScript wiki.  This is a good resource to use to find common
packages.

### Extensible Records

Records in PureScript don't work like records in Haskell.  PureScript records
are introduced nicely in [Chapter
3](https://leanpub.com/purescript/read#leanpub-auto-functions-and-records) of
the PureScript book.

PureScript has extensible records (also called "row polymorphism").  A short
introduction is given in [Chapter
5.7](https://leanpub.com/purescript/read#leanpub-auto-record-patterns-and-row-polymorphism)
of the PureScript book.  Make sure you understand extensible records before
reading about the `Eff` monad.

One neat thing about PureScript records is that they can be used in newtypes.
The following code in Haskell will fail to compile:

```haskell
newtype Foo = Foo { a :: String, b :: Int }
```

You'll get the following error message:

```
foo.hs:1:15:
    The constructor of a newtype must have exactly one field
      but ‘Foo’ has two
    In the definition of data constructor ‘Foo’
    In the newtype declaration for ‘Foo’
```

However, the same code in PureScript is completely fine!

```purescript
newtype Foo = Foo { a :: String, b :: Int }
```

When looking at the type of the `Foo` constructor in `psci`, we can see that it
takes a record as an argument:

```
> :t Foo
{ a :: String, b :: Int } -> Foo
```

### `Eff` Monad

PureScript doesn't use the `IO` monad.  It uses the `Eff` monad.  There are
two good tutorials on using the `Eff` monad:

- [Handling Native Effects with the Eff
  Monad](http://www.purescript.org/learn/eff/)
- [Chapter 8](https://leanpub.com/purescript/read#leanpub-auto-the-eff-monad)
  of the PureScript book

Since callbacks are used so heavily in JavaScript, PureScript code frequently
uses the [`Aff`](https://github.com/slamdata/purescript-aff) monad. The `Aff`
monad is an asynchronous effect monad.  It may be worthwhile to check it
out.

### FFI and JavaScript

You can get pretty far in PureScript without ever needing to drop down to
JavaScript.  But calling out to JavaScript is sometimes necessary.  Luckily,
PureScript has a very nice JavaScript FFI.  Here are two good tutorials on
PureScript's FFI:

- [The Foreign Function Interface](http://www.purescript.org/learn/ffi/)
- [Chapter
  10](https://leanpub.com/purescript/read#leanpub-auto-the-foreign-function-interface)
  of the PureScript book

## Pursuit

[Pursuit](http://pursuit.purescript.org/) is like a combination of Hackage and
Hoogle/Hayoo.  It should be your first stop when looking for a
function/type/class that you are used to from Haskell.  Things in PureScript
are often named the same as their counterparts in Haskell.

## Compiler and Build Tools

The PureScript/JavaScript ecosystem is much more complex than the Haskell
ecosystem.  In Haskell, when building a project, the normal advice is to "just
use `stack`"[^1].  In PureScript, you have to deal with `node`, `npm`, `bower`,
`pulp`, and `gulp`.  It may be second-nature for someone familiar with
JavaScript, but it can be intimidating to a Haskeller.

The following sections explain the relationship between Node.js, npm, Bower,
Pulp, Gulp, and Grunt.

I have created two example projects that you can use to play around with the
following tools.  One [project]() is using Node.js, npm, Bower, and Pulp, and
the other [project]() is using Node.js, npm, Bower, and Gulp.  You can use
either project to follow long.

### Node.js

[Node.js](https://nodejs.org) is a runtime environment for developing
server-side web applications.  It lets you run JavaScript code on a server,
instead of just in a browser.

Node.js has to be installed to be able to use PureScript.  It is recommended to
install Node.js with your [distribution's package
manager](https://nodejs.org/en/download/package-manager/).

### npm

[npm](https://www.npmjs.com/) is a package manager for JavaScript and Node.js.
You can find the package repository [here](https://www.npmjs.com).

npm is used to install other build tools like `bower` and `gulp` (discussed
below).  It is recommended to install npm with your [distribution's package
manager](https://nodejs.org/en/download/package-manager/).

The command `npm install` is used to install packages from the npm package
repository.  It creates a directory `node_modules/` in your current working
directory with the installed packages underneath it.

For example, if you run `npm install bower` to install the bower package, the
`bower` executable will be created as `node_modules/bower/bin/bower`.

All executables installed with npm will be available as symbolic links in
`node_modules/.bin/`.  You may want to add this directory to your `PATH`, or
use one of the other techniques specified in [this stackoverflow
answer](http://stackoverflow.com/a/15157360).

One easy way to use npm is to use a `package.json` file.  The `package.json`
file lists the dependencies for the current project.  All dependencies will be
installed if you run `npm install` with no arguments.

Here is an example `package.json` file that lists the PureScript compiler,
Bower, and gulp as dev-time dependencies, and the virtual-dom package as a
runtime dependency:

```javascript
{
  "name": "purescript-foo",
  "version": "1.0.0",
  "description": "example package",
  "author": "Pyour Skrept <ps@example.com>",
  "license": "Apache 2.0",
  "homepage": "https://github.com/example/purescript-foo",
  "dependencies": {
    "virtual-dom": "^2.1.1"
  },
  "devDependencies": {
    "purescript": "^0.7.6",
    "bower": "^1.6.5",
    "gulp": "^3.9.0",
    "gulp-purescript": "^0.7.0"
  },
  "repository": {
    "type": "git",
    "url": "git+ssh://git@github.com/example/purescript-foo.git"
  }
}
```

Check out the `package.json` file in the example project.  Use `npm install` to
install all the tools specified in the `package.json` file.

### PureScript Compiler

The PureScript compiler provides the executables `psc` and `psci`.  These
correspond to `ghc` and `ghci`, respectively.

The PureScript compiler can be installed [multiple
ways](http://www.purescript.org/download/).  Since we are using `npm` to
install `bower`, `gulp`, etc., the easiest way is just to install it with
`npm`:

```sh
$ npm install purescript
```

You can access `psc` and `psci` in `node_modules/.bin/`.

### Bower

Bower is a *separate* package manager from npm.  npm is often used for managing
Node.js modules (for example, build tools, the PureScript compiler, gulp, etc).
Bower is used to manage packages related to the front-end, like PureScript
libraries.  Here is an answer on stackoverflow [explaining the
difference](http://stackoverflow.com/a/18652918) between npm and Bower.

Here is Bower's [package repository](http://bower.io/search/).  If you search
for "purescript", you should find many packages.  PureScript packages are
almost all uploaded to Bower.  Pursuit even
[requires](http://pursuit.purescript.org/help) that new packages are uploaded
to Bower's repository.

Bower can be installed with `npm`:

```sh
$ npm install bower
```

You can access `bower` in `node_modules/.bin/`.

Once you have installed Bower, you can use Bower to install PureScript
packages.  This is how you would install the `purescript-prelude` package:

```sh
$ bower install purescript-prelude
```

`bower` installs packages to the `bower_components/` directory in the current
working directory.  After installing `purescript-prelude` you should be able to
find PureScript source files under `bower_components/purescript-prelude/`.

Just like npm can use a `package.json` file, Bower can use a `bower.json` file
to specify multiple packages.  Here is an example `bower.json` file.  It
specifies that we are using the `purescript-prelude` and `purescript-console`
libraries in the current project:

```javascript
{
  "name": "purescript-foo",
  "version": "1.0.0",
  "moduleType": [ "node" ],
  "ignore": [ "**/.*", "node_modules", "bower_components", "output" ],
  "dependencies": {
    "purescript-console": "^0.1.0",
    "purescript-prelude": "^0.1.0"
  }
}
```

To install all the files listed in `bower.json`, the `install` command is used:

```sh
$ bower install
```

In order to update libraries to newer versions, the `update` command is used:

```sh
$ bower update
```

Check out the `bower.json` file in the example projects.  Try running `bower
install` and `bower update` and see what happens.

### Pulp

Pulp is a build tool specific to PureScript.  It is similar to `stack` or
`cabal` for Haskell.  It works well for libraries and simple projects, but it
does not have enough flexibility for larger projects.  It seems like the
general recommendation from the PureScript community is to use Pulp for simple
libraries, and gulp for any non-trivial application[^2].

Pulp can be installed with `npm`:

```sh
$ npm install pulp
```

You can access `pulp` in `node_modules/.bin/`.

Pulp can be used to build and test your project:

```sh
$ pulp build
...
$ pulp test
...
```

You can find other functions in the Pulp [README](https://github.com/bodil/pulp).

Try running `pulp build` and `pulp test` in the example project for Pulp.  Try
running `pulp server` and opening up the `index.html` file in your browser.

### Gulp

Gulp is a full-featured, dependency-tracking build tool.  It is similar to Make
or [Shake](http://community.haskell.org/~ndm/shake/).

Gulp is used over Pulp
in a couple different cases.  The most common is when building a project that
produces multiple JavaScript files as output.  It is also used when you need to
change build settings that Pulp does not provide access to.

It seems like people using PureScript in production ([SlamData](#slamdata)) use Gulp almost
exclusively.

Gulp can be installed with `npm`:

```sh
$ npm install gulp
```

You can access `gulp` in `node_modules/.bin/`.

Just like `make` uses a `Makefile`, `gulp` uses a `gulpfile.js`.  This file
lists the different targets available, and what should happen when they are
run.

Take a look at the `gulpfile.js` from the example project.  Try running the
`build` and `test` targets:

```sh
$ gulp build
...
$ gulp test
...
```

Try running the `server` target and opening up `site/index.html` in a browser.

```sh
$ gulp server
...
```

### Grunt

Grunt is a build tool similar to Gulp.  It does not seem to be used much in the
PureScript community, but you occasionally see references to it.

If you are interested, here is an
[article](https://medium.com/@preslavrachev/gulp-vs-grunt-why-one-why-the-other-f5d3b398edc4#.p71z2qybh)
describing the differences between Grunt and Gulp.

## DOM Manipulation Libraries

PureScript is mostly used for writing frontend code.  Because of this, there
are many DOM manipulation libraries available.  In this section, I talk about
four different PureScript libraries used for DOM manipulation: simple-dom,
purescript-react, Thermite, and Halogen.

### simple-dom

Like the name implies,
[simple-dom](https://github.com/aktowns/purescript-simple-dom) is the simplest
of the four libraries.  It provides an interface similar to that provided by
web browsers.  It is easy to [create new
nodes](https://github.com/aktowns/purescript-simple-dom/blob/master/docs/Data/DOM/Simple/Document.md#document)
in an HTML Document, [append child
nodes](https://github.com/aktowns/purescript-simple-dom/blob/master/docs/Data/DOM/Simple/Element.md#element)
to an element, etc.  Code written using simple-dom is very similar to code
written using raw JavaScript (although PureScript makes it more type-safe).

A small amount of sample code is available in the project's
[README](https://github.com/aktowns/purescript-simple-dom#some-examples).  It
does not have any type information, so it is somewhat hard to read.  However,
the library is very simple so this shouldn't be much of a problem.

The
[simple-dom](https://github.com/aktowns/purescript-simple-dom/commits/master),
repository hasn't been very active over the past month.  Two (minor) issues I
have created in the past month did not get a response.

simple-dom should be easy to use even for someone just getting started with
languages like PureScript or Haskell.

### purescript-react

[purescript-react](https://github.com/purescript-contrib/purescript-react) is a
set of low-level bindings to [React](https://facebook.github.io/react/) for
PureScript.  React is a library that abstracts updating the UI from an
application.  From the [Why
React?](https://facebook.github.io/react/docs/why-react.html) page:

> Many people choose to think of React as the V in MVC.  We built React to
> solve one problem: building large applications with data that changes over
> time.  Simply express how your app should look at any given point in time,
> and React will automatically manage all UI updates when your underlying data
> changes.

Examples for purescript-react can be found both in the
[README](https://github.com/purescript-contrib/purescript-react) and in the
[tests](https://github.com/purescript-contrib/purescript-react/blob/master/test/Main.purs).

I have not personally used purescript-react, but it looks like there has been
development [within the last
month](https://github.com/purescript-contrib/purescript-react/commits/master).
The author, paf31, seems to respond quickly to issues and pull requests.

purescript-react will appeal to people coming from JavaScript with knowledge of
React.  Like simple-dom, it gives an easy way to make use of your existing
knowledge in a type-safe fashion.

### Thermite

From the Thermite [README](https://github.com/paf31/purescript-thermite):

> purescript-thermite is a simple PureScript wrapper for purescript-react. It
> does not (and does not aim to) provide all of the functionality of ReactJS,
> but instead to provide a clean API to the most commonly-used parts of its
> API.

Examples for Thermite can be found in the
[README](https://github.com/paf31/purescript-thermite#getting-started), as well
a [test
project](https://github.com/paf31/purescript-thermite/blob/master/test).  The
running test project can be viewed
[here](http://functorial.com/purescript-thermite-todomvc/).

I have not personally used Thermite, but it looks like there has been some
acitivity within the last month.  The author, paf31, responds to issuse and
pull requests quickly.

### Halogen

Halogen is quite possibly the most interesting of the DOM manipulation
libraries for PureScript.  It is similar to Thermite but aims to be more
type-safe and composable.  It is based on
[virtual-dom](https://github.com/Matt-Esch/virtual-dom) (a library similar to,
but more low-level, than React).  It feels like a well-typed, easy-to-use
version of Elm.

Halogen makes use of many advanced concepts like free monads, functor
coproducts, etc.  For an intermediate/advanced Haskeller, it provides _very_
nice abstractions for developing web frontends.

Halogen has a [README](https://github.com/slamdata/purescript-halogen) that
explains in detail how to use the library.  There are also 10+ example
projects in the
[examples](https://github.com/slamdata/purescript-halogen/tree/master/examples)
directory.

Halogen is being used heavily by SlamData. Because of this, its development
seems to be progressing faster than any of the other libraries.  The developers
of Halogen are very quick in responding to issues and pull requests.  This
makes working with Halogen a delight.

## PureScript in Industry

In this section I talk about companies using PureScript in industry and users
you are likely to see active on Github.

### Companies

There are not currently many companies using PureScript in production.
SlamData is a notable exception.

If you know any other companies that should be added to this list, please let
me know.

#### [SlamData](http://slamdata.com/)

SlamData is providing an open source, visualization solution for NoSQL data.
PureScript is being used in the frontend for visualization of data.  SlamData
is currently employing three full-time PureScript developers.[^3]

### People

The people below are all very active on Github in a variety of PureScript
projects.  You'll likely run in to them if you submit issues or send pull
requests.

If you know of anyone that should be added to this list, please let me know.

#### Phil Freeman ([paf31](https://github.com/paf31))

Phil is the creator of PureScript.  He contributes a lot of code to the
PureScript compiler, as well as responding to issues and pull requests.  He
also maintains the purescript-react and Thermite libraries.

#### Gary Burgess ([garyb](https://github.com/garyb))

Gary is working for SlamData and is also very active in the PureScript
community.  He does a lot of work on the PureScript compiler as well as
maintain the Halogen project.  Gary is _very_ helpful on Github.  He has
responded to many of my issues (even silly ones).  If PureScript succeeds as a
language, it will be partially due to the helpfulness of Gary.

#### John A. De Goes ([jdegoes](https://github.com/jdegoes))

John is the CTO of SlamData.  He is active within the Halogen project and also
responds to many issues in the PureScript compiler's issue tracker.  John is
also a long-time Haskell hacker.

## Conclusion

PureScript is very easy for a Haskeller to learn.  The most complicated thing
will be learning the PureScript/JavaScript ecosystem and deciding on a DOM
manipulation library.

Good luck on your PureScript journey.

## Footnotes

[^1]: It used to be, "Just use `cabal` and sandboxes.  For multi-project
builds... maybe try a small shell script?"

[^2]: Pulp was originally written in JavaScript, which may be why it does not
have the features one would expect from something like `stack`.  However, it is
[currently](https://github.com/bodil/pulp/issues/119) being rewritten in
PureScript, so maybe there is hope that it will be more flexible in the future.

[^3]: I believe I saw this number somewhere, but I can't find the page I saw it
on.  If someone could confirm/deny this, it would be much appreciated.
