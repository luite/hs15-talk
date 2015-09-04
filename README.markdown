# Haskell Symposium 2015 talk

This is the source code for the slides of my Haskell Symposium 2015 lighting talk on GHCJS. The format is markdown; pandoc is used to produce the result, with a filter to generate the interactive code blocks.

I wrote this code in one day, at a time that libraries like `ghcjs-dom` were broken due to changes in `ghcjs-base`, so the code is a bit messy and low-level. The CSS in particular needs some work. If anyone wants to turn this into a more serious library for doing Haskell presentations I'd be happy to assist.

[See the final result here](http://files.luite.com/hs15/)

## Getting started

First install GHC 7.10.2 or higher and make sure that your `cabal-install` is at least version 1.22.4.0.

You need GHCJS to compile the inline interactive code examples. The pandoc library needs to be available for the `filter.hs` script.

Do the following to get pandoc and the latest `improved-base` snapshot of GHCJS:

```
$ cabal install http://ghcjs.luite.com/improved-base.tar.gz
$ ghcjs-boot
$ cabal install pandoc
```

Install the dependencies for the code in the slides:

```
$ cabal install --ghcjs diagrams-contrib
$ git clone -b improved-base https://github.com/ghcjs/diagrams-ghcjs.git
$ cabal install --ghcjs ./diagrams-ghcjs
```

Get the code, the reveal.js submodule and compile the slides:

```
$ git clone https://github.com/luite/hs15-talk.git
$ cd hs15-talk
$ git submodule update --init
$ ./build.sh
```

An easy way to view the slides is the `warp-static` server:

```
$ cabal install warp-static
$ warp
```

Now go to `http://localhost:3000/hs15.html`
