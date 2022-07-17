# Contributing guide for “qm-interpolated-string” Haskell library

This document has some notes, instructions, and recommendations for making a
contribution to the project.


## Development environment

This project supports 3 build tools:

1. [Cabal](https://www.haskell.org/cabal/)
2. [Stack](https://haskellstack.org)
3. [Nix (package manager)](https://github.com/NixOS/nix)
   (also Cabal and Stack are available in the nix-shell configuration)


### Cabal

``` sh
cabal build
```


### Stack

``` sh
stack build
```


### Nix

#### Build

``` sh
nix-build
```

See for `result*` files.

#### Enter development nix-shell

``` sh
nix-shell
```

You can also use [direnv](https://direnv.net/).
See [.envrc example file](.envrc.example).

##### Using Cabal and Stack inside nix-shell

Cabal is turned on by default. You can change what tools are available by
providing `buildTools` argument:

``` sh
nix-shell --arg buildTools '[ "cabal" "stack" ]'
```

##### Haskell Language Server

[HLS](https://github.com/haskell/haskell-language-server) is turned on by
default (see `withHLS` argument).

###### Vim

If you are using (Neo)Vim you can use
[vim-lsp](https://github.com/prabirshrestha/vim-lsp) plugin.
Here is a configuration example:

``` viml
if executable('haskell-language-server-wrapper')
  aug HaskellLsp
  au! User lsp_setup cal lsp#register_server({
    \ 'name': 'hls',
    \ 'cmd': {server_info->['haskell-language-server-wrapper', '--lsp']},
    \ 'allowlist': ['haskell'],
    \ })
  aug END
en
```

##### Using different GHC versions

See `nixpkgs.haskell.packages.*` for available GHC versions.
Example (HLS was failing to build with GHC 9.2.3 at the moment, turned it off):

``` sh
nix-shell --argstr ghcVersion ghc923 --arg withHLS false
```


## How to upload new version to Hackage

**WARNING!** Disclaimer: This instruction was written in 2018. Some parts are
outdated already. It for instance refers to Cabal’s “sandbox” feature which is
not needed anymore. Also Travis CI is not freely available for open source
projects anymore. So it was removed from this project. This section needs to be
updated (**FIXME**).

See also:
  * https://hackage.haskell.org/upload
  * https://begriffs.com/posts/2014-10-25-creating-package-hackage.html

We're going to use Cabal for this and do everything inside cabal-sandbox.

1. Make sure you have successful build of your branch on
   [Travis CI](https://travis-ci.org/unclechu/haskell-qm-interpolated-string);

2. It's better to start with clean state to make sure everything goes right, so, remove old cabal-sandbox first:

   ```bash
   cabal sandbox delete
   ```

3. Check that everything is okay with *.cabal file:

   ```bash
   cabal check
   ```

4. Create new sandbox and install dependencies (for tests too):

   ```bash
   cabal sandbox init
   cabal install --only-dependencies --enable-tests
   ```

5. Build the library:

   ```bash
   cabal build
   ```

6. Run tests:

   ```bash
   cabal test
   ```

7. Build documentation using [Haddock](http://hackage.haskell.org/package/haddock):

   ```bash
   cabal haddock --builddir=docs --for-hackage
   ```

8. Create a source distribution:

   ```bash
   cabal sdist
   ```

   And make sure it goes okay by making *package candidate* release:

   1. Upload produced tarball:

      - By going to this link https://hackage.haskell.org/packages/candidates/upload
      - Or by running this command:

        ```bash
        cabal upload dist/qm-interpolated-string-0.3.0.0.tar.gz
        ```

        where `0.3.0.0` is a version you're going to release.

   2. Upload previously generated docs to the *package candidate*:

      ```bash
      cabal upload -d docs/qm-interpolated-string-0.3.0.0-docs.tar.gz
      ```

      where `0.3.0.0` is a version you're going to release.

9. If you sure it's ready to be released (**this cannot be undone!**) then do it:

   - By going to this link: https://hackage.haskell.org/packages/upload
   - Or by running this command:

     ```bash
     cabal upload --publish dist/qm-interpolated-string-0.3.0.0.tar.gz
     ```

     where `0.3.0.0` is a version you're going to release.

10. Publish previously generated docs:

    ```bash
    cabal upload --publish -d docs/qm-interpolated-string-0.3.0.0-docs.tar.gz
    ```

    where `0.3.0.0` is a version you're going to release.

11. Be happy, your life isn't worthless anymore.
