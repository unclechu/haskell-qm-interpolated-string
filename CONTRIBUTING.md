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

See https://hackage.haskell.org/upload page for details.

We are going to use Cabal for this. If you are using Nix you can just enter
`nix-shell` being in the directory of this repository, Cabal will be available.
I recommend to use at least Cabal v3 for this.

1. Check the CI first that everything is green.

   https://github.com/unclechu/haskell-qm-interpolated-string/actions

2. Cleanup the artifacts of previous builds:

   ``` sh
   cabal clean
   ```

3. Check that everything is okay with *.cabal file:

   ``` sh
   cabal check
   ```

4. Build the library:

   ``` sh
   cabal build
   ```

5. Run tests:

   ``` sh
   cabal test
   ```

6. Build documentation using [Haddock][Haddock]:

   ``` sh
   cabal v2-haddock --builddir=dist-docs --haddock-for-hackage --enable-documentation
   ```

7. Create a source distribution:

   ``` sh
   cabal sdist
   ```

8. Test the distribution as a *candidate*:


   1. Upload produced tarball:

      ``` sh
      cabal upload dist-newstyle/sdist/qm-interpolated-string-0.3.1.0.tar.gz
      ```

      where `0.3.1.0` is a version you're going to release.

   2. Upload previously generated docs for the *candidate*:

      ``` sh
      cabal upload -d dist-docs/qm-interpolated-string-0.3.1.0-docs.tar.gz
      ```

      where `0.3.1.0` is a version you're going to release.

   3. Look for the given link in the logs to the *candidate* page, open it,
      and inspect carefully that everything seems correct, including
      documentation.

9. If you sure it's ready to be released (**this cannot be undone!**) then do it:

   ``` sh
   cabal upload --publish dist-newstyle/sdist/qm-interpolated-string-0.3.1.0.tar.gz
   ```

   where `0.3.1.0` is a version you're going to release.

10. Publish previously generated docs:

    ``` sh
    cabal upload --publish -d dist-docs/qm-interpolated-string-0.3.1.0-docs.tar.gz
    ```

    where `0.3.1.0` is a version you're going to release.

11. Do not forget to create and push a Git tag for the new version!

    https://github.com/unclechu/haskell-qm-interpolated-string/tags

[Haddock]: https://hackage.haskell.org/package/haddock
