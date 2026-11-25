# argv-process

[![CI](https://github.com/jeremiepas/argv-process/actions/workflows/ci.yml/badge.svg)](https://github.com/jeremiepas/argv-process/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/argv-process.svg)](https://hackage.haskell.org/package/argv-process)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Run a program from an argument vector, never through a shell, with a time limit and no orphan process.

## Use it

Add the package to the `build-depends` of your cabal file:

```cabal
build-depends: argv-process ^>=0.1
```

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T
import System.Process.Argv

main :: IO ()
main = do
  -- "a; rm -rf ~" reaches printf as one argument: no shell ever reads it.
  result <- runWith defaultOptions {timeLimitSeconds = Just 10} ["printf", "%s\n", "a; rm -rf ~"]
  case result of
    Left problem -> T.putStrLn (describeStartError problem)
    Right (Result (Exited 0) out _) -> T.putStr out
    Right (Result end _ err) -> print end >> T.putStr err
```

The program runs in a process group of its own. When it exceeds its time limit, when the calling
thread gets an exception, or when the calling process dies (even by `SIGKILL`), the group is
terminated: the program and everything it started. `System.Process.Argv.Group` gives the same
guarantees to a process you start yourself. POSIX systems only.

## Build and test

The compiler and cabal are pinned with [devenv](https://devenv.sh):

```bash
devenv shell -- lib-ci      # build, tests, cabal check, docs, and a build from the source tarball
devenv shell -- lib-test    # the tests only
```

Without devenv, a GHC listed in `tested-with` and `cabal test` are enough.

## Versions and releases

Versions follow the [Package Versioning Policy](https://pvp.haskell.org). Each release is a tag
`v<version>`, a GitHub release with the source tarball, and the same tarball on
[Hackage](https://hackage.haskell.org/package/argv-process). See [CHANGELOG.md](CHANGELOG.md).

## Contributing

Issues and pull requests are welcome: see [CONTRIBUTING.md](CONTRIBUTING.md). To report a
vulnerability, see [SECURITY.md](SECURITY.md).

## License

[MIT](LICENSE)
