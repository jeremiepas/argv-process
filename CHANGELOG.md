# Changelog

All notable changes to `argv-process` are listed here. Versions follow the
[Package Versioning Policy](https://pvp.haskell.org): `A.B.C.D`, where `A.B` changes when the API
breaks, `C` when something is added and `D` for a fix.

## Unreleased

- First version, extracted from application code:
  `run` and `runWith` in `System.Process.Argv` (argument vector, captured outputs, optional standard
  input, time limit), and `System.Process.Argv.Group` (terminating a process group, and the watchdog
  that terminates it when the caller dies).
