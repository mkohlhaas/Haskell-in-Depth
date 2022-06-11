Import the whole module.
Import only specific names from the module by listing them in parentheses after the module name.
Import no names at all with an empty list of names (note that this imports all the instances because they have no names in Haskell).
Import names with optional or mandatory qualification with an alias ( as) or a full module name to avoid name clashes.
Import all names except those listed after the hiding keyword.

Remember also that instances of type classes are always exported.
The module name can be hierarchical, with components separated by dots (e.g., Graphics.Rendering.Chart.Backend.Cairo). The Haskell Report does not set out a meaning for this hierarchy, ALTHOUGH EXISTING IMPLEMENTATIONS normally use it as an instruction for finding module source code files in subdirectories.
Note that a module is always imported by its full name, regardless of whether we import it from the same subdirectory (as in S.A) or a neighboring subdirectory (as in T.B ).
Whenever we write import in a program, we import a particular module but not the whole module subhierarchy. Hierarchical module names are about naming only, and we shouldn’t expect anything else.

Provide include/module path with -i(path no spaces after flag):
$ ghci B.hs -i..

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
-- no names from Prelude
```

Custom preludes:
- https://hackage.haskell.org/packages/#cat:Prelude
- https://hackage.haskell.org/packages/search?terms=prelude
- relude (most stars on Github and very active; from kowainik)
- foundation
- protolude
- classy-prelude
- universum

GHC operates two package databases by default: the global one and a user-specific one, arranged in a stack with the user-specific database on top. It starts searching for
packages at the top of the stack and continues all the way to the bottom. We can specify additional databases or compile our project against a completely different stack of
package databases using the GHC_PACKAGE_PATH environment variable.

Besides using GHC package databases, we can maintain our own package databases. We can create a package
database for a specific project. The packages we have there don’t intervene with anything within a system.

```shell
$ ghc-pkg --help
$ ghc-pkg list
$ ghc-pkg list --user
$ ghc-pkg find-module '*time*'
$ ghc-pkg describe time
```

No matter which project management tool is used, Cabal as a library is always used to manipulate Haskell packages. Cooking a package out of the source files is called cabalizing.

```shell
$ cabal init # starting point for new project
$ cabal-bounds --help
$ cabal-bounds format
$ cabal-bounds update
```

Haskell Package Versioning Policy - PVP - https://pvp.haskell.org/
Version numbers: https://pvp.haskell.org/#version-numbers
base-4.11.0.1 -> 4.11 is major version number, 0.1 is minor version number

Some people use semantic versioning: https://semver.org/
Author supports PVP.

Tip:
Use hpack: https://github.com/sol/hpack

Cabal hell is history.

Three approaches:
- Curated sets of packages -> Stack
- Sandboxing -> old Cabal
- Persistent storage of uniquely identified package builds -> Nix, new Cabal (>=1.24, default since 3.0)

Cabal keeps every once-required version of every package built against every once-required set of dependencies in a shared environment, at the cost of greater storage requirements.
Cabal always tries to get the newest versions available. If not wanted use `cabal freeze`.

Check for outdated packages:
```shell
cabal outdated
```

Use Hackage Dependency Monitor: https://packdeps.haskellers.com/

A package consisting of several packages is called a project and uses a `cabal.project` file, e.g. https://github.com/timbod7/haskell-chart/

`cabal.project.local` keeps special developer box settings, e.g.
package gtk
  flags: +have-quartz-gtk

Many compiler options can be specified in `cabal.project`, either manually or by running the `cabal configure` command.

Use `cabal gen-bounds` to generate and test for version boundaries.

For starting a new project you can use the Summoner, https://kowainik.github.io/projects/summoner
