#### Fetch all unfoldings

A simple experiment to see if we can fetch definitions (unfoldings)
of the transitive closure of a module from within a GHC Plugin:
- [plugin](./plugin): the plugin, prints all definitions it has access to.
- [examples](./examples): a package that uses this plugin.
- [test-lib](./test-lib): a library used by "examples".


This stanza in the `cabal.project` file is the key bit:

```
package *
    ghc-options: -fexpose-all-unfoldings
```

It re-compiles all the dependencies with the flag `-fexpose-all-unfoldings` which
allows us to fetch the definitions.
This doesn't work for GHC's core libraries; Cabal always uses their precompiled versions.
To try this, you can run `cabal v2-build all > all_unfoldings.txt`.
This compiles `examples` with the GHC plugin and writes its output to `all_unfoldings.txt`
in which you should see all definitions from the libraries
`unordered-containers` (a direct dependency of examples) and
`hashable` (a transitive dependency of examples).
