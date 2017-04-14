# ctags-shim

This is a simple program that bundles a variety of tag generating binaries into one command. It can easily be extended via a config file.

`ctags-shim` ignores files listed in `.gitignore`.

## Config file

The default ctags-shim config is located at `~/.ctags-shim.yaml` and has a format like below:
```yaml
excludes:
  - dist
  - .git
  - Setup.hs
executables:
  - ctags:
      flags:
        - "-f -"
      extensions:
        - cpp
        - c
        - h
        - hpp
        - py
  - hothasktags:
      extensions:
        - hs
```

The `excludes` section is a list of glob patterns to be ignored when `ctags-shim` recurses through directories.

## Installing

You need GHC and cabal for installation.

```
> git clone https://github.com/tkonolige/ctags-shim.git
> cd ctags-shim
> cabal install
```

The default install location is `~/.cabal/bin`. You probably want to add this to your `$PATH`.

## Adding a tags generator

To add a new generator, add a new entry under the `executables` section of the config. The format should be:
```yaml
executables:
  - my_binary
    flags:
      - "-f -"
    extensions:
      - my_filetype
```

The `flags` section is passed to the binary everytime it is called. The binary should output to stdout and should not recurse.
