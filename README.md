# Distributed Systems Art

This repo contains Haskell code for generating a set of images I am
using in a technical memo.

This repo isn't the most beautiful and elegant solution for what I'm
doing. But it's what I have.

## Layout

There are three main pieces:
- A jupyter notebook containing Haskell diagrams code for live editing
- A Haskell program which, ideally, contains the same Haskell code as the notebook
  The resulting executable, `generator`, is a program that creates rendered Haskell diagrams, typically as `.png` or `.pdf` files
- A simple ruby script that runs `generator` and dumps the rendered diagrams into a directory

## Usage

### The notebook

To launch a jupyter notebook

``` shell
nix run
```

### The generator

``` shell
nix build .#generator
```
followed by

``` shell
./result/bin/generator --help
```

Alternatively,

``` shell
nix run .#generator -- --help
```

Note that the first `--` is used to separate arguments to `nix` and arguments to `generator`.

### The Ruby script

``` shell
nix build .#art
```

The resulting `result/` directory contains the beautiful art

