# Benchmarks

This directory contains benchmarking scripts.

## Installation

You can install Quarto via:

- Homebrew: `brew install --cask quarto`
- R package: `install.packages("quarto")`
- Download from the [Quarto website](https://quarto.org/docs/get-started/).

## Rebuilding Benchmarks

To manually rebuild the benchmark results, run the following command from the root of the package:

Using the CLI:

```bash
quarto render benchmarks/linelist_benchmark.qmd
```

Or from R:

```r
quarto::quarto_render("benchmarks/linelist_benchmark.qmd")
```
