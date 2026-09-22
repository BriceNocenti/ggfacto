# Pipe operator (deprecated)

Re-exported from magrittr for backward compatibility. Deprecated: use
the base pipe `|>` instead. ggfacto uses `|>` everywhere internally, and
this re-export will be removed in a future release, taking the magrittr
dependency with it.

## Usage

``` r
lhs %>% rhs
```

## Arguments

- lhs:

  A value or the magrittr placeholder.

- rhs:

  A function call using the magrittr semantics.

## Value

Pipe an object forward into a function or call expression.

## Details

See `magrittr::%>%` for details.
