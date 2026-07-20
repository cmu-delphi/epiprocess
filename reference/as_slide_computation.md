# Generate a `epi[x]_slide` computation function from a function, formula, or quosure

`as_slide_computation()` transforms a one-sided formula or a quosure
into a function; functions are returned as-is or with light
modifications to calculate `ref_time_value`.

This code extends
[`rlang::as_function`](https://rlang.r-lib.org/reference/as_function.html)
to create functions that take three arguments. The arguments can be
accessed via the idiomatic `.`, `.x`, and `.y`, extended to include
`.z`; positional references `..1` and `..2`, extended to include `..3`;
and also by `epi[x]_slide`-specific names `.group_key` and
`.ref_time_value`.

## Usage

``` r
as_slide_computation(
  .f,
  ...,
  .f_arg = caller_arg(.f),
  .call = caller_env(),
  .ref_time_value_long_varnames,
  .ref_time_value_label
)

as_time_slide_computation(
  .f,
  ...,
  .f_arg = caller_arg(.f),
  .call = caller_env()
)

as_diagonal_slide_computation(
  .f,
  ...,
  .f_arg = caller_arg(.f),
  .call = caller_env()
)
```

## Source

This code and documentation are based on
[`as_function`](https://github.com/r-lib/rlang/blob/c55f6027928d3104ed449e591e8a225fcaf55e13/R/fn.R#L343-L427)
from Hadley Wickham's `rlang` package.

Below is the original license for the `rlang` package.

MIT License

Copyright (c) 2020 rlang authors

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Portions of the original code used in this adaptation:

1.  Much of the documentation and examples

2.  The general flow of the function, including branching conditions

3.  Error conditions and wording

4.  The chunk converting a formula into a function, see
    https://github.com/r-lib/rlang/blob/c55f6027928d3104ed449e591e8a225fcaf55e13/R/fn.R#L411-L418

Changes made include:

1.  Updates to documentation due to new functionality

2.  The removal of function-as-string processing logic and helper arg
    `env`

3.  The addition of an output function wrapper that defines a data mask
    for evaluating quosures

4.  Calling an argument-checking function

5.  Replacing rlang error functions with internal error functions

## Arguments

- ...:

  Additional arguments to pass to the function or formula specified via
  `x`. If `x` is a quosure, any arguments passed via `...` will be
  ignored.

- .ref_time_value_long_varnames:

  **\[experimental\]** Character vector. What variable names should we
  allow formulas and data-masking tidy evaluation to use to refer to
  `ref_time_value` for the computation (in addition to `.z` in
  formulas)? E.g., `".ref_time_value"` or
  `c(".ref_time_value", ".version")`.

- .ref_time_value_label:

  String; how to describe/label the `ref_time_value` in error messages;
  e.g., "reference time value" or "version".

- f:

  A function, one-sided formula, or quosure.

  If a **function**, the function is returned as-is, with no
  modifications.

  If a **formula**, e.g. `~ mean(.x$cases)`, it is converted to a
  function with up to three arguments: `.x` (single argument), or `.x`
  and `.y` (two arguments), or `.x`, `.y`, and `.z` (three arguments).
  The `.` placeholder can be used instead of `.x`, `.group_key` can be
  used in place of `.y`, and `.ref_time_value` can be used in place of
  `.z`. This allows you to create very compact anonymous functions
  (lambdas) with up to three inputs. Functions created from formulas
  have a special class. Use
  `inherits(fn, "epiprocess_slide_computation")` to test for it.

  If a **quosure**, in the case that `f` was not provided to the parent
  `epi[x]_slide` call and the `...` is interpreted as an expression for
  tidy evaluation, it is evaluated within a wrapper function. The
  wrapper sets up object access via a data mask.
