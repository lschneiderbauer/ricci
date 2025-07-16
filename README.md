
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ricci

<!-- badges: start -->

[![R-CMD-check](https://github.com/lschneiderbauer/ricci/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lschneiderbauer/ricci/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/lschneiderbauer/ricci/graph/badge.svg)](https://app.codecov.io/gh/lschneiderbauer/ricci)
[![CRAN
status](https://www.r-pkg.org/badges/version/ricci)](https://CRAN.R-project.org/package=ricci)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of {ricci} is to provide a *compact*[^1] R interface for
performing [tensor
calculations](https://en.wikipedia.org/wiki/Ricci_calculus). This is
achieved by labeling (upper and lower) index slots of R’s `array` and
making use of Ricci calculus conventions to *implicitly* trigger
contractions and diagonal subsetting. Explicit tensor operations, such
as addition, subtraction, multiplication of tensors via the standard
operators (`*`, `+`, `-`), raising and lowering indices, taking
symmetric or antisymmetric tensor parts, as well as the Kronecker
product are available. Common tensors like the Kronecker delta, Levi
Civita epsilon, and certain metric tensors are provided. An effort was
made to provide the user with meaningful error messages.

{ricci} uses the [calculus](https://calculus.eguidotti.com/) package
(Guidotti 2022) behind the scenes to perform calculations and provides
an alternative interface to a subset of its functionality. Notably,
{[calculus](https://calculus.eguidotti.com/)} also supports symbolic
calculations which also enables {ricci} to do the same.

## Installation

You can install the development version of ricci from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("lschneiderbauer/ricci")
```

## Examples

The central object is R’s `array`. Adding index slot labels allows us to
perform common tensor operations implicitly. After the desired
calculations have been carried out we can remove the labels to obtain an
ordinary `array`.

The following example shows how to express the contraction of two
tensors, where one index has to be raised, and subsequent diagonal
subsetting. For demonstration purposes we use an arbitrary array of rank
3.

``` r
library(ricci)

# numeric data
a <- array(1:(2^3), dim = c(2,2,2))

# create labeled array (tensor)
(a %_% .(i, j, k) * 
  # mutliply with a labeled array (tensor) and raise index i and k
  a %_% .(i, l, k) |> r(i, k, g = g_mink(2))) |> 
  # * -i and +i as well as -k and +k dimension are implictely contracted
  # the result is a tensor of rank 2
  sym(j, l) |> # symmetrize over i and l
  subst(l -> j) |> # rename index and trigger diagonal subsetting
  as_a(j) # we unlabel the tensor with index order (j)
#> [1] 8 8
```

The same instructions work for a symbolic array:

``` r
library(ricci)

# symbolic data
a <- array(paste0("a", 1:(2^3)), dim = c(2,2,2))

(a %_% .(i, j, k) * 
  # mutliply with a labeled array (tensor) and raise index i and k
  a %_% .(i, l, k) |> r(i, k, g = g_mink(2))) |> 
  # * -i and +i as well as -k and +k dimension are implictely contracted
  # the result is a tensor of rank 2
  sym(j, l) |> # symmetrize over i and l
  subst(l -> j) |> # rename index and trigger diagonal subsetting
  as_a(j) # we unlabel the tensor with index order (j)
#> [1] "((a1) * (((a1) * -1) * -1) + (a2) * (((a2) * 1) * -1) + (a5) * (((a5) * -1) * 1) + (a6) * (((a6) * 1) * 1) + (a1) * (((a1) * -1) * -1) + (a2) * (((a2) * 1) * -1) + (a5) * (((a5) * -1) * 1) + (a6) * (((a6) * 1) * 1)) / 2"
#> [2] "((a3) * (((a3) * -1) * -1) + (a4) * (((a4) * 1) * -1) + (a7) * (((a7) * -1) * 1) + (a8) * (((a8) * 1) * 1) + (a3) * (((a3) * -1) * -1) + (a4) * (((a4) * 1) * -1) + (a7) * (((a7) * -1) * 1) + (a8) * (((a8) * 1) * 1)) / 2"
```

Below we outline more details on possible individual operations.

### Creating a labeled array (tensor)

We can use the array `a` to create a labeled array (tensor) with lower
index labels i, j, and k:

$$
a_{ijk}
$$

``` r
a %_% .(i, j, k)
#> <Labeled Array> [2x2x2] .(-i, -j, -k)
```

By default, indices are assumed to be lower indices. We can use a “+”
prefix to create an upper index label.

$$
a_{ij}^{\;\;k}
$$

``` r
a %_% .(i, j, +k)
#> <Labeled Array> [2x2x2] .(-i, -j, +k)
```

### Performing calculations

Creating index labels on its own is not very interesting nor helpful.
The act of labeling tensor index slots becomes useful when the labels
are set such that they trigger implicit calculations, or they are
combined with other tensors via multiplication or addition.

#### Contraction

Repeated index labels with opposite position are implicitly contracted.

$$
b_j=a_{i\;k}^{\;i}
$$

``` r
b <- a %_% .(i, +i, k)
b
#> <Labeled Array> [2] .(-k)
#> [1] "a1 + a4" "a5 + a8"

# retrieve array
b |> as_a(k)
#> [1] "a1 + a4" "a5 + a8"
```

#### Diagonal subsetting

Repeated labels on the same position (upper or lower) will trigger
diagonal subsetting.

$$
c_{ik}=a_{iik}
$$

``` r
c <- a %_% .(i, i, k)
c
#> <Labeled Array> [2x2] .(-i, -k)
#>      [,1] [,2]
#> [1,] "a1" "a5"
#> [2,] "a4" "a8"

# retrieve array
c |> as_a(i, k)
#>      [,1] [,2]
#> [1,] "a1" "a5"
#> [2,] "a4" "a8"
```

#### Outer tensor product

The same conventions apply for arbitrary tensor multiplication.

$$
d_{ijklmn}=a_{ijk}a_{lmn}
$$

``` r
d <- a %_% .(i, j, k) * a %_% .(l, m, n)
```

#### Tensor multiplication w/ contractions

$$
e=a_{ijk}a^{ijk}
$$

``` r
e <- a %_% .(i, j, k) * a %_% .(+i, +j, +k)
e
#> <Scalar>
#> [1] "(a1) * (a1) + (a2) * (a2) + (a3) * (a3) + (a4) * (a4) + (a5) * (a5) + (a6) * (a6) + (a7) * (a7) + (a8) * (a8)"
```

#### Tensor multiplication w/ contractions and subsetting

$$
f_j=a_{ijk}a^{i\;k}_{\;j}
$$

``` r
f <- a %_% .(i, j, k) * a %_% .(+i, j, +k)
f
#> <Labeled Array> [2] .(-j)
#> [1] "(a1) * (a1) + (a2) * (a2) + (a5) * (a5) + (a6) * (a6)"
#> [2] "(a3) * (a3) + (a4) * (a4) + (a7) * (a7) + (a8) * (a8)"

# retrieve array
f |> as_a(j)
#> [1] "(a1) * (a1) + (a2) * (a2) + (a5) * (a5) + (a6) * (a6)"
#> [2] "(a3) * (a3) + (a4) * (a4) + (a7) * (a7) + (a8) * (a8)"
```

#### Tensor addition

Tensor addition is taking care of correct index slot matching (by index
labels), so the position of the index does not matter.

$$
g_{ijk} = a_{ijk} + a_{jik}
$$

``` r
g <- a %_% .(i, j, k) + a %_% .(j, i, k)
g
#> <Labeled Array> [2x2x2] .(-i, -j, -k)

g |> as_a(i, j, k)
#> , , 1
#> 
#>      [,1]      [,2]     
#> [1,] "a1 + a1" "a3 + a2"
#> [2,] "a2 + a3" "a4 + a4"
#> 
#> , , 2
#> 
#>      [,1]      [,2]     
#> [1,] "a5 + a5" "a7 + a6"
#> [2,] "a6 + a7" "a8 + a8"
```

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-guidotti2022" class="csl-entry">

Guidotti, Emanuele. 2022. “**Calculus**: High-Dimensional Numerical and
Symbolic Calculus in *R*.” *Journal of Statistical Software* 104 (5).
<https://doi.org/10.18637/jss.v104.i05>.

</div>

</div>

[^1]: By compact interface, we mean an interface that is concise and
    non-verbose. The author is of the opinion that the less we need to
    write in order to express an intent, the less likely are we to make
    mistakes.
