
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tensr

<!-- badges: start -->

[![R-CMD-check](https://github.com/lschneiderbauer/tensr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lschneiderbauer/tensr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/lschneiderbauer/tensr/graph/badge.svg)](https://app.codecov.io/gh/lschneiderbauer/tensr)

<!-- badges: end -->

The goal of tensr is to provide a *compact* interface for performing
[tensor calculations](https://en.wikipedia.org/wiki/Ricci_calculus).
This is achieved by adding (upper and lower) index slot labeling to R’s
`array` and making use of Ricci calculus conventions to implicitly
trigger contractions and diagonal subsetting. Explicit tensor
operations, such as addition, multiplication of tensors, raising and
lowering indices, or the Kronecker product are also available. Common
tensors like the Kronecker delta, Levi Civita epsilon, and common metric
tensors are provided.

Under the hood calculations are performed using the
[calculus](https://calculus.eguidotti.com/)(Guidotti 2022) package.

## Installation

You can install the development version of tensr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("lschneiderbauer/tensr")
```

## Example

``` r
library(tensr)
## basic example code
```

The central object is R’s `array`. Adding index slot labels allows us to
do common tensor operations implicitly:

``` r
# the data
a <- array(1:(2^2*3), dim = c(2,2,3))

# creating a index-slot-labeled tensor
# with "lower index" labels i, j, and k
a %_% .(i, j, k)
#> <Labeled Tensor> [2x2x3] <-> .(-i, -j, -k)

# upper indices can be declared by using a "+" sign,
# e.g. this tensor has "lower index" labels i, j
# and "upper index" k.
a %_% .(i, j, +k)
#> <Labeled Tensor> [2x2x3] <-> .(-i, -j, +k)

# repeated labels with opposite position are implicitly
# contracted
a %_% .(i, +i, k)
#> <Labeled Tensor> [3] <-> .(-k)
#> [1]  5 13 21

# same label with same position are implicitly
# (diagonally) subset
a %_% .(i, i, k)
#> <Labeled Tensor> [2x3] <-> .(-i, -k)
#>      [,1] [,2] [,3]
#> [1,]    1    5    9
#> [2,]    4    8   12


# the same conventions apply for arbitrary
# tensor multiplications
a %_% .(i, j, k) * a %_% .(l, m, n)
#> <Labeled Tensor> [2x2x3x2x2x3] <-> .(-i, -j, -k, -l, -m, -n)

a %_% .(i, j, k) * a %_% .(+i, +j, +k)
#> <Scalar>
#> [1] 650

a %_% .(i, j, k) * a %_% .(+i, j, +k)
#> <Labeled Tensor> [2] <-> .(-j)
#> [1] 247 403


# tensor addition is taking care of correct
# index slot matching (by label), so the position of the index
# does not matter
a %_% .(i, j, k) + a %_% .(j, i, k)
#> <Labeled Tensor> [2x2x3] <-> .(-i, -j, -k)
#> [1] "(not reduced)"
```

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-guidotti2022" class="csl-entry">

Guidotti, Emanuele. 2022. “**Calculus**: High-Dimensional Numerical and
Symbolic Calculus in *R*.” *Journal of Statistical Software* 104 (5).
<https://doi.org/10.18637/jss.v104.i05>.

</div>

</div>
