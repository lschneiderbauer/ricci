
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ricci

<!-- badges: start -->

[![R-CMD-check](https://github.com/lschneiderbauer/ricci/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lschneiderbauer/ricci/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/lschneiderbauer/ricci/graph/badge.svg)](https://app.codecov.io/gh/lschneiderbauer/ricci)
[![CRAN
status](https://www.r-pkg.org/badges/version/ricci)](https://CRAN.R-project.org/package=ricci)
<!-- badges: end -->

The goal of {ricci} is to provide a *compact* R interface for performing
[tensor calculations](https://en.wikipedia.org/wiki/Ricci_calculus).
This is achieved by labeling (upper and lower) index slots of R’s
`array` and making use of Ricci calculus conventions to implicitly
trigger contractions and diagonal subsetting. Explicit tensor
operations, such as addition, multiplication of tensors, raising and
lowering indices, or the Kronecker product are also available via the
standard operators (`*`, `+`, `-`). Common tensors like the Kronecker
delta, Levi Civita epsilon, and certain metric tensors are provided.

The idea to use tensor labels to specify calculations in R is not new
and is already used by the [calculus](https://calculus.eguidotti.com/)
package (Guidotti 2022). {ricci} uses that package behind the scenes to
perform calculations. {ricci} simply provides an alternative interface
to a subset of its functionality.

## Installation

You can install the development version of ricci from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("lschneiderbauer/ricci")
```

## Example

The central object is R’s `array`. Adding index slot labels allows us to
perform common tensor operations implicitly.

For demonstration purposes we use an arbitrary array of rank 3.

``` r
library(ricci)

# the data
a <- array(1:(2^2*3), dim = c(2,2,3))
```

### Creating a labeled tensor

We can use `a` to create a labeled tensor with lower index labels i, j,
and k:

$$
a_{ijk}
$$

``` r
a %_% .(i, j, k)
#> <Labeled Tensor> [2x2x3] <-> .(-i, -j, -k)
```

By default, indices are assumed to be lower indices. We can use a “+”
prefix to create an upper index label.

$$
a_{ij}^{\;\;k}
$$

``` r
a %_% .(i, j, +k)
#> <Labeled Tensor> [2x2x3] <-> .(-i, -j, +k)
```

### Performing calculations

Simply creating labels is not very interesting. The act of labeling
tensor index slots becomes useful when the labels are such that they
trigger implicit calculations, or they are combined with other tensors
via multiplication or addition.

#### Contraction

Repeated index labels with opposite position are implicitly contracted.

$$
b_j=a_{i\;k}^{\;i}
$$

``` r
a %_% .(i, +i, k)
#> <Labeled Tensor> [3] <-> .(-k)
#> [1]  5 13 21
```

#### Diagonal subsetting

Repeated labels on the same position (upper or lower) will trigger
diagonal subsetting.

$$
c_{ik}=a_{iik}
$$

``` r
a %_% .(i, i, k)
#> <Labeled Tensor> [2x3] <-> .(-i, -k)
#>      [,1] [,2] [,3]
#> [1,]    1    5    9
#> [2,]    4    8   12
```

#### Outer tensor product

The same conventions apply for arbitrary tensor multiplication.

$$
d_{ijklmn}=a_{ijk}a_{lmn}
$$

``` r
a %_% .(i, j, k) * a %_% .(l, m, n)
#> <Labeled Tensor> [2x2x3x2x2x3] <-> .(-i, -j, -k, -l, -m, -n)
```

#### Tensor multiplication w/ contractions

$$
e=a_{ijk}a^{ijk}
$$

``` r
a %_% .(i, j, k) * a %_% .(+i, +j, +k)
#> <Scalar>
#> [1] 650
```

#### Tensor multiplication w/ contractions and subsetting

$$
f_j=a_{ijk}a^{i\;k}_{\;j}
$$

``` r
a %_% .(i, j, k) * a %_% .(+i, j, +k)
#> <Labeled Tensor> [2] <-> .(-j)
#> [1] 247 403
```

#### Tensor addition

Tensor addition is taking care of correct index slot matching (by index
labels), so the position of the index does not matter.

$$
g_{ijk} = a_{ijk} + a_{jik}
$$

``` r
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
