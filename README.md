
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ricci

<!-- badges: start -->

[![R-CMD-check](https://github.com/lschneiderbauer/ricci/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lschneiderbauer/ricci/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/lschneiderbauer/ricci/graph/badge.svg)](https://app.codecov.io/gh/lschneiderbauer/ricci)
[![CRAN
status](https://www.r-pkg.org/badges/version/ricci)](https://CRAN.R-project.org/package=ricci)

<!-- badges: end -->

The goal of {ricci} is to provide a *compact*[^1] R interface for
performing [tensor
calculations](https://en.wikipedia.org/wiki/Ricci_calculus). This is
achieved by labeling (upper and lower) index slots of R’s `array` and
making use of Ricci calculus conventions to *implicitly* trigger
contractions and diagonal subsetting. Explicit tensor operations, such
as addition, multiplication of tensors, raising and lowering indices, or
the Kronecker product are also available via the standard operators
(`*`, `+`, `-`). Common tensors like the Kronecker delta, Levi Civita
epsilon, and certain metric tensors are provided.

{ricci} uses the [calculus](https://calculus.eguidotti.com/) package
(Guidotti 2022) behind the scenes to perform calculations and simply
provides an alternative interface to a subset of its functionality.

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
calculations have been carried out we remove the labels to obtain an
`array` again.

For demonstration purposes we use an arbitrary array of rank 3.

``` r
library(ricci)

# the data
a <- array(1:(2^2*3), dim = c(2,2,3))
```

### Creating a labeled tensor

We can use the array `a` to create a labeled tensor with lower index
labels i, j, and k:

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

Creating labels on its own is not very interesting nor helpful. The act
of labeling tensor index slots becomes useful when the labels are set
such that they trigger implicit calculations, or they are combined with
other tensors via multiplication or addition.

#### Contraction

Repeated index labels with opposite position are implicitly contracted.

$$
b_j=a_{i\;k}^{\;i}
$$

``` r
b <- a %_% .(i, +i, k)
b
#> <Labeled Tensor> [3] <-> .(-k)
#> [1]  5 13 21

# retrieve array
b |> .a(k)
#> [1]  5 13 21
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
#> <Labeled Tensor> [2x3] <-> .(-i, -k)
#>      [,1] [,2] [,3]
#> [1,]    1    5    9
#> [2,]    4    8   12

# retrieve array
c |> .a(i, k)
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
d <- a %_% .(i, j, k) * a %_% .(l, m, n)

# retrieve array
# we can use any index order we like
d |> .a(l, m, i, n, j, k)
#> , , 1, 1, 1, 1
#> 
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4
#> 
#> , , 2, 1, 1, 1
#> 
#>      [,1] [,2]
#> [1,]    2    6
#> [2,]    4    8
#> 
#> , , 1, 2, 1, 1
#> 
#>      [,1] [,2]
#> [1,]    5    7
#> [2,]    6    8
#> 
#> , , 2, 2, 1, 1
#> 
#>      [,1] [,2]
#> [1,]   10   14
#> [2,]   12   16
#> 
#> , , 1, 3, 1, 1
#> 
#>      [,1] [,2]
#> [1,]    9   11
#> [2,]   10   12
#> 
#> , , 2, 3, 1, 1
#> 
#>      [,1] [,2]
#> [1,]   18   22
#> [2,]   20   24
#> 
#> , , 1, 1, 2, 1
#> 
#>      [,1] [,2]
#> [1,]    3    9
#> [2,]    6   12
#> 
#> , , 2, 1, 2, 1
#> 
#>      [,1] [,2]
#> [1,]    4   12
#> [2,]    8   16
#> 
#> , , 1, 2, 2, 1
#> 
#>      [,1] [,2]
#> [1,]   15   21
#> [2,]   18   24
#> 
#> , , 2, 2, 2, 1
#> 
#>      [,1] [,2]
#> [1,]   20   28
#> [2,]   24   32
#> 
#> , , 1, 3, 2, 1
#> 
#>      [,1] [,2]
#> [1,]   27   33
#> [2,]   30   36
#> 
#> , , 2, 3, 2, 1
#> 
#>      [,1] [,2]
#> [1,]   36   44
#> [2,]   40   48
#> 
#> , , 1, 1, 1, 2
#> 
#>      [,1] [,2]
#> [1,]    5   15
#> [2,]   10   20
#> 
#> , , 2, 1, 1, 2
#> 
#>      [,1] [,2]
#> [1,]    6   18
#> [2,]   12   24
#> 
#> , , 1, 2, 1, 2
#> 
#>      [,1] [,2]
#> [1,]   25   35
#> [2,]   30   40
#> 
#> , , 2, 2, 1, 2
#> 
#>      [,1] [,2]
#> [1,]   30   42
#> [2,]   36   48
#> 
#> , , 1, 3, 1, 2
#> 
#>      [,1] [,2]
#> [1,]   45   55
#> [2,]   50   60
#> 
#> , , 2, 3, 1, 2
#> 
#>      [,1] [,2]
#> [1,]   54   66
#> [2,]   60   72
#> 
#> , , 1, 1, 2, 2
#> 
#>      [,1] [,2]
#> [1,]    7   21
#> [2,]   14   28
#> 
#> , , 2, 1, 2, 2
#> 
#>      [,1] [,2]
#> [1,]    8   24
#> [2,]   16   32
#> 
#> , , 1, 2, 2, 2
#> 
#>      [,1] [,2]
#> [1,]   35   49
#> [2,]   42   56
#> 
#> , , 2, 2, 2, 2
#> 
#>      [,1] [,2]
#> [1,]   40   56
#> [2,]   48   64
#> 
#> , , 1, 3, 2, 2
#> 
#>      [,1] [,2]
#> [1,]   63   77
#> [2,]   70   84
#> 
#> , , 2, 3, 2, 2
#> 
#>      [,1] [,2]
#> [1,]   72   88
#> [2,]   80   96
#> 
#> , , 1, 1, 1, 3
#> 
#>      [,1] [,2]
#> [1,]    9   27
#> [2,]   18   36
#> 
#> , , 2, 1, 1, 3
#> 
#>      [,1] [,2]
#> [1,]   10   30
#> [2,]   20   40
#> 
#> , , 1, 2, 1, 3
#> 
#>      [,1] [,2]
#> [1,]   45   63
#> [2,]   54   72
#> 
#> , , 2, 2, 1, 3
#> 
#>      [,1] [,2]
#> [1,]   50   70
#> [2,]   60   80
#> 
#> , , 1, 3, 1, 3
#> 
#>      [,1] [,2]
#> [1,]   81   99
#> [2,]   90  108
#> 
#> , , 2, 3, 1, 3
#> 
#>      [,1] [,2]
#> [1,]   90  110
#> [2,]  100  120
#> 
#> , , 1, 1, 2, 3
#> 
#>      [,1] [,2]
#> [1,]   11   33
#> [2,]   22   44
#> 
#> , , 2, 1, 2, 3
#> 
#>      [,1] [,2]
#> [1,]   12   36
#> [2,]   24   48
#> 
#> , , 1, 2, 2, 3
#> 
#>      [,1] [,2]
#> [1,]   55   77
#> [2,]   66   88
#> 
#> , , 2, 2, 2, 3
#> 
#>      [,1] [,2]
#> [1,]   60   84
#> [2,]   72   96
#> 
#> , , 1, 3, 2, 3
#> 
#>      [,1] [,2]
#> [1,]   99  121
#> [2,]  110  132
#> 
#> , , 2, 3, 2, 3
#> 
#>      [,1] [,2]
#> [1,]  108  132
#> [2,]  120  144
```

#### Tensor multiplication w/ contractions

$$
e=a_{ijk}a^{ijk}
$$

``` r
e <- a %_% .(i, j, k) * a %_% .(+i, +j, +k)
e
#> <Scalar>
#> [1] 650

# convert to number
as.numeric(e)
#> [1] 650
```

#### Tensor multiplication w/ contractions and subsetting

$$
f_j=a_{ijk}a^{i\;k}_{\;j}
$$

``` r
f <- a %_% .(i, j, k) * a %_% .(+i, j, +k)
f
#> <Labeled Tensor> [2] <-> .(-j)
#> [1] 247 403

# retrieve array
f |> .a(j)
#> [1] 247 403
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
#> <Labeled Tensor> [2x2x3] <-> .(-i, -j, -k)
#> [1] "(not reduced)"

g |> .a(i, j, k)
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    5    8
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]   10   13
#> [2,]   13   16
#> 
#> , , 3
#> 
#>      [,1] [,2]
#> [1,]   18   21
#> [2,]   21   24
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
