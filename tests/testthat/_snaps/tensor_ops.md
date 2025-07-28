# tensor kronecker product works

    Code
      kron(arr %_% .(i, +j, k), l <- .(i, +j))
    Condition
      Error in `kron()`:
      ! In `...`: indices with different positions cannot be combined.
      x Indices `i` and `j` have different positions.
      i Make sure you combine only indices of the same position.

# raising tensor works

    Code
      r(arr %_% .(+i), i)
    Condition
      Error in `r()`:
      ! Argument `...` contains index with incorrect position.
      x Position of index `-i` does not match index position in <Labeled Array> [4] .(+i).
      i Make sure you explicitely specify the correct index position.

---

    Code
      r(arr %_% .(i), i, g = g_mink_cart(2))
    Condition
      Error in `r()`:
      ! Tensor index dimensions do not agree.
      x Tensor index `i` has dimension 4 in `arr %_% .(i)`.
      x Tensor index `i` has dimension 2 in `g_mink_cart(2)`.
      i Operation can only be carried out with two tensors having identical index dimensions.

# substituting labels works

    Code
      subst(arr %_% .(i, +j), k <- j)
    Condition
      Error in `subst()`:
      ! Argument `...` contains index with incorrect position.
      x Position of index `-j` does not match index position in <Labeled Array> [2x2] .(-i, +j).
      i Make sure you explicitely specify the correct index position.

---

    Code
      subst(arr %_% .(i, j), l <- k)
    Condition
      Error in `subst()`:
      ! Argument `...` contains invalid index.
      x Index `k` not present in <Labeled Array> [2x2] .(-i, -j).
      i Make sure you only select indices that match the tensor indices.

# sym/asym warnings

    Code
      sym(arr %_% .(i, j), i)
    Condition
      Warning in `sym()`:
      Symmetrization over a single index has no effect.
      i You might want to consider to remove this call.
    Output
      <Labeled Array> [2x2] .(-i, -j)
           [,1] [,2]
      [1,]    1    3
      [2,]    2    4

# `at` works correctly

    Code
      at(tensor("f(x)*y"), c(x = 0))
    Condition
      Error in `at()`:
      ! Not all symbols are specified.
      x Symbols `f` and `y` not defined.
      Caused by error in `f()`:
      ! could not find function "f"

