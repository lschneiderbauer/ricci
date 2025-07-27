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

