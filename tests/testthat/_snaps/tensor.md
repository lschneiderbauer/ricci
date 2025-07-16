# convert tensor to array errs if index does not fit

    Code
      as_a(arr %_% .(+i, j), j, i)
    Condition
      Error in `as_a()`:
      ! Argument `...` contains index with incorrect position.
      x Position of index `-i` does not match index position in <Labeled Array> [2x2] .(+i, -j).
      i Make sure you explicitely specify the correct index position.

---

    Code
      as_a(arr %_% .(+i, j), j)
    Condition
      Error in `as_a()`:
      ! Argument `...` does not contain all indices.
      x Index `i` missing.
      i Operation requires the explicit selection of all indices.

