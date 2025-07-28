# covd input validation works

    Code
      covd(arr %_% .(i, j), .(k), g = g_eucl_cart(2))
    Condition
      Error in `covd()`:
      ! Incorrect tensor dimension.
      x Indices i and j need to match the dimensions of the metric tensor.
      i Dimension of metric tensor: 2
      i Dimension of i and j: 3 and 3
        Do you need to use the argument `act_on`?

---

    Code
      covd(arr %_% .(i, j), .(k), g = g_eucl_cart(2), act_on = .(l))
    Condition
      Error in `covd()`:
      ! Argument `act_on` contains invalid index.
      x Index `l` not present in <Labeled Array> [3x3] .(-i, -j).
      i Make sure you only select indices that match the tensor indices.

