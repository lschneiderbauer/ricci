# tensor diaginalization fails if dimensions do not agree

    Code
      arr %_% .(i, i)
    Condition
      Error in `arr %_% .(i, i)`:
      ! Attempted diagonal subsetting on slots with different dimensions.
      x Index `i` is associated to dimension 2 and 3.
      i Only assign identical index labels to slots with the equal dimension.

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

# creating a tensor errs when number index does not match

    Code
      arr %_% .(i, j, k)
    Condition
      Error in `arr %_% .(i, j, k)`:
      ! The number of provided indices do not match the array rank
      x 3 indices provided for a rank 2 array
      i The number of indices needs to match the array rank of argument `a`.

