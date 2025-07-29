# creating d works

    Code
      d(3)(i, j)
    Condition
      Error in `d()`:
      ! Argument `...` constains index with invalid position.
      x Index `j` is lowered while it should be raised.
      i The generalized Kronecker delta requires the first half of indices to be lowered, and the second half to be raised. If other index structures are required, lower/raise them explicitely using a metric tensor, e.g. via `?ricci::r()`, or `?ricci::l()`.

---

    Code
      d(3)(i, j, k)
    Condition
      Error:
      ! Wrong number of indices.
      x Number of indices: 3.
      i The Kronecker delta only accepts an even number of indices.

# creating e works

    Code
      e(+i, j)
    Condition
      Error in `e()`:
      ! Argument `...` constains index with invalid position.
      x Index `i` is raised while it should be lowered.
      i The Levi-Civita epsilon can only have lower indices. If raised indices are required, raise them explicitely using a metric tensor, e.g. via `?ricci::r()`.

