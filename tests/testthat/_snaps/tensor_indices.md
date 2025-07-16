# .() errs correctly

    Code
      array(1) %_% "test"
    Condition
      Error in `array(1) %_% "test"`:
      ! Second argument of `%_%()` is not an index specification.
      i Indices need to be specified with `ricci::.()`.

---

    Code
      .(f(y))
    Condition
      Error in `.()`:
      ! Invalid expression in `...`.
      x Expression `f(y)` cannot be parsed.
      i A valid expressions is of the form {[+|-]<label1>, [+|-]<label2>, ...}

