# validation works

    Code
      metric_field(array("a", c(1, 1)), array("1/a", c(1, 1)), "b")
    Condition
      Error in `metric_field()`:
      ! Expression in `metric` is not solely a function of coordinates sepcified in `coords`.
      i The expression in `metric` can only depend on coordinates, not on other parameters. When evaluted at a point (specified by coorindates) the expression needs to yield a number.
      Caused by error:
      ! object 'a' not found

---

    Code
      metric_field(array("a", c(1, 1)), array(c("1/a", "b"), c(1, 2)), "a")
    Condition
      Error in `metric_field()`:
      ! Argument `metric_inv` is not a valid array.
      i A quadratic (n by n) matrix/array is required.

---

    Code
      metric_field(array("a", c(1, 1)), array(1:4, c(2, 2)), "a")
    Condition
      Error in `metric_field()`:
      ! Matrix dimensions of `metric` does not match matrix dimensions of `metric_inv`.
      i `metric_inv` needs to be the inverse matrix of `metric`.

---

    Code
      metric_field(array("a(", c(1, 1)), array("a", c(1, 1)), "a")
    Condition
      Error in `metric_field()`:
      ! Invalid expression.
      i Argument `metric` needs to contain valid R expressions.
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: a(
         ^

---

    Code
      metric_field(array(1:6, c(2, 3)), array(1:6, c(2, 3)), "a")
    Condition
      Error in `metric_field()`:
      ! Argument `metric` is not a valid array.
      i A quadratic (n by n) matrix/array is required.

---

    Code
      metric_field(array("a(", c(1, 1)), array("a", c(1, 1)), 3)
    Condition
      Error in `metric_field()`:
      ! Argument `coords` is no character vector.

---

    Code
      metric_field(array("a(", c(1, 1)), array("a", c(1, 1)), c("a", "b"))
    Condition
      Error in `metric_field()`:
      ! Dimension and length of `metric` and `coords` are incompatible.
      i The length of `coords` needs to match the array dimension of `metric`.

