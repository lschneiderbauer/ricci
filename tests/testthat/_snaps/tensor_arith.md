# tensor multiplication yields correct errors

    Code
      a %_% .(i) * b %_% .(+i)
    Condition
      Error in `` `*`() ``:
      ! Tensor index dimensions do not agree.
      x Tensor index `i` has dimension 4 in `e1`.
      x Tensor index `i` has dimension 3 in `e2`.
      i Operation can only be carried out with two tensors having identical index dimensions.

# tensor addition works

    Code
      arr %_% .(i, j) + arr %_% .(k, l)
    Condition
      Error in `` `+`() ``:
      ! Tensor indices do not agree.
      x Tensor index `i` and `j` appear in `e1` but not in `e2`.
      x Tensor index `k` and `l` appear in `e2` but not in `e1`.
      i Operation can only be carried out with two tensors having identical indices.

---

    Code
      arr %_% .(i, j) + arr2 %_% .(i)
    Condition
      Error in `` `+`() ``:
      ! Tensor ranks do not agree.
      x Tensor rank of `e1` is 2.
      x Tensor rank of `e2` is 1.
      i Operation can only be carried out with two tensors of same rank.

---

    Code
      arr %_% .(i, j) + arr %_% .(+i, j)
    Condition
      Error in `` `+`() ``:
      ! Tensor index positions do not agree.
      x Tensor index `i` appears with different positions in `e1` and `e2`.
      i Operation can only be carried out with two tensors having identical index positions.

