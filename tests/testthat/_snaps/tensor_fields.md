# creating g_mink works

    Code
      g_mink_cart(2) %_% .(i, +j)
    Condition
      Error in `g_mink_cart(2) %_% .(i, +j)`:
      ! The metric tensor indices cannot be mixed
      i Indices either must be all lowered or all raised.

# creating g_eucl works

    Code
      g_eucl_cart(2) %_% .(i, +j)
    Condition
      Error in `g_eucl_cart(2) %_% .(i, +j)`:
      ! The metric tensor indices cannot be mixed
      i Indices either must be all lowered or all raised.

# creating g_ss works

    Code
      g_ss(3)
    Output
      <Covariant metric tensor field> (t, r, ph1)
           [,1]       [,2]        [,3]   
      [1,] "-(1-1/r)" "0"         "0"    
      [2,] "0"        "1/(1-1/r)" "0"    
      [3,] "0"        "0"         "r^2*1"

