function add(a, b) result(res)
    integer, intent(in)     :: a, b
    integer                 :: res

    res = a * a + b
end function