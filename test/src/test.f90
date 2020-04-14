program test
    implicit none
end program

function add(a, b) result(res)
    implicit none
    integer, intent(in)     :: a, b
    integer                 :: res

    res = a * a + b
end function
