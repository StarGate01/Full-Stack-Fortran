module test

    use, intrinsic :: iso_fortran_env

    implicit none

    contains

    function add(a, b) result(res)
        integer(INT32), intent(in)     :: a, b
        integer(INT32)                 :: res

        res = a * a + b
    end function

end module
