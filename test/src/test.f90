module test

    use, intrinsic :: iso_c_binding

    implicit none

    !include "src/lib/externals.f90"

    integer, external :: f90_test

    contains

    function add(a, b) result(res)
        integer, value, intent(in)      :: a, b
        integer                         :: res

        write(*,*) "Hello World!"
        res = a * a + b + f90_test(a)
    end function

end module
