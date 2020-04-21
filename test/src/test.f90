module test

    use iolib

    implicit none

    contains

    function add(a, b) result(res)
        integer, value, intent(in)      :: a, b
        integer                         :: res

        write(*,*) "Hello from Fortran!"
        call iolib_write("Hello from Fortran via libc!"//char(10))

        res = a * b + iolib_test(a)
    end function

end module
