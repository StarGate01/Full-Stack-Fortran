module test

    use cbind

    implicit none

    contains

    function add(a, b) result(res)
        integer, value, intent(in)      :: a, b
        integer                         :: res

        call f90_mkdir("test")
        res = a * a + b 
    end function

end module
