module test

    implicit none

    contains

    function add(a, b) result(res)
        integer, value, intent(in)      :: a, b
        integer                         :: res

        res = a
    end function

end module
