module test

    ! use c_interface_module

    implicit none

    ! external write !$PRAGMA C(write) 

    contains

    function add(a, b) result(res)
        integer, intent(in)             :: a, b
        integer                         :: res
! 
        ! type(C_ptr) :: buf

        ! buf = C_string_alloc(16_C_size_t)
        ! call F_C_string_ptr("Hallo Welt" // char(0), buf, 16)

        ! res = write(1, buf, 16)
        ! character(kind=C_CHAR), target :: buffer(16)
        ! integer(kind=c_intptr_t) :: ptr
        ! ptr = C_LOC(buffer)

        ! buffer = "hallo"
        ! res = write(1, ptr, 5)


        res = a * a + b
    end function

end module
