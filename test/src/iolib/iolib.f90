module iolib

    use, intrinsic :: iso_c_binding

    implicit none

    interface
        integer(C_INT32_T) function iolib_write_c(fd, buf, count) bind(C, name='write')
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT32_T), value, intent(in) :: fd, count
            character(kind=c_char), intent(in) :: buf
        end function

        integer(C_INT32_T) function iolib_printf_c(format, ptr0) bind(C, name='printf')
            use, intrinsic :: iso_c_binding
            implicit none
            character(kind=c_char), intent(in) :: format
            type(c_ptr), intent(in) :: ptr0
        end function

        integer(C_INT32_T) function iolib_test(var) bind(C, name='iolib_test')
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT32_T), value, intent(in) :: var
        end function
    end interface

    interface iolib_printf
        module procedure iolib_printf_int_8
        module procedure iolib_printf_int_16
        module procedure iolib_printf_int_32
        module procedure iolib_printf_int_64
        module procedure iolib_printf_real_32
        module procedure iolib_printf_real_64
    end interface

    contains

    subroutine iolib_write(string)
        character(*), intent(in), target :: string
        integer(C_INT32_T) :: res

        res = iolib_write_c(1_C_INT32_T, string, int(len(string), C_INT32_T))
    end subroutine

    subroutine iolib_printf_int_8(format, var)
        character(*), intent(in), target :: format
        integer(C_INT8_T), intent(in), target :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(format, c_loc(var))
    end subroutine

    subroutine iolib_printf_int_16(format, var)
        character(*), intent(in), target :: format
        integer(C_INT16_T), intent(in), target :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(format, c_loc(var))
    end subroutine

    subroutine iolib_printf_int_32(format, var)
        character(*), intent(in) , target:: format
        integer(C_INT32_T), intent(in), target :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(format, c_loc(var))
    end subroutine

    subroutine iolib_printf_int_64(format, var)
        character(*), intent(in), target :: format
        integer(C_INT64_T), intent(in), target :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(format, c_loc(var))
    end subroutine

    subroutine iolib_printf_real_32(format, var)
        character(*), intent(in), target :: format
        real(c_float), intent(in), target :: var
        integer(C_INT32_T) :: res
        real(c_double), target :: var_conv

        var_conv = real(var, c_double)
        res = iolib_printf_c(format, c_loc(var_conv))
    end subroutine

    subroutine iolib_printf_real_64(format, var)
        character(*), intent(in), target :: format
        real(c_double), intent(in), target :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(format, c_loc(var))
    end subroutine

end module