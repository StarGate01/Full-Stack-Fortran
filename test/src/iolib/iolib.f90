module iolib

    use iolib_types

    implicit none

    interface
        integer(C_INT32_T) function iolib_write_c(fd, buf, count) bind(C, name='write')
            use iolib_types
            implicit none
            integer(C_INT32_T), value, intent(in) :: fd, count
            integer(C_PTR_T), value, intent(in) :: buf
        end function

        integer(C_INT32_T) function iolib_printf_c(format, ptr0) bind(C, name='printf')
            use iolib_types
            implicit none
            integer(C_PTR_T), value, intent(in) :: format, ptr0
        end function

        integer(C_INT32_T) function iolib_test(var) bind(C, name='iolib_test')
            use iolib_types
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
        character(*), intent(in) :: string
        integer(C_INT32_T) :: res

        res = iolib_write_c(1_C_INT32_T, loc(string), int(len(string), C_INT32_T))
    end subroutine

    subroutine iolib_printf_int_8(format, var)
        character(*), intent(in) :: format
        integer(C_INT8_T), intent(in) :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(loc(format), loc(var))
    end subroutine

    subroutine iolib_printf_int_16(format, var)
        character(*), intent(in) :: format
        integer(C_INT16_T), intent(in) :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(loc(format), loc(var))
    end subroutine

    subroutine iolib_printf_int_32(format, var)
        character(*), intent(in) :: format
        integer(C_INT32_T), intent(in) :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(loc(format), loc(var))
    end subroutine

    subroutine iolib_printf_int_64(format, var)
        character(*), intent(in) :: format
        integer(C_INT64_T), intent(in) :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(loc(format), loc(var))
    end subroutine

    subroutine iolib_printf_real_32(format, var)
        character(*), intent(in) :: format
        real(C_FLOAT32_T), intent(in) :: var
        integer(C_INT32_T) :: res
        real(C_FLOAT64_T) :: var_conv

        var_conv = real(var, C_FLOAT64_T)
        res = iolib_printf_c(loc(format), loc(var_conv))
    end subroutine

    subroutine iolib_printf_real_64(format, var)
        character(*), intent(in) :: format
        real(C_FLOAT64_T), intent(in) :: var
        integer(C_INT32_T) :: res

        res = iolib_printf_c(loc(format), loc(var))
    end subroutine

end module