module cbind
    use iso_c_binding, only: C_CHAR, C_NULL_CHAR

    interface

    subroutine f90_mkdir(string) bind(C, name="my_mkdir")
        use iso_c_binding, only: c_char
        character(kind=c_char) :: string(*)

    end subroutine f90_mkdir

    end interface

end module