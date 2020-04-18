module test

    use iolib

    implicit none

    external :: ZGEEV

    contains

    function add(a, b) result(res)
        integer, value, intent(in)      :: a, b
        integer                         :: res

        real                            :: foo
        integer                         :: test

        foo = 5.0

        call iolib_write("Hello from Fortran!"//char(10))
        test = iolib_test(5)
        call iolib_printf("test int: 0x%x"//char(10), test)
        
        call iolib_printf("a float: %f"//char(10), 144523.4556)
        call iolib_printf("a double: %f"//char(10), 123.456_C_FLOAT64_T)
        call iolib_printf("a 64bit int: %lld"//char(10), 21474836471234567_C_INT64_T)

        res = a * b + int(sin(foo) * 12.5)
    end function

    function eigen() result(res)
        integer                         :: res
        ! Source: http://sites.science.oregonstate.edu/~landaur/nacphy/lapack/codes/eigen-f.html
        ! finding the eigenvalues of a complex matrix using LAPACK declarations, notice double precision
        complex*16 A(3,3), b(3), DUMMY(1,1), WORK(6)
        integer i, ok
        ! define matrix A
        A(1,1)=(3.1, -1.8)
        A(1,2)=(1.3, 0.2) 
        A(1,3)=(-5.7, -4.3)
        A(2,1)=(1.0, 0)
        A(2,2)=(-6.9, 3.2)
        A(2,3)=(5.8, 2.2)
        A(3,1)=(3.4, -4.0)
        A(3,2)=(7.2, 2.9)
        A(3,3)=(-8.8, 3.2)
        
        ! find the solution using the LAPACK routine ZGEEV
        call ZGEEV('N', 'N', 3, A, 3, b, DUMMY, 1, DUMMY, 1, WORK, 6, WORK, ok)
        
        ! parameters in the order as they appear in the function call
        !    no left eigenvectors, no right eigenvectors, order of input matrix A,
        !    input matrix A, leading dimension of A, array for eigenvalues, 
        !    array for left eigenvalue, leading dimension of DUMMY, 
        !    array for right eigenvalues, leading dimension of DUMMY,
        !    workspace array dim>=2*order of A, dimension of WORK
        !    workspace array dim=2*order of A, return value 
        
        ! output of eigenvalues
        if (ok .eq. 0) then
            do i=1, 3
                call iolib_printf("(%f, ", realpart(b(i)))
                call iolib_printf("%f)"//char(10), imagpart(b(i)))
            enddo
        else
            call iolib_write("An error occured"//char(10))
        endif
        res = 0
    end function

end module