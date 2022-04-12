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

    subroutine canary_dotest_f90() bind(c, name="canary_dotest_c")
        integer, parameter :: N=2

        complex*16, parameter :: imag1 = cmplx(0.d0, 1.d0)
        complex*16 :: a(N,N), x(N), y(N)
      
        complex*16 :: alpha, beta
      
        write(*,*) "Testing complex BLAS function zgemv"

        a(:,:)=imag1;
        x(:)=1.d0
        y(:)=0.d0
      
        alpha=1.d0; beta=0.d0
      
        call zgemv('N',N,N,alpha,a,N,x,1,beta,y,1)
      
        print*, y
    end subroutine

end module
