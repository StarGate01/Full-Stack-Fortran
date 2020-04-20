module test

    use iolib
    use lapack_prb
    use test_eigen
    use, intrinsic :: iso_c_binding

    implicit none

    contains

    function add(a, b) result(res)
        integer, value, intent(in)      :: a, b
        integer                         :: res

        real                            :: foo
        integer                         :: test

        ! real, dimension(100) :: x, y  
        ! ! real, dimension(100) :: p, q
        ! integer :: i, fpid
        
        ! fpid = 42

        ! ! data  
        ! do i=1,100  
        !    x(i) = i * 0.1 
        !    y(i) = sin(x(i)) * (1-cos(x(i)/3.0))  
        ! end do  
        
        ! write(*,*) "before write"

        ! ! output data into a file 
        ! open(fpid, file = 'data1.dat', status = 'new')  
        ! do i=1,100  
        !    write(fpid,*) x(i), y(i)   
        ! end do  
        
        ! close(fpid) 

        ! write(*,*) "after write"
        ! write(*,*) "hallo welt tada", 12, (/ 12.5, 14.5 /)

        ! call iolib_write("Hello from Fortran!"//char(10))
        ! test = iolib_test(5)
        ! call iolib_printf("test int: 0x%x"//char(10), test)
        
        ! call iolib_printf("a float: %f"//char(10), 144523.4556)
        ! call iolib_printf("a double: %f"//char(10), 123.456)
        ! call iolib_printf("a 64bit int: %lld"//char(10), 21474836471234567_c_int64_t)
        call test_eigen_main()
        call lapack_prb_main()

        res = a * b + int(sin(foo) * 12.5)
    end function

    ! function eigen() result(res)
    !     integer                         :: res
    !     ! Source: http://sites.science.oregonstate.edu/~landaur/nacphy/lapack/codes/eigen-f.html
    !     ! finding the eigenvalues of a complex matrix using LAPACK declarations, notice double precision
    !     ! dont notice it tho cus wasm32 is shite m9
    !     complex*8 A(3,3), b(3), DUMMY(1,1), WORK(6)
    !     integer i, ok
    !     ! define matrix A
    !     A(1,1)=(3.1, -1.8)
    !     A(1,2)=(1.3, 0.2) 
    !     A(1,3)=(-5.7, -4.3)
    !     A(2,1)=(1.0, 0)
    !     A(2,2)=(-6.9, 3.2)
    !     A(2,3)=(5.8, 2.2)
    !     A(3,1)=(3.4, -4.0)
    !     A(3,2)=(7.2, 2.9)
    !     A(3,3)=(-8.8, 3.2)
        
    !     ! find the solution using the LAPACK routine ZGEEV
    !     call CGEEV('N', 'N', 3, A, 3, b, DUMMY, 1, DUMMY, 1, WORK, 6, WORK, ok)
        
    !     ! parameters in the order as they appear in the function call
    !     !    no left eigenvectors, no right eigenvectors, order of input matrix A,
    !     !    input matrix A, leading dimension of A, array for eigenvalues, 
    !     !    array for left eigenvalue, leading dimension of DUMMY, 
    !     !    array for right eigenvalues, leading dimension of DUMMY,
    !     !    workspace array dim>=2*order of A, dimension of WORK
    !     !    workspace array dim=2*order of A, return value 
        
    !     ! output of eigenvalues
    !     if (ok .eq. 0) then
    !         do i=1, 3
    !             write(*,*) b(i)
    !         enddo
    !     else
    !         write (*,*) "An error occured"
    !     endif
    !     res = 0
    ! end function

end module