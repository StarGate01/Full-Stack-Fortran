module blas3_z_prb
      use blas0
      contains
    subroutine blas3_z_prb_main

!*****************************************************************************80
!
!! MAIN is the main program for BLAS3_Z_TEST.
!
!  Discussion:
!
!    BLAS3_Z_TEST tests the BLAS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 January 2014
!
!  Author:
!
!    John Burkardt
!
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS3_Z_TEST'
      write ( *, '(a)' ) '  FORTRAN90 version'
      write ( *, '(a)' ) '  Test the BLAS library.'
    
      call test01 ( )
    !
    !  Terminate.
    !
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS3_Z_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )
    
      stop 0
    end
    subroutine test01 ( )
    
    !*****************************************************************************80
    !
    !! TEST01 demonstrates the use of ZGEMM.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    03 April 2014
    !
    !  Author:
    !
    !    John Burkardt
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: k = 3
      integer ( kind = 4 ), parameter :: m = 3
      integer ( kind = 4 ), parameter :: n = 3
    
      integer ( kind = 4 ), parameter :: lda = m
      integer ( kind = 4 ), parameter :: ldb = k
      integer ( kind = 4 ), parameter :: ldc = m
    
      complex ( kind = 8 ) a(lda,k)
      complex ( kind = 8 ) alpha
      complex ( kind = 8 ) b(ldb,n)
      complex ( kind = 8 ) beta
      complex ( kind = 8 ) c(ldc,n)
      character :: transa = 'N'
      character :: transb = 'N'
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  ZGEMM can combine scale, multiply and add matrices'
      write ( *, '(a)' ) '  using double precision complex arithmetic.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Here, we simply compute C = A * B.'
      write ( *, '(a)' ) '  Because B is inverse ( A ), C should be the identity.'
     
      call c8mat_test ( m, a )
      call c8mat_print ( m, m, a, '  Matrix A:' )
    
      call c8mat_test_inverse ( m, b )
      call c8mat_print ( m, m, b, '  Matrix B:' )
    
      alpha = ( 1.0D+00, 0.0D+00 )
      beta = ( 0.0D+00, 0.0D+00 )
    
      call zgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )
    
      call c8mat_print ( m, n, c, '  Product C = A * B:' )
    
      return
    end
      end module