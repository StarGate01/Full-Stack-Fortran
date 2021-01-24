module blas0
    implicit none
    contains
function c4_uniform_01 ( seed )

    !*****************************************************************************80
    !
    !! C4_UNIFORM_01 returns a unit pseudorandom C4.
    !
    !  Discussion:
    !
    !    A C4 is a complex ( kind = 4 ) value.
    !
    !    The angle should be uniformly distributed between 0 and 2 * PI,
    !    the square root of the radius uniformly distributed between 0 and 1.
    !
    !    This results in a uniform distribution of values in the unit circle.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    13 December 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
    !    should NOT be 0.  On output, SEED has been updated.
    !
    !    Output, complex ( kind = 4 ) C4_UNIFORM_01, a pseudorandom complex value.
    !
      implicit none
    
      complex ( kind = 4 ) c4_uniform_01
      integer ( kind = 4 ), parameter :: i4_huge = 2147483647
      integer ( kind = 4 ) k
      real ( kind = 4 ) r
      real ( kind = 4 ), parameter :: r4_pi = 3.141592653589793E+00
      integer ( kind = 4 ) seed
      real ( kind = 4 ) theta
    
      k = seed / 127773
    
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if
    
      r = sqrt ( real ( seed, kind = 4 ) * 4.656612875E-10 )
    
      k = seed / 127773
    
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if
    
      theta = 2.0E+00 * r4_pi * ( real ( seed, kind = 4 ) * 4.656612875E-10 )
    
      c4_uniform_01 = r * cmplx ( cos ( theta ), sin ( theta ), kind = 4 )
    
      return
    end
    subroutine c4mat_print ( m, n, a, title )
    
    !*****************************************************************************80
    !
    !! C4MAT_PRINT prints a C4MAT.
    !
    !  Discussion:
    !
    !    A C4MAT is a matrix of C4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    13 December 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns 
    !    in the matrix.
    !
    !    Input, complex ( kind = 4 ) A(M,N), the matrix.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      complex ( kind = 4 ) a(m,n)
      character ( len = * ) title
    
      call c4mat_print_some ( m, n, a, 1, 1, m, n, title )
    
      return
    end
    subroutine c4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )
    
    !*****************************************************************************80
    !
    !! C4MAT_PRINT_SOME prints some of a C4MAT.
    !
    !  Discussion:
    !
    !    A C4MAT is a matrix of C4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    21 June 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns 
    !    in the matrix.
    !
    !    Input, complex ( kind = 4 ) A(M,N), the matrix.
    !
    !    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, the first row and
    !    column, and the last row and column to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: incx = 4
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      complex ( kind = 4 ) a(m,n)
      character ( len = 20 ) ctemp(incx)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2hi
      integer ( kind = 4 ) i2lo
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) inc
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) j2hi
      integer ( kind = 4 ) j2lo
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      character ( len = * ) title
      complex ( kind = 4 ) zero
    
      zero = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
    
      if ( m <= 0 .or. n <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)' 
        return
      end if
    !
    !  Print the columns of the matrix, in strips of INCX.
    !
      do j2lo = jlo, min ( jhi, n ), incx
    
        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )
    
        inc = j2hi + 1 - j2lo
    
        write ( *, '(a)' ) ' '
    
        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i10,10x)' ) j
        end do
    
        write ( *, '(a,4a20)' ) '  Col: ', ( ctemp(j2), j2 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
    !
    !  Determine the range of the rows in this strip.
    !
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )
    
        do i = i2lo, i2hi
    !
    !  Print out (up to) INCX entries in row I, that lie in the current strip.
    !
          do j2 = 1, inc
    
            j = j2lo - 1 + j2
    
            if ( imag ( a(i,j) ) == 0.0E+00 ) then
              write ( ctemp(j2), '(g10.3,10x)' ) real ( a(i,j), kind = 4 )
            else
              write ( ctemp(j2), '(2g10.3)' ) a(i,j)
            end if
    
          end do
    
          write ( *, '(i5,a1,4a20)' ) i, ':', ( ctemp(j2), j2 = 1, inc )
    
        end do
    
      end do
    
      return
    end
    subroutine c4mat_test ( n, a )
    
    !*****************************************************************************80
    !
    !! C4MAT_TEST sets up a test matrix.
    !
    !  Formula:
    !
    !    A(I,J) = exp ( 2 * PI * sqrt ( -1 ) * (I-1) * (J-1) / N ) / sqrt ( N )
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
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of the matrix.
    !
    !    Output, complex ( kind = 4 ) A(N,N), the matrix.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      complex ( kind = 4 ) a(n,n)
      real ( kind = 4 ) angle
      complex ( kind = 4 ), parameter :: c4_i = cmplx ( 0.0D+00, 1.0D+00, kind = 4 )
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      real ( kind = 4 ), parameter :: pi = 3.141592653589793E+00
    
      do i = 1, n
        do j = 1, n
    
          angle = 2.0E+00 * pi * real ( ( i - 1 ) * ( j - 1 ), kind = 4 ) &
            / real ( n, kind = 4 )
    
          a(i,j) = exp ( c4_i * angle )
    
        end do
      end do
    
      a(1:n,1:n) = a(1:n,1:n) / sqrt ( real ( n, kind = 4 ) )
    
      return
    end
    subroutine c4mat_test_inverse ( n, a )
    
    !*****************************************************************************80
    !
    !! C4MAT_TEST_INVERSE returns the inverse of the test matrix.
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
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of the matrix.
    !
    !    Output, complex ( kind = 4 ) A(N,N), the matrix.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      complex ( kind = 4 ) a(n,n)
    
      call c4mat_test ( n, a )
    
      a(1:n,1:n) = conjg ( transpose ( a(1:n,1:n) ) )
    
      return
    end
    function c8_uniform_01 ( seed )
    
    !*****************************************************************************80
    !
    !! C8_UNIFORM_01 returns a unit pseudorandom C8.
    !
    !  Discussion:
    !
    !    A C8 is a complex ( kind = 8 ) value.
    !
    !    The angle should be uniformly distributed between 0 and 2 * PI,
    !    the square root of the radius uniformly distributed between 0 and 1.
    !
    !    This results in a uniform distribution of values in the unit circle.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    15 March 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
    !    should NOT be 0.  On output, SEED has been updated.
    !
    !    Output, complex ( kind = 8 ) C8_UNIFORM_01, a pseudorandom complex value.
    !
      implicit none
    
      complex ( kind = 8 ) c8_uniform_01
      integer ( kind = 4 ), parameter :: i4_huge = 2147483647
      integer ( kind = 4 ) k
      real ( kind = 8 ) r
      real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
      integer ( kind = 4 ) seed
      real ( kind = 8 ) theta
    
      k = seed / 127773
    
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if
    
      r = sqrt ( real ( seed, kind = 8 ) * 4.656612875D-10 )
    
      k = seed / 127773
    
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if
    
      theta = 2.0D+00 * r8_pi * ( real ( seed, kind = 8 ) * 4.656612875D-10 )
    
      c8_uniform_01 = r * cmplx ( cos ( theta ), sin ( theta ), kind = 8 )
    
      return
    end
    subroutine c8mat_print ( m, n, a, title )
    
    !*****************************************************************************80
    !
    !! C8MAT_PRINT prints a C8MAT.
    !
    !  Discussion:
    !
    !    A C8MAT is a matrix of C8's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    23 March 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns 
    !    in the matrix.
    !
    !    Input, complex ( kind = 8 ) A(M,N), the matrix.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer   ( kind = 4 ) m
      integer   ( kind = 4 ) n
    
      complex ( kind = 8 ) a(m,n)
      character ( len = * ) title
    
      call c8mat_print_some ( m, n, a, 1, 1, m, n, title )
    
      return
    end
    subroutine c8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )
    
    !*****************************************************************************80
    !
    !! C8MAT_PRINT_SOME prints some of a C8MAT.
    !
    !  Discussion:
    !
    !    A C8MAT is a matrix of C8's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    23 March 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns 
    !    in the matrix.
    !
    !    Input, complex ( kind = 8 ) A(M,N), the matrix.
    !
    !    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, the first row and
    !    column, and the last row and column to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: incx = 4
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      complex ( kind = 8 ) a(m,n)
      character ( len = 20 ) ctemp(incx)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2hi
      integer ( kind = 4 ) i2lo
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) inc
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) j2hi
      integer ( kind = 4 ) j2lo
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      character ( len = * ) title
      complex ( kind = 8 ) zero
    
      zero = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
    
      if ( m <= 0 .or. n <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)' 
        return
      end if
    !
    !  Print the columns of the matrix, in strips of INCX.
    !
      do j2lo = jlo, min ( jhi, n ), incx
    
        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )
    
        inc = j2hi + 1 - j2lo
    
        write ( *, '(a)' ) ' '
    
        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i10,10x)' ) j
        end do
    
        write ( *, '(a,4a20)' ) '  Col: ', ( ctemp(j2), j2 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
    !
    !  Determine the range of the rows in this strip.
    !
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )
    
        do i = i2lo, i2hi
    !
    !  Print out (up to) INCX entries in row I, that lie in the current strip.
    !
          do j2 = 1, inc
    
            j = j2lo - 1 + j2
    
            if ( imag ( a(i,j) ) == 0.0D+00 ) then
              write ( ctemp(j2), '(g10.3,10x)' ) real ( a(i,j), kind = 8 )
            else
              write ( ctemp(j2), '(2g10.3)' ) a(i,j)
            end if
    
          end do
    
          write ( *, '(i5,a1,4a20)' ) i, ':', ( ctemp(j2), j2 = 1, inc )
    
        end do
    
      end do
    
      return
    end
    subroutine c8mat_test ( n, a )
    
    !*****************************************************************************80
    !
    !! C8MAT_TEST sets up a test matrix.
    !
    !  Formula:
    !
    !    A(I,J) = exp ( 2 * PI * sqrt ( -1 ) * (I-1) * (J-1) / N ) / sqrt ( N )
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
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of the matrix.
    !
    !    Output, complex ( kind = 8 ) A(N,N), the matrix.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      complex ( kind = 8 ) a(n,n)
      real ( kind = 8 ) angle
      complex ( kind = 8 ), parameter :: c8_i = cmplx ( 0.0D+00, 1.0D+00, kind = 8 )
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
    
      do i = 1, n
        do j = 1, n
    
          angle = 2.0D+00 * pi * real ( ( i - 1 ) * ( j - 1 ), kind = 8 ) &
            / real ( n, kind = 8 )
    
          a(i,j) = exp ( c8_i * angle )
    
        end do
      end do
    
      a(1:n,1:n) = a(1:n,1:n) / sqrt ( real ( n, kind = 8 ) )
    
      return
    end
    subroutine c8mat_test_inverse ( n, a )
    
    !*****************************************************************************80
    !
    !! C8MAT_TEST_INVERSE returns the inverse of the test matrix.
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
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of the matrix.
    !
    !    Output, complex ( kind = 8 ) A(N,N), the matrix.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      complex ( kind = 8 ) a(n,n)
    
      call c8mat_test ( n, a )
    
      a(1:n,1:n) = conjg ( transpose ( a(1:n,1:n) ) )
    
      return
    end
    function cabs1 ( z )
    
    !*****************************************************************************80
    !
    !! CABS1 returns the L1 norm of a single precision complex number.
    !
    !  Discussion:
    !
    !    The L1 norm of a complex number is the sum of the absolute values
    !    of the real and imaginary components.
    !
    !    CABS1 ( Z ) = abs ( real ( Z ) ) + abs ( imaginary ( Z ) )
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    22 May 2002
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Basic Linear Algebra Subprograms for FORTRAN usage,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, pages 308-323, 1979.
    !
    !  Parameters:
    !
    !    Input, complex ( kind = 4 ) Z, the number whose norm is desired.
    !
    !    Output, real ( kind = 4 ) CABS1, the L1 norm of Z.
    !
      implicit none
    
      real ( kind = 4 ) cabs1
      complex ( kind = 4 ) z
    
      cabs1 = abs ( real ( z ) ) + abs ( aimag ( z ) )
    
      return
    end
    function cabs2 ( z )
    
    !*****************************************************************************80
    !
    !! CABS2 returns the L2 norm of a single precision complex number.
    !
    !  Discussion:
    !
    !    The L2 norm of a complex number is the square root of the sum
    !    of the squares of the real and imaginary components.
    !
    !    CABS2 ( Z ) = sqrt ( ( real ( Z ) ^ 2 + ( imaginary ( Z ) ) ^ 2 )
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    19 March 2006
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Basic Linear Algebra Subprograms for FORTRAN usage,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, pages 308-323, 1979.
    !
    !  Parameters:
    !
    !    Input, complex ( kind = 4 ) Z, the number whose norm is desired.
    !
    !    Output, real ( kind = 4 ) CABS2, the L2 norm of Z.
    !
      implicit none
    
      real ( kind = 4 ) cabs2
      complex ( kind = 4 ) z
    
      cabs2 = sqrt ( ( real ( z ) )**2 + ( aimag ( z ) )**2 )
    
      return
    end
    function cmach ( job )
    
    !*****************************************************************************80
    !
    !! CMACH computes machine parameters for single precision complex arithmetic.
    !
    !  Discussion:
    !
    !    Assume the computer has
    !
    !      B = base of arithmetic;
    !      T = number of base B digits;
    !      L = smallest possible exponent;
    !      U = largest possible exponent;
    !
    !    then
    !
    !      EPS = B^(1-T)
    !      TINY = 100.0 * B^(-L+T)
    !      HUGE = 0.01 * B^(U-T)
    !
    !    If complex division is done by
    !
    !      1 / (X+i*Y) = (X-i*Y) / (X^2+Y^2)
    !
    !    then
    !
    !      TINY = sqrt ( TINY )
    !      HUGE = sqrt ( HUGE )
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    23 June 2009
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Basic Linear Algebra Subprograms for FORTRAN usage,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, pages 308-323, 1979.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) JOB:
    !    1, EPS is desired;
    !    2, TINY is desired;
    !    3, HUGE is desired.
    !
    !    Output, real ( kind = 4 ) CMACH, the requested value.
    !
      implicit none
    
      real ( kind = 4 ) cmach
      integer ( kind = 4 ) job
      real ( kind = 4 ), parameter :: t = 1.0E+00
    
      if ( job == 1 ) then
        cmach = epsilon ( t )
      else if ( job == 2 ) then
        cmach = tiny ( t )
      else if ( job == 3 ) then
        cmach = huge ( t )
      else
        cmach = 0.0E+00
      end if
    
      return
    end
    function csign1 ( z1, z2 )
    
    !*****************************************************************************80
    !
    !! CSIGN1 is a single precision complex transfer-of-sign function.
    !
    !  Discussion:
    !
    !    The L1 norm is used.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    14 May 2004
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Parameters:
    !
    !    Input, complex ( kind = 4 ) Z1, Z2, the arguments.
    !
    !    Output, complex ( kind = 4 ) CSIGN1,  a complex value, with the magnitude
    !    of Z1, and the argument of Z2.
    !
      implicit none
    
      real ( kind = 4 ) cabs1
      complex ( kind = 4 ) csign1
      complex ( kind = 4 ) z1
      complex ( kind = 4 ) z2
    
      if ( cabs1 ( z2 ) == 0.0E+00 ) then
        csign1 = cmplx ( 0.0E+00, 0.0E+00 )
      else
        csign1 = cabs1 ( z1 ) * ( z2 / cabs1 ( z2 ) )
      end if
    
      return
    end
    function csign2 ( z1, z2 )
    
    !*****************************************************************************80
    !
    !! CSIGN2 is a single precision complex transfer-of-sign function.
    !
    !  Discussion:
    !
    !    The L2 norm is used.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    19 March 2006
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Parameters:
    !
    !    Input, complex ( kind = 4 ) Z1, Z2, the arguments.
    !
    !    Output, complex ( kind = 4 ) CSIGN2,  a complex value, with the magnitude
    !    of Z1, and the argument of Z2.
    !
      implicit none
    
      real ( kind = 4 ) cabs2
      complex ( kind = 4 ) csign2
      complex ( kind = 4 ) z1
      complex ( kind = 4 ) z2
    
      if ( cabs2 ( z2 ) == 0.0E+00 ) then
        csign2 = cmplx ( 0.0E+00, 0.0E+00 )
      else
        csign2 = cabs2 ( z1 ) * ( z2 / cabs2 ( z2 ) )
      end if
    
      return
    end
    function dmach ( job )
    
    !*****************************************************************************80
    !
    !! DMACH computes machine parameters for double precision arithmetic.
    !
    !  Discussion:
    !
    !    Assume the computer has
    !
    !      B = base of arithmetic;
    !      T = number of base B digits;
    !      L = smallest possible exponent;
    !      U = largest possible exponent;
    !
    !    then
    !
    !      EPS = B^(1-T)
    !      TINY = 100.0 * B^(-L+T)
    !      HUGE = 0.01 * B^(U-T)
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    31 May 2020
    !
    !  Author:
    !
    !    Original FORTRAN77 version by Charles Lawson, Richard Hanson, 
    !    David Kincaid, Fred Krogh.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Algorithm 539, 
    !    Basic Linear Algebra Subprograms for Fortran Usage,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, pages 308-323, 1979.
    !
    !  Input:
    !
    !    integer ( kind = 4 ) JOB:
    !    1, EPS is desired;
    !    2, TINY is desired;
    !    3, HUGE is desired.
    !
    !  Output:
    !
    !    real ( kind = 8 ) DMACH, the requested value.
    !
      implicit none
    
      real ( kind = 8 ) dmach
      integer ( kind = 4 ) job
      real ( kind = 8 ), parameter :: t = 1.0D+00
    
      if ( job == 1 ) then
        dmach = epsilon ( t )
      else if ( job == 2 ) then
        dmach = tiny ( t )
      else if ( job == 3 ) then
        dmach = huge ( t )
      else
        dmach = 0.0D+00
      end if
    
      return
    end
    function lsame ( ca, cb )
    
    !*****************************************************************************80
    !
    !! LSAME returns TRUE if CA is the same letter as CB regardless of case.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    16 May 2005
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Basic Linear Algebra Subprograms for Fortran Usage,
    !    Algorithm 539,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, September 1979, pages 308-323.
    !
    !  Parameters:
    !
    !    Input, character CA, CB, the character to compare.
    !
    !    Output, logical LSAME, is TRUE if the characters are equal,
    !    disregarding case.
    !
      implicit none
    
      character ca
      character cb
      integer ( kind = 4 ) inta
      integer ( kind = 4 ) intb
      logical lsame
      integer ( kind = 4 ) zcode
    !
    !  Test if the characters are equal
    !
      lsame = ( ca == cb )
    
      if ( lsame ) then
        return
      end if
    !
    !  Now test for equivalence if both characters are alphabetic.
    !
      zcode = ichar ( 'Z' )
    !
    !  Use 'Z' rather than 'A' so that ASCII can be detected on Prime
    !  machines, on which ICHAR returns a value with bit 8 set.
    !  ICHAR('A') on Prime machines returns 193 which is the same as
    !  ICHAR('A') on an EBCDIC machine.
    !
      inta = ichar ( ca )
      intb = ichar ( cb )
    
      if ( zcode == 90 .or. zcode == 122 ) then
    !
    !  ASCII is assumed - zcode is the ASCII code of either lower or
    !  upper case 'Z'.
    !
        if ( 97 <= inta .and. inta <= 122 ) then
          inta = inta - 32
        end if
    
        if ( 97 <= intb .and. intb <= 122 ) then
          intb = intb - 32
        end if
    
      else if ( zcode == 233 .or. zcode == 169 ) then
    !
    !  EBCDIC is assumed - zcode is the EBCDIC code of either lower or
    !  upper case 'Z'.
    !
        if ( 129 <= inta .and. inta <= 137 .or. &
             145 <= inta .and. inta <= 153 .or. &
             162 <= inta .and. inta <= 169 ) then
          inta = inta + 64
        end if
    
        if ( 129 <= intb .and. intb <= 137 .or. &
             145 <= intb .and. intb <= 153 .or. &
             162 <= intb .and. intb <= 169 ) then
          intb = intb + 64
        end if
    
      else if ( zcode == 218 .or. zcode == 250 ) then
    !
    !  ASCII is assumed, on Prime machines - zcode is the ASCII code
    !  plus 128 of either lower or upper case 'Z'.
    !
        if ( 225 <= inta .and. inta <= 250 ) then
          inta = inta - 32
        end if
    
        if ( 225 <= intb .and. intb <= 250 ) then
          intb = intb - 32
        end if
    
      end if
    
      lsame = ( inta == intb )
    
      return
    end
    function r4_abs ( x )
    
    !*****************************************************************************80
    !
    !! R4_ABS returns the absolute value of an R4.
    !
    !  Discussion:
    !
    !    An R4 is a real ( kind = 4 ) value.
    !
    !    FORTRAN90 supplies the ABS function, which should be used instead
    !    of this function!
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    25 June 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 4 ) X, the number whose absolute value is desired.
    !
    !    Output, real ( kind = 4 ) R4_ABS, the absolute value of X.
    !
      implicit none
    
      real ( kind = 4 ) r4_abs
      real ( kind = 4 ) x
    
      if ( 0.0E+00 <= x ) then
        r4_abs = + x
      else
        r4_abs = - x
      end if
    
      return
    end
    function r4_sign ( x )
    
    !*****************************************************************************80
    !
    !! R4_SIGN returns the sign of an R4.
    !
    !  Discussion:
    !
    !    value = -1 if X < 0;
    !    value =  0 if X => 0.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 March 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 4 ) X, the number whose sign is desired.
    !
    !    Output, real ( kind = 4 ) R4_SIGN, the sign of X:
    !
      implicit none
    
      real ( kind = 4 ) r4_sign
      real ( kind = 4 ) x
    
      if ( x < 0.0E+00 ) then
        r4_sign = -1.0E+00
      else
        r4_sign = +1.0E+00
      end if
    
      return
    end
    function r4_uniform_01 ( seed )
    
    !*****************************************************************************80
    !
    !! R4_UNIFORM_01 returns a unit pseudorandom R4.
    !
    !  Discussion:
    !
    !    This routine implements the recursion
    !
    !      seed = 16807 * seed mod ( 2^31 - 1 )
    !      r4_uniform_01 = seed / ( 2^31 - 1 )
    !
    !    The integer arithmetic never requires more than 32 bits,
    !    including a sign bit.
    !
    !    If the initial seed is 12345, then the first three computations are
    !
    !      Input     Output      R4_UNIFORM_01
    !      SEED      SEED
    !
    !         12345   207482415  0.096616
    !     207482415  1790989824  0.833995
    !    1790989824  2035175616  0.947702
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    11 August 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Paul Bratley, Bennett Fox, Linus Schrage,
    !    A Guide to Simulation,
    !    Springer Verlag, pages 201-202, 1983.
    !
    !    Pierre L'Ecuyer,
    !    Random Number Generation,
    !    in Handbook of Simulation,
    !    edited by Jerry Banks,
    !    Wiley Interscience, page 95, 1998.
    !
    !    Bennett Fox,
    !    Algorithm 647:
    !    Implementation and Relative Efficiency of Quasirandom
    !    Sequence Generators,
    !    ACM Transactions on Mathematical Software,
    !    Volume 12, Number 4, pages 362-376, 1986.
    !
    !    Peter Lewis, Allen Goodman, James Miller,
    !    A Pseudo-Random Number Generator for the System/360,
    !    IBM Systems Journal,
    !    Volume 8, pages 136-143, 1969.
    !
    !  Parameters:
    !
    !    Input/output, integer SEED, the "seed" value, which should NOT be 0.
    !    On output, SEED has been updated.
    !
    !    Output, real R4_UNIFORM_01, a new pseudorandom variate,
    !    strictly between 0 and 1.
    !
      implicit none
    
      integer ( kind = 4 ) k
      integer ( kind = 4 ) seed
      real ( kind = 4 ) r4_uniform_01
    
      k = seed / 127773
    
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
      if ( seed < 0 ) then
        seed = seed + 2147483647
      end if
    !
    !  Although SEED can be represented exactly as a 32 bit integer,
    !  it generally cannot be represented exactly as a 32 bit real number!
    !
      r4_uniform_01 = real ( real ( seed, kind = 8 ) * 4.656612875D-10, kind = 4 )
    
      return
    end
    function r4_uniform_ab ( a, b, seed )
    
    !*****************************************************************************80
    !
    !! R4_UNIFORM_AB returns a scaled pseudorandom R4.
    !
    !  Discussion:
    !
    !    An R4 is a real ( kind = 4 ) value.
    !
    !    The pseudorandom number should be uniformly distributed
    !    between A and B.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    31 May 2007
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Paul Bratley, Bennett Fox, Linus Schrage,
    !    A Guide to Simulation,
    !    Second Edition,
    !    Springer, 1987,
    !    ISBN: 0387964673,
    !    LC: QA76.9.C65.B73.
    !
    !    Bennett Fox,
    !    Algorithm 647:
    !    Implementation and Relative Efficiency of Quasirandom
    !    Sequence Generators,
    !    ACM Transactions on Mathematical Software,
    !    Volume 12, Number 4, December 1986, pages 362-376.
    !
    !    Pierre L'Ecuyer,
    !    Random Number Generation,
    !    in Handbook of Simulation,
    !    edited by Jerry Banks,
    !    Wiley, 1998,
    !    ISBN: 0471134031,
    !    LC: T57.62.H37.
    !
    !    Peter Lewis, Allen Goodman, James Miller,
    !    A Pseudo-Random Number Generator for the System/360,
    !    IBM Systems Journal,
    !    Volume 8, Number 2, 1969, pages 136-143.
    !
    !  Parameters:
    !
    !    Input, real ( kind = 4 ) A, B, the limits of the interval.
    !
    !    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
    !    should NOT be 0.  On output, SEED has been updated.
    !
    !    Output, real ( kind = 4 ) R4_UNIFORM_AB, a number strictly between A and B.
    !
      implicit none
    
      real ( kind = 4 ) a
      real ( kind = 4 ) b
      integer ( kind = 4 ), parameter :: i4_huge = 2147483647
      integer ( kind = 4 ) k
      real ( kind = 4 ) r4_uniform_ab
      integer ( kind = 4 ) seed
    
      if ( seed == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R4_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if
    
      k = seed / 127773
    
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if
    
      r4_uniform_ab = a + ( b - a ) * real ( seed, kind = 4 ) * 4.656612875E-10
    
      return
    end
    subroutine r4mat_print ( m, n, a, title )
    
    !*****************************************************************************80
    !
    !! R4MAT_PRINT prints an R4MAT.
    !
    !  Discussion:
    !
    !    An R4MAT is an array of R4 values.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 January 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows in A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns in A.
    !
    !    Input, real ( kind = 4 ) A(M,N), the matrix.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      real ( kind = 4 ) a(m,n)
      character ( len = * ) title
    
      call r4mat_print_some ( m, n, a, 1, 1, m, n, title )
    
      return
    end
    subroutine r4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )
    
    !*****************************************************************************80
    !
    !! R4MAT_PRINT_SOME prints some of an R4MAT.
    !
    !  Discussion:
    !
    !    An R4MAT is an array of R4 values.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    10 September 2009
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, real ( kind = 4 ) A(M,N), an M by N matrix to be printed.
    !
    !    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
    !
    !    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: incx = 5
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      real ( kind = 4 ) a(m,n)
      character ( len = 14 ) ctemp(incx)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2hi
      integer ( kind = 4 ) i2lo
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) inc
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) j2hi
      integer ( kind = 4 ) j2lo
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
    
      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx
    
        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )
    
        inc = j2hi + 1 - j2lo
    
        write ( *, '(a)' ) ' '
    
        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i8,6x)' ) j
        end do
    
        write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '
    
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )
    
        do i = i2lo, i2hi
    
          do j2 = 1, inc
    
            j = j2lo - 1 + j2
    
            if ( a(i,j) == real ( int ( a(i,j) ), kind = 4 ) ) then
              write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
            else
              write ( ctemp(j2), '(g14.6)' ) a(i,j)
            end if
    
          end do
    
          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )
    
        end do
    
      end do
    
      return
    end
    subroutine r4mat_test ( trans, lda, m, n, a )
    
    !*****************************************************************************80
    !
    !! R4MAT_TEST sets up a test matrix.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !  
    !  Modified:
    !
    !    10 February 2014
    !
    !  Author:
    !
    !    John Burkardt.
    !
    !  Parameters:
    !
    !    Input, character * ( 1 ) TRANS, indicates whether matrix is to be 
    !    transposed.
    !    'N', no transpose.
    !    'T', transpose the matrix.
    !
    !    Input, integer ( kind = 4 ) LDA, the leading dimension of the matrix.
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns of 
    !    the matrix.
    !
    !    Output, real ( kind = 4 ) A(LDA,*), the matrix.
    !    if TRANS is 'N', then the matrix is stored in LDA*N entries,
    !    as an M x N matrix;
    !    if TRANS is 'T', then the matrix is stored in LDA*M entries,
    !    as an N x M matrix.
    !
      implicit none
    
      integer ( kind = 4 ) lda
    
      real ( kind = 4 ) a(lda,*)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
      character * ( 1 ) trans
    
      if ( trans == 'N' ) then
    
        do j = 1, n
          do i = 1, m
            a(i,j) = real ( 10 * i + j, kind = 4 )
          end do
        end do
    
      else
    
        do j = 1, n
          do i = 1, m
            a(j,i) = real ( 10 * i + j, kind = 4 )
          end do
        end do
    
      end if
    
      return
    end
    subroutine r4vec_print ( n, a, title )
    
    !*****************************************************************************80
    !
    !! R4VEC_PRINT prints an R4VEC.
    !
    !  Discussion:
    !
    !    An R4VEC is a vector of R4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 January 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of components of the vector.
    !
    !    Input, real ( kind = 4 ) A(N), the vector to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 4 ) a(n)
      integer ( kind = 4 ) i
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do
    
      return
    end
    function r8_abs ( x )
    
    !*****************************************************************************80
    !
    !! R8_ABS returns the absolute value of an R8.
    !
    !  Discussion:
    !
    !    An R8 is a real ( kind = 8 ) value.
    !
    !    FORTRAN90 supplies the ABS function, which should be used instead
    !    of this function!
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    06 September 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 8 ) X, the number whose absolute value is desired.
    !
    !    Output, real ( kind = 8 ) R8_ABS, the absolute value of X.
    !
      implicit none
    
      real ( kind = 8 ) r8_abs
      real ( kind = 8 ) x
    
      if ( 0.0D+00 <= x ) then
        r8_abs = + x
      else
        r8_abs = - x
      end if
    
      return
    end
    function r8_sign ( x )
    
    !*****************************************************************************80
    !
    !! R8_SIGN returns the sign of an R8.
    !
    !  Discussion:
    !
    !    value = -1 if X < 0;
    !    value = +1 if X => 0.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 March 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 8 ) X, the number whose sign is desired.
    !
    !    Output, real ( kind = 8 ) R8_SIGN, the sign of X:
    !
      implicit none
    
      real ( kind = 8 ) r8_sign
      real ( kind = 8 ) x
    
      if ( x < 0.0D+00 ) then
        r8_sign = -1.0D+00
      else
        r8_sign = +1.0D+00
      end if
    
      return
    end
    function r8_uniform_01 ( seed )
    
    !*****************************************************************************80
    !
    !! R8_UNIFORM_01 returns a unit pseudorandom R8.
    !
    !  Discussion:
    !
    !    This routine implements the recursion
    !
    !      seed = 16807 * seed mod ( 2^31 - 1 )
    !      r8_uniform_01 = seed / ( 2^31 - 1 )
    !
    !    The integer arithmetic never requires more than 32 bits,
    !    including a sign bit.
    !
    !    If the initial seed is 12345, then the first three computations are
    !
    !      Input     Output      R8_UNIFORM_01
    !      SEED      SEED
    !
    !         12345   207482415  0.096616
    !     207482415  1790989824  0.833995
    !    1790989824  2035175616  0.947702
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    11 August 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Paul Bratley, Bennett Fox, Linus Schrage,
    !    A Guide to Simulation,
    !    Springer Verlag, pages 201-202, 1983.
    !
    !    Pierre L'Ecuyer,
    !    Random Number Generation,
    !    in Handbook of Simulation,
    !    edited by Jerry Banks,
    !    Wiley Interscience, page 95, 1998.
    !
    !    Bennett Fox,
    !    Algorithm 647:
    !    Implementation and Relative Efficiency of Quasirandom
    !    Sequence Generators,
    !    ACM Transactions on Mathematical Software,
    !    Volume 12, Number 4, pages 362-376, 1986.
    !
    !    Philip Lewis, Allen Goodman, James Miller,
    !    A Pseudo-Random Number Generator for the System/360,
    !    IBM Systems Journal,
    !    Volume 8, pages 136-143, 1969.
    !
    !  Parameters:
    !
    !    Input/output, integer SEED, the "seed" value, which should NOT be 0.
    !    On output, SEED has been updated.
    !
    !    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
    !    strictly between 0 and 1.
    !
      implicit none
    
      integer ( kind = 4 ) k
      integer ( kind = 4 ) seed
      real ( kind = 8 ) r8_uniform_01
    
      k = seed / 127773
    
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
      if ( seed < 0 ) then
        seed = seed + 2147483647
      end if
    !
    !  Although SEED can be represented exactly as a 32 bit integer,
    !  it generally cannot be represented exactly as a 32 bit real number!
    !
      r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10
    
      return
    end
    function r8_uniform_ab ( a, b, seed )
    
    !*****************************************************************************80
    !
    !! R8_UNIFORM_AB returns a scaled pseudorandom R8.
    !
    !  Discussion:
    !
    !    An R8 is a real ( kind = 8 ) value.
    !
    !    For now, the input quantity SEED is an integer variable.
    !
    !    The pseudorandom number should be uniformly distributed
    !    between A and B.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    05 July 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 8 ) A, B, the limits of the interval.
    !
    !    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
    !    NOT be 0.  On output, SEED has been updated.
    !
    !    Output, real ( kind = 8 ) R8_UNIFORM_AB, a number strictly between A and B.
    !
      implicit none
    
      real ( kind = 8 ) a
      real ( kind = 8 ) b
      integer ( kind = 4 ), parameter :: i4_huge = 2147483647
      integer ( kind = 4 ) k
      real ( kind = 8 ) r8_uniform_ab
      integer ( kind = 4 ) seed
    
      if ( seed == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if
    
      k = seed / 127773
    
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if
    
      r8_uniform_ab = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10
    
      return
    end
    subroutine r8mat_print ( m, n, a, title )
    
    !*****************************************************************************80
    !
    !! R8MAT_PRINT prints an R8MAT.
    !
    !  Discussion:
    !
    !    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    12 September 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows in A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns in A.
    !
    !    Input, real ( kind = 8 ) A(M,N), the matrix.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(m,n)
      character ( len = * ) title
    
      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )
    
      return
    end
    subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )
    
    !*****************************************************************************80
    !
    !! R8MAT_PRINT_SOME prints some of an R8MAT.
    !
    !  Discussion:
    !
    !    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    10 September 2009
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
    !
    !    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
    !
    !    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: incx = 5
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(m,n)
      character ( len = 14 ) ctemp(incx)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2hi
      integer ( kind = 4 ) i2lo
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) inc
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) j2hi
      integer ( kind = 4 ) j2lo
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
    
      if ( m <= 0 .or. n <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if
    
      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx
    
        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )
    
        inc = j2hi + 1 - j2lo
    
        write ( *, '(a)' ) ' '
    
        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i8,6x)' ) j
        end do
    
        write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '
    
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )
    
        do i = i2lo, i2hi
    
          do j2 = 1, inc
    
            j = j2lo - 1 + j2
    
            if ( a(i,j) == real ( int ( a(i,j) ), kind = 8 ) ) then
              write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
            else
              write ( ctemp(j2), '(g14.6)' ) a(i,j)
            end if
    
          end do
    
          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )
    
        end do
    
      end do
    
      return
    end
    subroutine r8mat_test ( trans, lda, m, n, a )
    
    !*****************************************************************************80
    !
    !! R8MAT_TEST sets up a test matrix.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !  
    !  Modified:
    !
    !    10 February 2014
    !
    !  Author:
    !
    !    John Burkardt.
    !
    !  Parameters:
    !
    !    Input, character * ( 1 ) TRANS, indicates whether matrix is to be 
    !    transposed.
    !    'N', no transpose.
    !    'T', transpose the matrix.
    !
    !    Input, integer ( kind = 4 ) LDA, the leading dimension of the matrix.
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns of 
    !    the matrix.
    !
    !    Output, real ( kind = 8 ) A(LDA,*), the matrix.
    !    if TRANS is 'N', then the matrix is stored in LDA*N entries,
    !    as an M x N matrix;
    !    if TRANS is 'T', then the matrix is stored in LDA*M entries,
    !    as an N x M matrix.
    !
      implicit none
    
      integer ( kind = 4 ) lda
    
      real ( kind = 8 ) a(lda,*)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
      character * ( 1 ) trans
    
      if ( trans == 'N' ) then
    
        do j = 1, n
          do i = 1, m
            a(i,j) = real ( 10 * i + j, kind = 8 )
          end do
        end do
    
      else
    
        do j = 1, n
          do i = 1, m
            a(j,i) = real ( 10 * i + j, kind = 8 )
          end do
        end do
    
      end if
    
      return
    end
    subroutine r8vec_print ( n, a, title )
    
    !*****************************************************************************80
    !
    !! R8VEC_PRINT prints an R8VEC.
    !
    !  Discussion:
    !
    !    An R8VEC is a vector of R8's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    22 August 2000
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of components of the vector.
    !
    !    Input, real ( kind = 8 ) A(N), the vector to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(n)
      integer ( kind = 4 ) i
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
    
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do
    
      return
    end
    function smach ( job )
    
    !*****************************************************************************80
    !
    !! SMACH computes machine parameters for single precision arithmetic.
    !
    !  Discussion:
    !
    !    Assume the computer has
    !
    !      B = base of arithmetic;
    !      T = number of base B digits;
    !      L = smallest possible exponent;
    !      U = largest possible exponent;
    !
    !    then
    !
    !      EPS = B^(1-T)
    !      TINY = 100.0 * B^(-L+T)
    !      HUGE = 0.01 * B^(U-T)
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    01 June 2020
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Algorithm 539, 
    !    Basic Linear Algebra Subprograms for Fortran Usage,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, pages 308-323, 1979.
    !
    !  Input:
    !
    !    integer ( kind = 4 ) JOB:
    !    1, EPS is desired;
    !    2, TINY is desired;
    !    3, HUGE is desired.
    !
    !  Output:
    !
    !    real ( kind = 4 ) SMACH, the requested value.
    !
      implicit none
    
      integer ( kind = 4 ) job
      real ( kind = 4 ) smach
      real ( kind = 4 ) t
    
      if ( job == 1 ) then
        smach = epsilon ( t )
      else if ( job == 2 ) then
        smach = tiny ( t )
      else if ( job == 3 ) then
        smach = huge ( t )
      else
        smach = 0.0E+00
      end if
    
      return
    end
    subroutine timestamp ( )
    
    !*****************************************************************************80
    !
    !! TIMESTAMP prints the current YMDHMS date as a time stamp.
    !
    !  Example:
    !
    !    31 May 2001   9:45:54.872 AM
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 May 2013
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    None
    !
      implicit none
    
      character ( len = 8 ) ampm
      integer ( kind = 4 ) d
      integer ( kind = 4 ) h
      integer ( kind = 4 ) m
      integer ( kind = 4 ) mm
      character ( len = 9 ), parameter, dimension(12) :: month = (/ &
        'January  ', 'February ', 'March    ', 'April    ', &
        'May      ', 'June     ', 'July     ', 'August   ', &
        'September', 'October  ', 'November ', 'December ' /)
      integer ( kind = 4 ) n
      integer ( kind = 4 ) s
      integer ( kind = 4 ) values(8)
      integer ( kind = 4 ) y
    
      call date_and_time ( values = values )
    
      y = values(1)
      m = values(2)
      d = values(3)
      h = values(5)
      n = values(6)
      s = values(7)
      mm = values(8)
    
      if ( h < 12 ) then
        ampm = 'AM'
      else if ( h == 12 ) then
        if ( n == 0 .and. s == 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h < 12 ) then
          ampm = 'PM'
        else if ( h == 12 ) then
          if ( n == 0 .and. s == 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if
    
      write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
        d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )
    
      return
    end
    subroutine triangle_upper_to_i4 ( i, j, k )
    
    !*****************************************************************************80
    !
    !! TRIANGLE_UPPER_TO_I4 converts an upper triangular coordinate to an integer.
    !
    !  Discussion:
    !
    !    Triangular coordinates are handy when storing a naturally triangular
    !    array (such as the upper half of a matrix) in a linear array.
    !
    !    Thus, for example, we might consider storing 
    !
    !    (1,1) (1,2) (1,3) (1,4)
    !          (2,2) (2,3) (2,4)
    !          (3,2) (3,3) (3,4)
    !          (4,2) (4,3) (4,4)
    !
    !    as the linear array
    !
    !    (1,1) (1,2) (2,2) (1,3) (2,3) (3,3) (1,4) (2,4) (3,4) (4,4)    
    !
    !    Here, the quantities in parenthesis represent the natural row and
    !    column indices of a single number when stored in a rectangular array.
    !
    !    Thus, our goal is, given the row I and column J of the data,
    !    to produce the value K which indicates its position in the linear
    !    array.
    !
    !    The triangular numbers are the indices associated with the
    !    diagonal elements of the original array, T(1,1), T(2,2), T(3,3)
    !    and so on.
    !
    !    The formula is:
    !
    !      K = I + ( (J-1) * J ) / 2
    !
    !  First Values:
    !
    !     I  J  K
    !
    !     0  0  0
    !     1  1  1
    !     1  2  2
    !     2  2  3
    !     1  3  4
    !     2  3  5
    !     3  3  6
    !     1  4  7
    !     2  4  8
    !     3  4  9
    !     4  4 10
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    22 March 2017
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, J, the row and column indices.  I and J must
    !    be nonnegative, and I must not be greater than J.
    !
    !    Output, integer ( kind = 4 ) K, the linear index of the (I,J) element.
    
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
    
      if ( i < 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_UPPER_TO_I4 - Fatal error!'
        write ( *, '(a)' ) '  I < 0.'
        write ( *, '(a,i8)' ) '  I = ', i
        stop 1
      else if ( j < 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_UPPER_TO_I4 - Fatal error!'
        write ( *, '(a)' ) '  J < 0.'
        write ( *, '(a,i8)' ) '  J = ', j
        stop 1
      else if ( j < i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_UPPER_TO_I4 - Fatal error!'
        write ( *, '(a)' ) '  J < I.'
        write ( *, '(a,i8)' ) '  I = ', i
        write ( *, '(a,i8)' ) '  J = ', j
        stop 1
      end if
    
      k = i + ( ( j - 1 ) * j ) / 2
    
      return
    end
    subroutine xerbla ( srname, info )
    
    !*****************************************************************************80
    !
    !! XERBLA is an error handler for the LAPACK routines.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    16 May 2005
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Basic Linear Algebra Subprograms for Fortran Usage,
    !    Algorithm 539,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, September 1979, pages 308-323.
    !
    !  Parameters:
    !
    !    Input, character ( len = 6 ) SRNAME, the name of the routine
    !    which called XERBLA.
    !
    !    Input, integer ( kind = 4 ) INFO, the position of the invalid parameter in
    !    the parameter list of the calling routine.
    !
      implicit none
    
      integer ( kind = 4 ) info
      character ( len = 6 ) srname
    
      write ( *, '(a,a6,a,i2,a)' ) ' ** On entry to ', srname, &
        ' parameter number ', info, ' had an illegal value.'
    
      stop 1
    end
    function zabs1 ( z )
    
    !*****************************************************************************80
    !
    !! ZABS1 returns the L1 norm of a double precision complex number.
    !
    !  Discussion:
    !
    !    The L1 norm of a double complex number is the sum of the absolute values
    !    of the real and imaginary components.
    !
    !    ZABS1 ( Z ) = abs ( real ( Z ) ) + abs ( imaginary ( Z ) )
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    22 May 2002
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Basic Linear Algebra Subprograms for FORTRAN usage,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, pages 308-323, 1979.
    !
    !  Parameters:
    !
    !    Input, complex ( kind = 8 ) Z, the number whose norm is desired.
    !
    !    Output, real ( kind = 8 ) ZABS1, the L1 norm of Z.
    !
      implicit none
    
      real ( kind = 8 ) zabs1
      complex ( kind = 8 ) z
    
      zabs1 = abs ( real ( z ) ) + abs ( aimag ( z ) )
    
      return
    end
    function zabs2 ( z )
    
    !*****************************************************************************80
    !
    !! ZABS2 returns the L2 norm of a double precision complex number.
    !
    !  Discussion:
    !
    !    The L2 norm of a complex number is the square root of the sum 
    !    of the squares of the real and imaginary components.
    !
    !    ZABS2 ( Z ) = sqrt ( ( real ( Z ) )**2 + ( imaginary ( Z ) )**2 )
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    19 March 2006
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Basic Linear Algebra Subprograms for FORTRAN usage,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, pages 308-323, 1979.
    !
    !  Parameters:
    !
    !    Input, complex ( kind = 8 ) Z, the number whose norm is desired.
    !
    !    Output, real ( kind = 8 ) ZABS2, the L2 norm of Z.
    !
      implicit none
    
      complex ( kind = 8 ) z
      real ( kind = 8 ) zabs2
    
      zabs2 = sqrt ( ( real ( z, kind = 8 ) )**2 &
                   + ( aimag ( z ) )**2 )
    
      return
    end
    function zmach ( job )
    
    !*****************************************************************************80
    !
    !! ZMACH computes machine parameters for double precision complex arithmetic.
    !
    !  Discussion:
    !
    !    Assume the computer has
    !
    !      B = base of arithmetic;
    !      T = number of base B digits;
    !      L = smallest possible exponent;
    !      U = largest possible exponent;
    !
    !    then
    !
    !      EPS = B^(1-T)
    !      TINY = 100.0 * B^(-L+T)
    !      HUGE = 0.01 * B^(U-T)
    !
    !    If complex division is done by
    !
    !      1 / (X+i*Y) = (X-i*Y) / (X^2+Y^2)
    !
    !    then
    !
    !      TINY = sqrt ( TINY )
    !      HUGE = sqrt ( HUGE )
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    28 March 2006
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
    !    LINPACK User's Guide,
    !    SIAM, 1979,
    !    ISBN13: 978-0-898711-72-1,
    !    LC: QA214.L56.
    !
    !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
    !    Basic Linear Algebra Subprograms for FORTRAN usage,
    !    ACM Transactions on Mathematical Software,
    !    Volume 5, Number 3, pages 308-323, 1979.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) JOB:
    !    1, EPS is desired;
    !    2, TINY is desired;
    !    3, HUGE is desired.
    !
    !    Output, real ( kind = 8 ) ZMACH, the requested value.
    !
      implicit none
    
      integer ( kind = 4 ) job
      real ( kind = 8 ), parameter :: t = 1.0D+00
      real ( kind = 8 ) zmach
    
      if ( job == 1 ) then
        zmach = epsilon ( t )
      else if ( job == 2 ) then
        zmach = tiny ( t )
      else if ( job == 3 ) then
        zmach = huge ( t )
      else
        zmach = 0.0D+00
      end if
    
      return
    end
    function zsign1 ( z1, z2 )
    
    !*****************************************************************************80
    !
    !! ZSIGN1 is a double precision complex transfer-of-sign function.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    28 March 2006
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Parameters:
    !
    !    Input, complex ( kind = 8 ) Z1, Z2, the arguments.
    !
    !    Output, complex ( kind = 8 ) ZSIGN1,  a complex value, with the 
    !    magnitude of Z1, and the argument of Z2.
    !
      implicit none
    
      complex ( kind = 8 ) z1
      complex ( kind = 8 ) z2
      real ( kind = 8 ) zabs1
      complex ( kind = 8 ) zsign1
    
      if ( zabs1 ( z2 ) == 0.0D+00 ) then
        zsign1 = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
      else
        zsign1 = zabs1 ( z1 ) * ( z2 / zabs1 ( z2 ) )
      end if
    
      return
    end
    function zsign2 ( z1, z2 )
    
    !*****************************************************************************80
    !
    !! ZSIGN2 is a double precision complex transfer-of-sign function.
    !
    !  Discussion:
    !
    !    The L2 norm is used.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    28 March 2006
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !  Parameters:
    !
    !    Input, complex ( kind = 8 ) Z1, Z2, the arguments.
    !
    !    Output, complex ( kind = 8 ) CSIGN2,  a complex value, with the 
    !    magnitude of Z1, and the argument of Z2.
    !
      implicit none
    
      complex ( kind = 8 ) z1
      complex ( kind = 8 ) z2
      real ( kind = 8 ) zabs2
      complex ( kind = 8 ) zsign2
    
      if ( zabs2 ( z2 ) == 0.0D+00 ) then
        zsign2 = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
      else
        zsign2 = zabs2 ( z1 ) * ( z2 / zabs2 ( z2 ) )
      end if
    
      return
    end
    end module