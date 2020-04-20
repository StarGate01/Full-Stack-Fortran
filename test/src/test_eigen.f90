module test_eigen
    contains
    subroutine test_eigen_main()

        !*****************************************************************************80
        !
        !! MAIN is the main program for TEST_EIGEN_TEST.
        !
        !  Discussion:
        !
        !    TEST_EIGEN_TEST tests the TEST_EIGEN library.
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Modified:
        !
        !    09 March 2018
        !
        !  Author:
        !
        !    John Burkardt
        !
          implicit none
        
          call timestamp ( )
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'TEST_EIGEN_TEST'
          write ( *, '(a)' ) '  FORTRAN90 version'
          write ( *, '(a)' ) '  Test the TEST_EIGEN library.'
        
          call r8symm_gen_test ( )
          call r8nsymm_gen_test ( )
        !
        !  Terminate.
        !
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'TEST_EIGEN_TEST'
          write ( *, '(a)' ) '  Normal end of execution.'
          write ( *, '(a)' ) ' '
          call timestamp ( )
        
          stop 0
        end
        subroutine r8symm_gen_test ( )
        
        !*****************************************************************************80
        !
        !! R8SYMM_GEN_TEST tests R8SYMM_GEN.
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Modified:
        !
        !    20 February 2012
        !
        !  Author:
        !
        !    John Burkardt
        !
          implicit none
        
          integer ( kind = 4 ), parameter :: n = 100
          integer ( kind = 4 ), parameter :: bin_num = 10
        
          real ( kind = 8 ) a(n,n)
          real ( kind = 8 ) aq(n,n)
          integer ( kind = 4 ) bin(0:bin_num+1)
          real ( kind = 8 ) bin_limit(0:bin_num)
          integer ( kind = 4 ) j
          real ( kind = 8 ) lambda(n)
          real ( kind = 8 ) lambda2(n)
          real ( kind = 8 ), parameter :: lambda_dev = 1.0D+00
          real ( kind = 8 ) lambda_max
          real ( kind = 8 ), parameter :: lambda_mean = 1.0D+00
          real ( kind = 8 ) lambda_min
          real ( kind = 8 ) q(n,n)
          integer ( kind = 4 ) :: seed = 123456789
        
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8SYMM_GEN_TEST'
          write ( *, '(a)' ) '  R8SYMM_GEN generates an arbitrary size symmetric matrix'
          write ( *, '(a)' ) '  with known eigenvalues and eigenvectors.'
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  Real data is declared as "REAL ( KIND = 8 )".'
        
          call r8symm_gen ( n, lambda_mean, lambda_dev, seed, a, q, lambda )
        !
        !  Get the eigenvalue range.
        !
          lambda_min = minval ( lambda(1:n) )
          lambda_max = maxval ( lambda(1:n) )
        
          write ( *, '(a)' ) ' '
          write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
          write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max
        !
        !  Bin the eigenvalues.
        !
          call r8vec_bin ( n, lambda, bin_num, lambda_min, lambda_max, bin, bin_limit )
        
          call r8bin_print ( bin_num, bin, bin_limit, '  Lambda bins:' )
        
          if ( .false. ) then
            call r8mat_print ( n, n, a, '  The matrix A:' )
          end if
        
          if ( .false. ) then
            call r8mat_print ( n, n, q, '  The eigenvector matrix Q:' )
          end if
        
          aq(1:n,1:n) = matmul ( a(1:n,1:n), q(1:n,1:n) )
        
          do j = 1, n
            lambda2(j) = sqrt ( sum ( aq(1:n,j)**2 ) )
          end do
        
          call r8vec2_print ( n, lambda, lambda2, &
            '  LAMBDA versus the column norms of A*Q:' )
        
          return
        end
        subroutine r8nsymm_gen_test ( )
        
        !*****************************************************************************80
        !
        !! R8NSYMM_GEN_TEST tests R8NSYMM_GEN.
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Modified:
        !
        !    09 March 2018
        !
        !  Author:
        !
        !    John Burkardt
        !
          implicit none
        
          integer ( kind = 4 ), parameter :: n = 100
          integer ( kind = 4 ), parameter :: bin_num = 10
        
          real ( kind = 8 ) a(n,n)
          real ( kind = 8 ) aqr(n,n)
          integer ( kind = 4 ) bin(0:bin_num+1)
          real ( kind = 8 ) bin_limit(0:bin_num)
          integer ( kind = 4 ) j
          real ( kind = 8 ) lambda(n)
          real ( kind = 8 ) lambda2(n)
          real ( kind = 8 ), parameter :: lambda_dev = 1.0D+00
          real ( kind = 8 ) lambda_max
          real ( kind = 8 ), parameter :: lambda_mean = 1.0D+00
          real ( kind = 8 ) lambda_min
          real ( kind = 8 ) ql(n,n)
          real ( kind = 8 ) qr(n,n)
          integer ( kind = 4 ) :: seed = 123456789
        
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8NSYMM_GEN_TEST'
          write ( *, '(a)' ) '  R8NSYMM_GEN generates an arbitrary size nonsymmetric'
          write ( *, '(a)' ) '  matrix with known eigenvalues and eigenvectors.'
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  Real data is declared as "REAL ( KIND = 8 )".'
        
          call r8nsymm_gen ( n, lambda_mean, lambda_dev, seed, a, ql, qr, lambda )
        !
        !  Get the eigenvalue range.
        !
          lambda_min = minval ( lambda(1:n) )
          lambda_max = maxval ( lambda(1:n) )
        
          write ( *, '(a)' ) ' '
          write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
          write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max
        !
        !  Bin the eigenvalues.
        !
          call r8vec_bin ( n, lambda, bin_num, lambda_min, lambda_max, bin, bin_limit )
        
          call r8bin_print ( bin_num, bin, bin_limit, '  Lambda bins:' )
        
          if ( .false. ) then
            call r8mat_print ( n, n, a, '  The matrix A:' )
          end if
        
          if ( .false. ) then
            call r8mat_print ( n, n, qr, '  The right eigenvector matrix QR:' )
          end if
        
          aqr(1:n,1:n) = matmul ( a(1:n,1:n), qr(1:n,1:n) )
        
          do j = 1, n
            lambda2(j) = sqrt ( sum ( aqr(1:n,j)**2 ) )
          end do
        
          call r8vec2_print ( n, lambda, lambda2, &
            '  LAMBDA versus the column norms of A*Q:' )
        
          return
        end
    
        
subroutine get_unit ( iunit )

    !*****************************************************************************80
    !
    !! GET_UNIT returns a free FORTRAN unit number.
    !
    !  Discussion:
    !
    !    A "free" FORTRAN unit number is an integer between 1 and 99 which
    !    is not currently associated with an I/O device.  A free FORTRAN unit
    !    number is needed in order to open a file with the OPEN command.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    02 March 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Output, integer ( kind = 4 ) IUNIT.
    !
    !    If IUNIT = 0, then no free FORTRAN unit could be found, although
    !    all 99 units were checked (except for units 5 and 6).
    !
    !    Otherwise, IUNIT is an integer between 1 and 99, representing a
    !    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
    !    are special, and will never return those values.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) ios
      integer ( kind = 4 ) iunit
      logical lopen
    
      iunit = 0
    
      do i = 1, 99
    
        if ( i /= 5 .and. i /= 6 ) then
    
          inquire ( unit = i, opened = lopen, iostat = ios )
    
          if ( ios == 0 ) then
            if ( .not. lopen ) then
              iunit = i
              return
            end if
          end if
    
        end if
    
      end do
    
      return
    end
    subroutine histogram_file_write ( bin_limit, bin, bin_num )
    
    !*****************************************************************************80
    !
    !! HISTOGRAM_FILE_WRITE creates a plot file of histogram data.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    31 January 2003
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) BIN_NUM, the number of bins.
    !
    !    Input, real ( kind = 8 ) BIN_LIMIT(0:BIN_NUM), the "limits" of the bins.
    !    BIN(I) counts the number of entries X(J) such that
    !      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
    !
    !    Output, integer ( kind = 4 ) BIN(0:BIN_NUM+1).
    !    BIN(0) counts entries of X less than BIN_LIMIT(0).
    !    BIN(BIN_NUM+1) counts entries greater than or equal to BIN_LIMIT(BIN_NUM).
    !    For 1 <= I <= BIN_NUM, BIN(I) counts the entries X(J) such that
    !      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
    !    where H is the bin spacing.
    !
      implicit none
    
      integer ( kind = 4 ) bin_num
    
      real ( kind = 8 ) bin_limit(0:bin_num)
      real ( kind = 8 ) bin(0:bin_num+1)
      real ( kind = 8 ) fat
      character ( len = 255 ) graph_file_name
      integer ( kind = 4 ) graph_file_unit
      integer ( kind = 4 ) i
      integer ( kind = 4 ) ios
      integer ( kind = 4 ) nlabel
      real ( kind = 8 ) px
      real ( kind = 8 ) pxmax
      real ( kind = 8 ) pxmin
      real ( kind = 8 ) py
      real ( kind = 8 ) pymax
      real ( kind = 8 ) pymin
      real ( kind = 8 ) s
      character ( len = 14 ) string
      real ( kind = 8 ) x
      real ( kind = 8 ) xmax
      real ( kind = 8 ) xmin
      real ( kind = 8 ) y
      real ( kind = 8 ) ymax
      real ( kind = 8 ) ymin
    
      call get_unit ( graph_file_unit )
    
      graph_file_name = 'hist.plot'
    
      open ( unit = graph_file_unit, file = graph_file_name, status = 'replace', &
        iostat = ios )
    
      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HISTOGRAM_FILE_WRITE - Fatal error!'
        write ( *, '(a)' ) '  Could not open the output unit.'
        stop
      end if
    
      write ( graph_file_unit, '(a)' ) '# ' // trim ( graph_file_name ) // &
        ' created by HISTOGRAM_FILE_WRITE.'
      write ( graph_file_unit, '(a)' ) '#'
      write ( graph_file_unit, '(a)' ) ' '
      write ( graph_file_unit, '(a)' ) 'file ' // trim ( graph_file_name )
      write ( graph_file_unit, '(a)' ) '  space 0.0 0.0 8.5 11.0'
    
      x = 0.0D+00
      y = 0.0D+00
    
      xmin = bin_limit(0)
      xmax = bin_limit(bin_num)
    
      ymin = 0.0D+00
      ymax = 0.0D+00
      do i = 1, bin_num
        ymax = max ( ymax, bin(i) )
      end do
    
      ymax = real ( 1 + int ( 10.0D+00 * ymax ), kind = 8 ) / 10.0D+00
      ymax = min ( ymax, 1.0D+00 )
    
      pxmin = 1.5D+00
      pxmax = 7.5D+00
      pymin = 2.0D+00
      pymax = 8.0D+00
    
      write ( graph_file_unit, '(a)' ) '  page'
      write ( graph_file_unit, '(a)' ) ' '
      write ( graph_file_unit, '(a)' ) '    line_width 1'
      write ( graph_file_unit, '(a)' ) '    line_rgb 0.5 0.5 0.5'
      write ( graph_file_unit, '(a)' ) '    grid 1.5 2.0 7.5 8.0 11 11'
      write ( graph_file_unit, '(a)' ) ' '
      write ( graph_file_unit, '(a)' ) '    line_width 2'
      write ( graph_file_unit, '(a)' ) ' '
      write ( graph_file_unit, '(a)' ) '    line_rgb 0.0 0.0 1.0'
    
      fat = 0.85D+00
    
      do i = 1, bin_num
    
        x = fat * bin_limit(i-1) + ( 1.0D+00 - fat ) * bin_limit(i)
        y = 0.0D+00
        px = pxmin + ( pxmax - pxmin ) * ( x - xmin ) / ( xmax - xmin )
        py = pymin + ( pymax - pymin ) * ( y - ymin ) / ( ymax - ymin )
        write ( graph_file_unit, '(a,2g14.6)' ) '    moveto ', px, py
    
        x = fat * bin_limit(i-1) + ( 1.0D+00 - fat ) * bin_limit(i)
        y = bin(i)
        px = pxmin + ( pxmax - pxmin ) * ( x - xmin ) / ( xmax - xmin )
        py = pymin + ( pymax - pymin ) * ( y - ymin ) / ( ymax - ymin )
        write ( graph_file_unit, '(a,2g14.6)' ) '    lineto ', px, py
    
        x = ( 1.0D+00 - fat ) * bin_limit(i-1) + fat * bin_limit(i)
        y = bin(i)
        px = pxmin + ( pxmax - pxmin ) * ( x - xmin ) / ( xmax - xmin )
        py = pymin + ( pymax - pymin ) * ( y - ymin ) / ( ymax - ymin )
        write ( graph_file_unit, '(a,2g14.6)' ) '    lineto ', px, py
    
        x = ( 1.0D+00 - fat ) * bin_limit(i-1) + fat * bin_limit(i)
        y = 0.0D+00
        px = pxmin + ( pxmax - pxmin ) * ( x - xmin ) / ( xmax - xmin )
        py = pymin + ( pymax - pymin ) * ( y - ymin ) / ( ymax - ymin )
        write ( graph_file_unit, '(a,2g14.6)' ) '    lineto ', px, py
    
      end do
    
      write ( graph_file_unit, '(a)' ) ' '
      write ( graph_file_unit, '(a)' ) '    line_width 2'
      write ( graph_file_unit, '(a)' ) '    line_rgb 0.0 0.0 0.0'
    
      write ( graph_file_unit, '(a)' ) ' '
      write ( graph_file_unit, '(a)' ) '    font_size 0.20'
    !
    !  Labels on left side of graph.
    !
      nlabel = 5
    
      do i = 0, nlabel
        s = (   real ( nlabel - i, kind = 8 ) * ymin   &
              + real (          i, kind = 8 ) * ymax ) &
              / real ( nlabel,     kind = 8 )
    
        x = 0.1D+00
        y = ( ( real ( nlabel - i, kind = 8 ) + 0.25D+00 ) * 2.0D+00   &
            + ( real (          i, kind = 8 ) - 0.25D+00 ) * 8.0D+00 ) &
              / real ( nlabel,     kind = 8 )
    
        write ( graph_file_unit, '(a,2g14.6)' ) '    moveto ', x, y
        call r8_to_s_left ( s, string )
        write ( graph_file_unit, '(a)' ) '    label ' // trim ( string )
      end do
    !
    !  Labels under the graph.
    !
      write ( graph_file_unit, '(a)' ) ' '
      write ( graph_file_unit, '(a)' ) '    moveto 4.0 1.5'
      write ( graph_file_unit, '(a)' ) '    label Z (Angstroms)'
    
      nlabel = 5
    
      do i = 0, nlabel
        s = (     real ( nlabel - i, kind = 8 ) * xmin   &
                + real (          i, kind = 8 ) * xmax ) &
                / real ( nlabel,     kind = 8 )
    
        x = (    ( real ( nlabel - i, kind = 8 ) + 0.25D+00 ) * pxmin   &
               + ( real (          i, kind = 8 ) - 0.25D+00 ) * pxmax ) &
                 / real ( nlabel,     kind = 8 )
    
        y = 1.75D+00
        write ( graph_file_unit, '(a,2g14.6)' ) '    moveto ', x, y
        call r8_to_s_left ( s, string )
        write ( graph_file_unit, '(a)' ) '    label ' // trim ( string )
      end do
    !
    !  Labels above the graph.
    !
      write ( graph_file_unit, '(a)' ) ' '
      write ( graph_file_unit, '(a)' ) '    font_size 0.20'
      write ( graph_file_unit, '(a)' ) '    moveto 1.0 9.0'
      write ( graph_file_unit, '(a)' ) '    label Glutamine Umbrella-sampled ' // &
        'Free Energy Calculations at Z = ?'
    
      write ( graph_file_unit, '(a)' ) ' '
      write ( graph_file_unit, '(a)' ) 'endpage'
    
      write ( graph_file_unit, '(a)' ) 'endfile'
    
      close ( unit = graph_file_unit )
    
      return
    end
    function r8_normal_01_func ( seed )
    
    !*****************************************************************************80
    !
    !! R8_NORMAL_01 returns a unit pseudonormal R8.
    !
    !  Discussion:
    !
    !    The standard normal probability distribution function (PDF) has
    !    mean 0 and standard deviation 1.
    !
    !    Because this routine uses the Box Muller method, it requires pairs
    !    of uniform random values to generate a pair of normal random values.
    !    This means that on every other call, essentially, the input value of
    !    SEED is ignored, since the code saves the second normal random value.
    !
    !    If you didn't know this, you might be confused since, usually, the
    !    output of a random number generator can be completely controlled by
    !    the input value of the SEED.  If I were more careful, I could rewrite
    !    this routine so that it would distinguish between cases where the input
    !    value of SEED is the output value from the previous call (all is well)
    !    and those cases where it is not (the user has decided to do something
    !    new.  Restart the uniform random number sequence.)  But I'll leave
    !    that for later.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    17 July 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, integer ( kind = 4 ) SEED, a seed for the random
    !    number generator.
    !
    !    Output, real ( kind = 8 ) R8_NORMAL_01, a sample of the
    !    standard normal PDF.
    !
      implicit none
    
      real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
      real ( kind = 8 ) r1
      real ( kind = 8 ) r2
      real ( kind = 8 ) r8_normal_01_func
      real ( kind = 8 ) r8_uniform_01
      integer ( kind = 4 ) seed
      integer ( kind = 4 ), save :: seed2 = 0
      integer ( kind = 4 ), save :: used = 0
      real ( kind = 8 ) x
      real ( kind = 8 ), save :: y = 0.0D+00
    !
    !  On odd numbered calls, generate two uniforms, create two normals,
    !  return the first normal and its corresponding seed.
    !
      if ( mod ( used, 2 ) == 0 ) then
    
        r1 = r8_uniform_01_func ( seed )
    
        if ( r1 == 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8_NORMAL_01 - Fatal error!'
          write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
          stop
        end if
    
        seed2 = seed
        r2 = r8_uniform_01_func ( seed2 )
    
        x = sqrt ( -2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * pi * r2 )
        y = sqrt ( -2.0D+00 * log ( r1 ) ) * sin ( 2.0D+00 * pi * r2 )
    !
    !  On odd calls, return the second normal and its corresponding seed.
    !
      else
    
        seed = seed2
        x = y
    
      end if
    
      used = used + 1
    
      r8_normal_01_func = x
    
      return
    end
    subroutine r8_to_s_left ( r, s )
    
    !*****************************************************************************80
    !
    !! R8_TO_S_LEFT writes a real into a left justified character string.
    !
    !  Method:
    !
    !    A 'G14.6' format is used with a WRITE statement.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    28 August 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 8 ) R, the real number to be written into STRING.
    !
    !    Output, character ( len = * ) S, the string into which
    !    the real number is to be written.  If the string is less than 14
    !    characters long, it will will be returned as a series of
    !    asterisks.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) nchar
      real ( kind = 8 ) r
      character ( len = * ) s
      character ( len = 14 ) s2
    
      nchar = len ( s )
    
      if ( nchar < 14 ) then
    
        do i = 1, nchar
          s(i:i) = '*'
        end do
    
      else if ( r == 0.0D+00 ) then
        s(1:14) = '     0.0      '
      else
        write ( s2, '(g14.6)' ) r
        s(1:14) = s2
      end if
    
      call s_left ( s )
    
      return
    end
    function r8_uniform_01_func ( seed )
    
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
    !    17 July 2006
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
    !    Peter Lewis, Allen Goodman, James Miller
    !    A Pseudo-Random Number Generator for the System/360,
    !    IBM Systems Journal,
    !    Volume 8, pages 136-143, 1969.
    !
    !  Parameters:
    !
    !    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
    !    NOT be 0.  On output, SEED has been updated.
    !
    !    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
    !    strictly between 0 and 1.
    !
      implicit none
    
      integer ( kind = 4 ) k
      real ( kind = 8 ) r8_uniform_01_func
      integer ( kind = 4 ) seed
    
      k = seed / 127773
    
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
      if ( seed < 0 ) then
        seed = seed + 2147483647
      end if
    !
    !  Although SEED can be represented exactly as a 32 bit integer,
    !  it generally cannot be represented exactly as a 32 bit real number!
    !
      r8_uniform_01_func = real ( seed, kind = 8 ) * 4.656612875D-10
    
      return
    end
    subroutine r8bin_print ( bin_num, bin, bin_limit, title )
    
    !*****************************************************************************80
    !
    !! R8BIN_PRINT prints the bins of a real vector.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    20 February 2012
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) BIN_NUM, the number of bins.
    !
    !    Input, integer ( kind = 4 ) BIN(0:BIN_NUM+1).
    !    BIN(0) counts entries of X less than BIN_LIMIT(0).
    !    BIN(BIN_NUM+1) counts entries greater than or equal to BIN_LIMIT(BIN_NUM).
    !    For 1 <= I <= BIN_NUM, BIN(I) counts the entries X(J) such that
    !      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
    !    where H is the bin spacing.
    !
    !    Input, real ( kind = 8 ) BIN_LIMIT(0:BIN_NUM), the "limits" of the bins.
    !    BIN(I) counts the number of entries X(J) such that
    !      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) bin_num
    
      integer ( kind = 4 ) bin(0:bin_num+1)
      real ( kind = 8 ) bin_limit(0:bin_num)
      integer ( kind = 4 ) i
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Index     Lower Limit   Count     Upper Limit'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,i6,2x,14x,2x,i6,2x,g14.6)' ) 0, bin(0), bin_limit(0)
      do i = 1, bin_num
        write ( *, '(2x,i6,2x,g14.6,2x,i6,2x,g14.6)' ) &
          i, bin_limit(i-1), bin(i), bin_limit(i)
      end do
      write ( *, '(2x,i6,2x,g14.6,2x,i6)') &
        bin_num + 1, bin_limit(bin_num), bin(bin_num+1)
    
      return
    end
    subroutine r8mat_copy ( m, n, a, b )
    
    !*****************************************************************************80
    !
    !! R8MAT_COPY copies an R8MAT.
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
    !    26 July 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the order of the matrix.
    !
    !    Input, real ( kind = 8 ) A(M,N), the matrix to be copied.
    !
    !    Output, real ( kind = 8 ) B(M,N), a copy of the matrix.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(m,n)
      real ( kind = 8 ) b(m,n)
    
      b(1:m,1:n) = a(1:m,1:n)
    
      return
    end
    subroutine r8mat_house_axh ( n, a, v, ah )
    
    !*****************************************************************************80
    !
    !! R8MAT_HOUSE_AXH computes A*H where H is a compact Householder matrix.
    !
    !  Discussion:
    !
    !    The Householder matrix H(V) is defined by
    !
    !      H(V) = I - 2 * v * v' / ( v' * v )
    !
    !    This routine is not particularly efficient.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    01 February 2002
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of A.
    !
    !    Input, real ( kind = 8 ) A(N,N), the matrix.
    !
    !    Input, real ( kind = 8 ) V(N), a vector defining a Householder matrix.
    !
    !    Output, real ( kind = 8 ) AH(N,N), the product A*H.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(n,n)
      real ( kind = 8 ) ah(n,n)
      real ( kind = 8 ) av(n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      real ( kind = 8 ) v(n)
      real ( kind = 8 ) v_normsq
    
      v_normsq = sum ( v(1:n)**2 )
    
      av(1:n) = matmul ( a(1:n,1:n), v(1:n) )
    
      ah(1:n,1:n) = a(1:n,1:n)
    
      do i = 1, n
        do j = 1, n
          ah(i,j) = ah(i,j) - 2.0D+00 * av(i) * v(j)
        end do
      end do
    
      ah(1:n,1:n) = ah(1:n,1:n) / v_normsq
    
      return
    end
    subroutine r8mat_orth_uniform ( n, seed, a )
    
    !*****************************************************************************80
    !
    !! R8MAT_ORTH_UNIFORM returns a random orthogonal R8MAT.
    !
    !  Discussion:
    !
    !    An R8MAT is a two dimensional matrix of R8 values.
    !
    !    Thanks to Eugene Petrov, B I Stepanov Institute of Physics,
    !    National Academy of Sciences of Belarus, for convincingly
    !    pointing out the severe deficiencies of an earlier version of
    !    this routine.
    !
    !    Essentially, the computation involves saving the Q factor of the
    !    QR factorization of a matrix whose entries are normally distributed.
    !    However, it is only necessary to generate this matrix a column at
    !    a time, since it can be shown that when it comes time to annihilate
    !    the subdiagonal elements of column K, these (transformed) elements of
    !    column K are still normally distributed random values.  Hence, there
    !    is no need to generate them at the beginning of the process and
    !    transform them K-1 times.
    !
    !    For computational efficiency, the individual Householder transformations
    !    could be saved, as recommended in the reference, instead of being
    !    accumulated into an explicit matrix format.
    !
    !  Properties:
    !
    !    The inverse of A is equal to A'.
    !
    !    A * A'  = A' * A = I.
    !
    !    Columns and rows of A have unit Euclidean norm.
    !
    !    Distinct pairs of columns of A are orthogonal.
    !
    !    Distinct pairs of rows of A are orthogonal.
    !
    !    The L2 vector norm of A*x = the L2 vector norm of x for any vector x.
    !
    !    The L2 matrix norm of A*B = the L2 matrix norm of B for any matrix B.
    !
    !    The determinant of A is +1 or -1.
    !
    !    All the eigenvalues of A have modulus 1.
    !
    !    All singular values of A are 1.
    !
    !    All entries of A are between -1 and 1.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 November 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Pete Stewart,
    !    Efficient Generation of Random Orthogonal Matrices With an Application
    !    to Condition Estimators,
    !    SIAM Journal on Numerical Analysis,
    !    Volume 17, Number 3, June 1980, pages 403-409.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of A.
    !
    !    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
    !    number generator.
    !
    !    Output, real ( kind = 8 ) A(N,N), the orthogonal matrix.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(n,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      real ( kind = 8 ) r8_normal_01
      integer ( kind = 4 ) seed
      real ( kind = 8 ) v(n)
      real ( kind = 8 ) x(n)
    !
    !  Start with A = the identity matrix.
    !
      do i = 1, n
        do j = 1, n
          if ( i == j ) then
            a(i,j) = 1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do
    !
    !  Now behave as though we were computing the QR factorization of
    !  some other random matrix.  Generate the N elements of the first column,
    !  compute the Householder matrix H1 that annihilates the subdiagonal elements,
    !  and set A := A * H1' = A * H.
    !
    !  On the second step, generate the lower N-1 elements of the second column,
    !  compute the Householder matrix H2 that annihilates them,
    !  and set A := A * H2' = A * H2 = H1 * H2.
    !
    !  On the N-1 step, generate the lower 2 elements of column N-1,
    !  compute the Householder matrix HN-1 that annihilates them, and
    !  and set A := A * H(N-1)' = A * H(N-1) = H1 * H2 * ... * H(N-1).
    !  This is our random orthogonal matrix.
    !
      do j = 1, n-1
    !
    !  Set the vector that represents the J-th column to be annihilated.
    !
        x(1:j-1) = 0.0D+00
    
        do i = j, n
          x(i) = r8_normal_01_func ( seed )
        end do
    !
    !  Compute the vector V that defines a Householder transformation matrix
    !  H(V) that annihilates the subdiagonal elements of X.
    !
        call r8vec_house_column ( n, x, j, v )
    !
    !  Postmultiply the matrix A by H'(V) = H(V).
    !
        call r8mat_house_axh ( n, a, v, a )
    
      end do
    
      return
    end
    subroutine r8mat_print ( m, n, a, title )
    
    !*****************************************************************************80
    !
    !! R8MAT_PRINT prints an R8MAT.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    20 May 2004
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
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 November 2003
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
    
      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx
    
        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )
    
        inc = j2hi + 1 - j2lo
    
        write ( *, '(a)' ) ' '
    
        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do
    
        write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '
    
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )
    
        do i = i2lo, i2hi
    
          do j2 = 1, inc
    
            j = j2lo - 1 + j2
    
            write ( ctemp(j2), '(g14.6)' ) a(i,j)
    
          end do
    
          write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j), j = 1, inc )
    
        end do
    
      end do
    
      return
    end
    subroutine r8nsymm_gen ( n, lambda_mean, lambda_dev, seed, a, ql, qr, lambda )
    
    !*****************************************************************************80
    !
    !! R8NSYMM_GEN generates a nonsymmetric matrix with a certain eigenstructure.
    !
    !  Discussion:
    !
    !    An R8NSYMM is a real nonsymmetric matrix stored using full storage, and
    !    using R8 arithmetic.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    09 March 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of the matrix.
    !
    !    Input, real ( kind = 8 ) LAMBDA_MEAN, the mean value of the normal
    !    distribution from which the eigenvalues will be chosen.
    !
    !    Input, real ( kind = 8 ) LAMBDA_DEV, the standard deviation of the normal
    !    distribution from which the eigenvalues will be chosen.
    !
    !    Input/output, integer ( kind = 4 ) SEED, a seed for the random
    !    number generator.
    !
    !    Output, real ( kind = 8 ) A(N,N), the test matrix.
    !
    !    Output, real ( kind = 8 ) QL(N,N), the left eigenvector matrix.
    !
    !    Output, real ( kind = 8 ) QR(N,N), the right eigenvector matrix.
    !
    !    Output, real ( kind = 8 ) LAMBDA(N), the eigenvalue vector.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(n,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
      real ( kind = 8 ) lambda(n)
      real ( kind = 8 ) lambda_dev
      real ( kind = 8 ) lambda_mean
      real ( kind = 8 ) ql(n,n)
      real ( kind = 8 ) qr(n,n)
      integer ( kind = 4 ) seed
    !
    !  Choose the eigenvalues LAMBDA.
    !
      call r8vec_normal_ab ( n, lambda_mean, lambda_dev, seed, lambda )
    !
    !  Get random orthogonal matrices QL and QR.
    !
      call r8mat_orth_uniform ( n, seed, ql )
      call r8mat_orth_uniform ( n, seed, qr )
    !
    !  Set A = QL * Lambda*I * QR'.
    !
      a(1:n,1:n) = 0.0D+00
    
      do i = 1, n
        do j = 1, n
          do k = 1, n
            a(i,j) = a(i,j) + ql(i,k) * lambda(k) * qr(j,k)
          end do
        end do
      end do
    
      return
    end
    subroutine r8nsymm_test01 ( a, q, lambda )
    
    !*****************************************************************************80
    !
    !! R8NSYMM_TEST01 returns a nonsymmetric matrix with a certain eigenstructure.
    !
    !  Discussion:
    !
    !    An R8NSYMM is a real nonsymmetric matrix stored using full storage, and
    !    using R8 arithmetic.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    28 October 2014
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Cleve Moler,
    !    Variants of the QR Algorithm,
    !    Mathworks News and Notes,
    !    October 2014.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) A(4,4), the test matrix.
    !
    !    Output, real ( kind = 8 ) Q(4,4), the eigenvector matrix.
    !
    !    Output, real ( kind = 8 ) LAMBDA(4), the eigenvalue vector.
    !
      implicit none
    
      real ( kind = 8 ) a(4,4)
      real ( kind = 8 ), dimension ( 4, 4 ) :: a_save = reshape ( (/ &
        0.0D+00, 1.0D+00, 0.0D+00, 0.0D+00, &
        2.0D+00, 0.0D+00, 1.0D+00, 0.0D+00, &
        0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00, &
       -1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00 /), (/ 4, 4 /) )
      real ( kind = 8 ) lambda(4)
      real ( kind = 8 ), dimension ( 4 ) :: lambda_save = (/ &
        -1.0D+00, -1.0D+00, +1.0D+00, +1.0D+00 /)
      real ( kind = 8 ) q(4,4)
      real ( kind = 8 ), dimension ( 4, 4 ) :: q_save = reshape ( (/ &
        0.5D+00, -0.5D+00,  0.5D+00, -0.5D+00, &
        0.5D+00, -0.5D+00,  0.5D+00, -0.5D+00, &
       -0.5D+00, -0.5D+00, -0.5D+00, -0.5D+00, &
       -0.5D+00, -0.5D+00, -0.5D+00, -0.5D+00 /), (/ 4, 4 /) )
    
      call r8mat_copy ( 4, 4, a_save, a )
      call r8vec_copy ( 4, lambda_save, lambda )
      call r8mat_copy ( 4, 4, q_save, q )
    
      return
    end
    subroutine r8symm_gen ( n, lambda_mean, lambda_dev, seed, a, q, lambda )
    
    !*****************************************************************************80
    !
    !! R8SYMM_GEN generates a symmetric matrix with a certain eigenstructure.
    !
    !  Discussion:
    !
    !    An R8SYMM is a real symmetric matrix stored using full storage, and
    !    using R8 arithmetic.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    31 October 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of the matrix.
    !
    !    Input, real ( kind = 8 ) LAMBDA_MEAN, the mean value of the normal
    !    distribution from which the eigenvalues will be chosen.
    !
    !    Input, real ( kind = 8 ) LAMBDA_DEV, the standard deviation of the normal
    !    distribution from which the eigenvalues will be chosen.
    !
    !    Input/output, integer ( kind = 4 ) SEED, a seed for the random
    !    number generator.
    !
    !    Output, real ( kind = 8 ) A(N,N), the test matrix.
    !
    !    Output, real ( kind = 8 ) Q(N,N), the eigenvector matrix.
    !
    !    Output, real ( kind = 8 ) LAMBDA(N), the eigenvalue vector.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(n,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
      real ( kind = 8 ) lambda(n)
      real ( kind = 8 ) lambda_dev
      real ( kind = 8 ) lambda_mean
      real ( kind = 8 ) q(n,n)
      integer ( kind = 4 ) seed
    !
    !  Choose the eigenvalues LAMBDA.
    !
      call r8vec_normal_ab ( n, lambda_mean, lambda_dev, seed, lambda )
    !
    !  Get a random orthogonal matrix Q.
    !
      call r8mat_orth_uniform ( n, seed, q )
    !
    !  Set A = Q * Lambda*I * Q'.
    !
      a(1:n,1:n) = 0.0D+00
    
      do i = 1, n
        do j = 1, n
          do k = 1, n
            a(i,j) = a(i,j) + q(i,k) * lambda(k) * q(j,k)
          end do
        end do
      end do
    
      return
    end
    subroutine r8vec_bin ( n, x, bin_num, bin_min, bin_max, bin, bin_limit )
    
    !*****************************************************************************80
    !
    !! R8VEC_BIN computes bins based on a given R8VEC.
    !
    !  Discussion:
    !
    !    The user specifies minimum and maximum bin values, BIN_MIN and
    !    BIN_MAX, and the number of bins, BIN_NUM.  This determines a
    !    "bin width":
    !
    !      H = ( BIN_MAX - BIN_MIN ) / BIN_NUM
    !
    !    so that bin I will count all entries X(J) such that
    !
    !      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
    !
    !    The array X does NOT have to be sorted.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    29 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of entries of X.
    !
    !    Input, real ( kind = 8 ) X(N), an (unsorted) array to be binned.
    !
    !    Input, integer ( kind = 4 ) BIN_NUM, the number of bins.  Two extra bins,
    !    #0 and #BIN_NUM+1, count extreme values.
    !
    !    Input, real ( kind = 8 ) BIN_MIN, BIN_MAX, define the range and size
    !    of the bins.  BIN_MIN and BIN_MAX must be distinct.
    !    Normally, BIN_MIN < BIN_MAX, and the documentation will assume
    !    this, but proper results will be computed if BIN_MIN > BIN_MAX.
    !
    !    Output, integer ( kind = 4 ) BIN(0:BIN_NUM+1).
    !    BIN(0) counts entries of X less than BIN_MIN.
    !    BIN(BIN_NUM+1) counts entries greater than or equal to BIN_MAX.
    !    For 1 <= I <= BIN_NUM, BIN(I) counts the entries X(J) such that
    !      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
    !    where H is the bin spacing.
    !
    !    Output, real ( kind = 8 ) BIN_LIMIT(0:BIN_NUM), the "limits" of the bins.
    !    BIN(I) counts the number of entries X(J) such that
    !      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
    !
      implicit none
    
      integer ( kind = 4 ) n
      integer ( kind = 4 ) bin_num
    
      integer ( kind = 4 ) bin(0:bin_num+1)
      real ( kind = 8 ) bin_limit(0:bin_num)
      real ( kind = 8 ) bin_max
      real ( kind = 8 ) bin_min
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      real ( kind = 8 ) t
      real ( kind = 8 ) x(n)
    
      if ( bin_max == bin_min ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_BIN - Fatal error!'
        write ( *, '(a)' ) '  BIN_MIN = BIN_MAX.'
        stop
      end if
    
      bin(0:bin_num+1) = 0
    
      do i = 1, n
    
        t = ( x(i) - bin_min ) / ( bin_max - bin_min )
    
        if ( t < 0.0D+00 ) then
          j = 0
        else if ( 1.0D+00 <= t ) then
          j = bin_num + 1
        else
          j = 1 + int ( real ( bin_num, kind = 8 ) * t )
        end if
    
        bin(j) = bin(j) + 1
    
      end do
    !
    !  Compute the bin limits.
    !
      do i = 0, bin_num
        bin_limit(i) = (   real ( bin_num - i, kind = 8 ) * bin_min   &
                         + real (           i, kind = 8 ) * bin_max ) &
                         / real ( bin_num,     kind = 8 )
      end do
    
      return
    end
    subroutine r8vec_copy ( n, a1, a2 )
    
    !*****************************************************************************80
    !
    !! R8VEC_COPY copies an R8VEC.
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
    !    17 September 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the length of the vectors.
    !
    !    Input, real ( kind = 8 ) A1(N), the vector to be copied.
    !
    !    Output, real ( kind = 8 ) A2(N), a copy of A1.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a1(n)
      real ( kind = 8 ) a2(n)
    
      a2(1:n) = a1(1:n)
    
      return
    end
    subroutine r8vec_house_column ( n, a, k, v )
    
    !*****************************************************************************80
    !
    !! R8VEC_HOUSE_COLUMN defines a Householder premultiplier that "packs" a column.
    !
    !  Discussion:
    !
    !    The routine returns a vector V that defines a Householder
    !    premultiplier matrix H(V) that zeros out the subdiagonal entries of
    !    column K of the matrix A.
    !
    !       H(V) = I - 2 * v * v'
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    01 June 2002
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of the matrix A.
    !
    !    Input, real ( kind = 8 ) A(N), column K of the matrix A.
    !
    !    Input, integer ( kind = 4 ) K, the column of the matrix to be modified.
    !
    !    Output, real ( kind = 8 ) V(N), a vector of unit L2 norm which defines an
    !    orthogonal Householder premultiplier matrix H with the property
    !    that the K-th column of H*A is zero below the diagonal.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(n)
      integer ( kind = 4 ) k
      real ( kind = 8 ) s
      real ( kind = 8 ) v(n)
    
      v(1:n) = 0.0D+00
    
      if ( k < 1 .or. n <= k ) then
        return
      end if
    
      s = sqrt ( dot_product ( a(k:n), a(k:n) ) )
    
      if ( s == 0.0D+00 ) then
        return
      end if
    
      v(k) = a(k) + sign ( s, a(k) )
      v(k+1:n) = a(k+1:n)
    
      v(k:n) = v(k:n) / sqrt ( dot_product ( v(k:n), v(k:n) ) )
    
      return
    end
    subroutine r8vec_normal_ab ( n, a, b, seed, x )
    
    !*****************************************************************************80
    !
    !! R8VEC_NORMAL_AB returns a scaled pseudonormal R8VEC.
    !
    !  Discussion:
    !
    !    The standard normal probability distribution function (PDF) has
    !    mean 0 and standard deviation 1.
    !
    !    This routine can generate a vector of values on one call.  It
    !    has the feature that it should provide the same results
    !    in the same order no matter how we break up the task.
    !
    !    Before calling this routine, the user may call RANDOM_SEED
    !    in order to set the seed of the random number generator.
    !
    !    The Box-Muller method is used, which is efficient, but
    !    generates an even number of values each time.  On any call
    !    to this routine, an even number of new values are generated.
    !    Depending on the situation, one value may be left over.
    !    In that case, it is saved for the next call.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    17 July 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of values desired.  If N
    !    is negative, then the code will flush its internal memory; in
    !    particular, if there is a saved value to be used on the next call,
    !    it is instead discarded.  This is useful if the user has reset the
    !    random number seed, for instance.
    !
    !    Input, real ( kind = 8 ) A, B, the mean and standard deviation.
    !
    !    Input/output, integer ( kind = 4 ) SEED, a seed for the random
    !    number generator.
    !
    !    Output, real ( kind = 8 ) X(N), a sample of the standard normal PDF.
    !
    !  Local parameters:
    !
    !    Local, integer ( kind = 4 ) MADE, records the number of values that have
    !    been computed.  On input with negative N, this value overwrites
    !    the return value of N, so the user can get an accounting of
    !    how much work has been done.
    !
    !    Local, real ( kind = 8 ) R(N+1), is used to store some uniform
    !    random values.  Its dimension is N+1, but really it is only needed
    !    to be the smallest even number greater than or equal to N.
    !
    !    Local, integer ( kind = 4 ) SAVED, is 0 or 1 depending on whether
    !    there is a single saved value left over from the previous call.
    !
    !    Local, integer ( kind = 4 ) X_LO_INDEX, X_HI_INDEX, records the range
    !    of entries of X that we need to compute.  This starts off as 1:N, but
    !    is adjusted if we have a saved value that can be immediately stored 
    !    in X(1), and so on.
    !
    !    Local, real ( kind = 8 ) Y, the value saved from the previous call, if
    !    SAVED is 1.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a
      real ( kind = 8 ) b
      integer ( kind = 4 ) m
      integer ( kind = 4 ), save :: made = 0
      real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
      real ( kind = 8 ) r(n+1)
      real ( kind = 8 ) r8_uniform_01
      integer ( kind = 4 ), save :: saved = 0
      integer ( kind = 4 ) seed
      real ( kind = 8 ) x(n)
      integer ( kind = 4 ) x_hi_index
      integer ( kind = 4 ) x_lo_index
      real ( kind = 8 ), save :: y = 0.0D+00
    !
    !  I'd like to allow the user to reset the internal data.
    !  But this won't work properly if we have a saved value Y.
    !  I'm making a crock option that allows the user to signal
    !  explicitly that any internal memory should be flushed,
    !  by passing in a negative value for N.
    !
      if ( n < 0 ) then
        n = made
        made = 0
        saved = 0
        y = 0.0D+00
        return
      else if ( n == 0 ) then
        return
      end if
    !
    !  Record the range of X we need to fill in.
    !
      x_lo_index = 1
      x_hi_index = n
    !
    !  Use up the old value, if we have it.
    !
      if ( saved == 1 ) then
        x(1) = y
        saved = 0
        x_lo_index = 2
      end if
    !
    !  Maybe we don't need any more values.
    !
      if ( x_hi_index - x_lo_index + 1 == 0 ) then
    !
    !  If we need just one new value, do that here to avoid null arrays.
    !
      else if ( x_hi_index - x_lo_index + 1 == 1 ) then
    
        r(1) = r8_uniform_01_func ( seed )
    
        if ( r(1) == 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_NORMAL_AB - Fatal error!'
          write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
          stop
        end if
    
        r(2) = r8_uniform_01_func ( seed )
    
        x(x_hi_index) = &
                 sqrt ( -2.0D+00 * log ( r(1) ) ) * cos ( 2.0D+00 * pi * r(2) )
        y =      sqrt ( -2.0D+00 * log ( r(1) ) ) * sin ( 2.0D+00 * pi * r(2) )
    
        saved = 1
    
        made = made + 2
    !
    !  If we require an even number of values, that's easy.
    !
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) == 0 ) then
    
        m = ( x_hi_index - x_lo_index + 1 ) / 2
    
        call r8vec_uniform_01 ( 2*m, seed, r )
    
        x(x_lo_index:x_hi_index-1:2) = &
          sqrt ( -2.0D+00 * log ( r(1:2*m-1:2) ) ) &
          * cos ( 2.0D+00 * pi * r(2:2*m:2) )
    
        x(x_lo_index+1:x_hi_index:2) = &
          sqrt ( -2.0D+00 * log ( r(1:2*m-1:2) ) ) &
          * sin ( 2.0D+00 * pi * r(2:2*m:2) )
    
        made = made + x_hi_index - x_lo_index + 1
    !
    !  If we require an odd number of values, we generate an even number,
    !  and handle the last pair specially, storing one in X(N), and
    !  saving the other for later.
    !
      else
    
        x_hi_index = x_hi_index - 1
    
        m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1
    
        call r8vec_uniform_01 ( 2*m, seed, r )
    
        x(x_lo_index:x_hi_index-1:2) = &
          sqrt ( -2.0D+00 * log ( r(1:2*m-3:2) ) ) &
          * cos ( 2.0D+00 * pi * r(2:2*m-2:2) )
    
        x(x_lo_index+1:x_hi_index:2) = &
          sqrt ( -2.0D+00 * log ( r(1:2*m-3:2) ) ) &
          * sin ( 2.0D+00 * pi * r(2:2*m-2:2) )
    
        x(n) = sqrt ( -2.0D+00 * log ( r(2*m-1) ) ) &
          * cos ( 2.0D+00 * pi * r(2*m) )
    
        y = sqrt ( -2.0D+00 * log ( r(2*m-1) ) ) &
          * sin ( 2.0D+00 * pi * r(2*m) )
    
        saved = 1
    
        made = made + x_hi_index - x_lo_index + 2
    
      end if
    
      x(1:n) = a + b * x(1:n)
    
      return
    end
    subroutine r8vec_print ( n, a, title )
    
    !*****************************************************************************80
    !
    !! R8VEC_PRINT prints an R8VEC.
    !
    !  Discussion:
    !
    !    If all the entries are integers, the data if printed
    !    in integer format.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    19 November 2002
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
    
      if ( all ( a(1:n) == aint ( a(1:n) ) ) ) then
        do i = 1, n
          write ( *, '(i6,i6)' ) i, int ( a(i) )
        end do
      else if ( all ( abs ( a(1:n) ) < 1000000.0D+00 ) ) then
        do i = 1, n
          write ( *, '(i6,f14.6)' ) i, a(i)
        end do
      else
        do i = 1, n
          write ( *, '(i6,g14.6)' ) i, a(i)
        end do
      end if
    
      return
    end
    subroutine r8vec_uniform_01 ( n, seed, r )
    
    !*****************************************************************************80
    !
    !! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    17 July 2006
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
    !    Bennett Fox,
    !    Algorithm 647:
    !    Implementation and Relative Efficiency of Quasirandom
    !    Sequence Generators,
    !    ACM Transactions on Mathematical Software,
    !    Volume 12, Number 4, pages 362-376, 1986.
    !
    !    Peter Lewis, Allen Goodman, James Miller
    !    A Pseudo-Random Number Generator for the System/360,
    !    IBM Systems Journal,
    !    Volume 8, pages 136-143, 1969.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of entries in the vector.
    !
    !    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
    !    NOT be 0.  On output, SEED has been updated.
    !
    !    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) k
      integer ( kind = 4 ) seed
      real ( kind = 8 ) r(n)
    
      do i = 1, n
    
        k = seed / 127773
    
        seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
        if ( seed < 0 ) then
          seed = seed + 2147483647
        end if
    
        r(i) = real ( seed, kind = 8 ) * 4.656612875D-10
    
      end do
    
      return
    end
    subroutine r8vec2_print ( n, a1, a2, title )
    
    !*****************************************************************************80
    !
    !! R8VEC2_PRINT prints an R8VEC2.
    !
    !  Discussion:
    !
    !    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
    !    as two separate vectors A1 and A2.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 December 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of components of the vector.
    !
    !    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a1(n)
      real ( kind = 8 ) a2(n)
      integer ( kind = 4 ) i
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
    
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, a1(i), a2(i)
      end do
    
      return
    end
    subroutine random_initialize ( seed )
    
    !*****************************************************************************80
    !
    !! RANDOM_INITIALIZE initializes the FORTRAN90 random number seed.
    !
    !  Discussion:
    !
    !    If you don't initialize the random number generator, its behavior
    !    is not specified.  If you initialize it simply by:
    !
    !      call random_seed ( )
    !
    !    its behavior is not specified.  On the DEC ALPHA, if that's all you
    !    do, the same random number sequence is returned.  In order to actually
    !    try to scramble up the random number generator a bit, this routine
    !    goes through the tedious process of getting the size of the random
    !    number seed, making up values based on the current time, and setting
    !    the random number seed.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    19 December 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, integer ( kind = 4 ) SEED.
    !    If SEED is zero on input, then you're asking this routine to come up
    !    with a seed value, which is returned as output.
    !    If SEED is nonzero on input, then you're asking this routine to
    !    use the input value of SEED to initialize the random number generator,
    !    and SEED is not changed on output.
    !
      implicit none
    
      integer ( kind = 4 ) count
      integer ( kind = 4 ) count_max
      integer ( kind = 4 ) count_rate
      logical, parameter :: debug = .false.
      integer ( kind = 4 ) i
      integer ( kind = 4 ) seed
      integer ( kind = 4 ), allocatable :: seed_vector(:)
      integer ( kind = 4 ) seed_size
      real ( kind = 8 ) t
    !
    !  Initialize the random number seed.
    !
      call random_seed ( )
    !
    !  Determine the size of the random number seed.
    !
      call random_seed ( size = seed_size )
    !
    !  Allocate a seed of the right size.
    !
      allocate ( seed_vector(seed_size) )
    
      if ( seed /= 0 ) then
    
        if ( debug ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'RANDOM_INITIALIZE'
          write ( *, '(a,i20)' ) '  Initialize RANDOM_NUMBER, user SEED = ', seed
        end if
    
      else
    
        call system_clock ( count, count_rate, count_max )
    
        seed = count
    
        if ( debug ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'RANDOM_INITIALIZE'
          write ( *, '(a,i20)' ) '  Initialize RANDOM_NUMBER, arbitrary SEED = ', &
            seed
        end if
    
      end if
    !
    !  Now set the seed.
    !
      seed_vector(1:seed_size) = seed
    
      call random_seed ( put = seed_vector(1:seed_size) )
    !
    !  Free up the seed space.
    !
      deallocate ( seed_vector )
    !
    !  Call the random number routine a bunch of times.
    !
      do i = 1, 100
        call random_number ( harvest = t )
      end do
    
      return
    end
    subroutine s_left ( s )
    
    !*****************************************************************************80
    !
    !! S_LEFT flushes a string left.
    !
    !  Discussion:
    !
    !    Both blanks and tabs are treated as "white space".
    !
    !  Examples:
    !
    !    Input             Output
    !
    !    '     Hello'      'Hello     '
    !    ' Hi there!  '    'Hi there!   '
    !    'Fred  '          'Fred  '
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    31 January 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, character ( len = * ) S.
    !
    !    On input, S is a string of characters.
    !
    !    On output, any initial blank or tab characters have been cut.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) lchar
      integer ( kind = 4 ) nonb
      character ( len = * ) s
      character, parameter :: TAB = char ( 9 )
    !
    !  Check the length of the string to the last nonblank.
    !  If nonpositive, return.
    !
      lchar = len_trim ( s )
    
      if ( lchar <= 0 ) then
        return
      end if
    !
    !  Find NONB, the location of the first nonblank, nontab.
    !
      nonb = 0
    
      do i = 1, lchar
    
        if ( s(i:i) /= ' ' .and. s(i:i) /= TAB ) then
          nonb = i
          exit
        end if
    
      end do
    
      if ( nonb == 0 ) then
        s = ' '
        return
      end if
    !
    !  Shift the string left.
    !
      if ( nonb > 1 ) then
        do i = 1, lchar + 1 - nonb
          s(i:i) = s(i+nonb-1:i+nonb-1)
        end do
      end if
    !
    !  Blank out the end of the string.
    !
      s(lchar+2-nonb:lchar) = ' '
    
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
    end module