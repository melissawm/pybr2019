program simple_pi

  ! To run, use
  ! $ gfortran -O3 -fcoarray=lib simple_pi.f90 -lcaf_mpi -o simple_pi
  ! $ cafrun -np 4 ./simple_pi

  !! implements calculation:
  !! $$ \pi = \int_{-1}^1 \frac{dx}{\sqrt{1-x^2}}$$
  
  use, intrinsic:: iso_fortran_env, only: dp=>real64, int64, stderr=>error_unit
  implicit none
  
  integer, parameter :: wp = dp
  real(wp), parameter :: x0 = -1.0_wp, x1 = 1.0_wp
  real(wp), parameter :: pi = 4._wp*atan(1.0_wp)
  real(wp) :: psum[*]  ! this is a scalar coarray
  integer(int64) :: rate,tic,toc
  real(wp) :: f,x,telaps, dx
  integer :: i, stat, im, Ni
  
  psum = 0._wp
  
  dx = 1e-9_wp ! resolution
  
  Ni = int((x1-x0) / dx)    ! (1 - (-1)) / interval
  im = this_image()
  
  !---------------------------------
  if (im == 1) then
     call system_clock(tic)
     print *, "========================================="
     print *,'approximating pi in ',Ni,' steps.'
     print *, "========================================="
  end if
  !---------------------------------
  
  do i = im, Ni-1, num_images() ! Each image works on a subset of the problem
     x = x0 + i*dx
     f = dx / sqrt(1.0_wp - x**2)
     psum = psum + f
  end do
  
  call co_sum(psum)
  
  if (im == 1)  then
     print *,'pi:',pi,'  iterated pi: ',psum
     print '(A,E10.3)', 'pi error',pi - psum
  endif
  
  if (im == 1) then
     call system_clock(toc)
     call system_clock(count_rate=rate)
     telaps = real((toc - tic), wp)  / rate
     print '(A,E9.3,A,I3,A)', 'Elapsed wall clock time ', telaps, ' seconds, using',num_images(),' images.'
  end if
  
end program simple_pi
