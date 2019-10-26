program linear_regression

  implicit none
  real :: x(10), y(10), b(2)

  interface
     subroutine estimate_coef(x, y, b)
       real, intent(in) :: x(:), y(:)
       real, intent(out) :: b(2)
       real :: m_x, m_y
       integer :: n
     end subroutine estimate_coef
  end interface

  x = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
  y = (/ 1, 3, 2, 5, 7, 8, 8, 9, 10, 12 /)

  call estimate_coef(x, y, b)

  print *, " Estimated coefficients in Fortran: "
  print *, "b(1) = ", b(1)
  print *, "b(2) = ", b(2)

  stop
  
end program linear_regression

subroutine estimate_coef(x, y, b)

  real, intent(in) :: x(:), y(:)
  real, intent(out) :: b(2)
  real :: m_x, m_y
  integer :: n

  ! number of observations/points
  n = size(x)

  ! mean of x and y vector
  m_x = sum(x)/n 
  m_y = sum(y)/n

  ! calculating cross-deviation and deviation about x
  SS_xy = sum(y*x) - n*m_y*m_x
  SS_xx = sum(x*x) - n*m_x*m_x 
  
  ! calculating regression coefficients 
  b(2) = SS_xy / SS_xx 
  b(1) = m_y - b(2)*m_x 

  return
  
end subroutine estimate_coef
