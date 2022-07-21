program integral
  implicit none

  external integrate

  real*8 :: integralis
  call integrate(f1, 1.0d0, 2.0d0, integralis)

  print *, integralis

contains
  function f1(x) result(y)
    implicit none

    real*8 :: x, y
    y = x**2 - 1
  end function f1

end program integral

subroutine integrate(f, xmin, xmax, integral)
  implicit none

  real*8, intent(in) :: xmin, xmax
  real*8, intent(out) :: integral
  real*8 :: h, x
  integer :: i
  interface
     function f(x) result(y)
       real*8 :: x, y
     end function f
  end interface

  integral = 0.0
  h = (xmax - xmin) / 9000
  do i = 1, 9000
     x = xmin + (i - 1) * h
     integral = integral + f(x) * h
  end do
end subroutine integrate
