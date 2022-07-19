program derivada
  implicit none

  external f1

  real*8 f1, x, h, der1, first_val, second_val
  integer n, i

  x = 1.0
  h = 1.0
  n = 20

  do i = 1, n

     second_val = f1(x + h)
     first_val = f1(x - h)
     der1 = (second_val - first_val) / (2*h)

     print *, 'x = ', x, ', h = ', h, ', f(x - h) = ', first_val, ', f(x + h) = ', second_val, ', derivada = ', der1

     h = h / 2.0
  end do

end program derivada

real*8 function f1(x)
  implicit none

  real*8 x

  f1 = x**3 + 2*x - 1
  return

end function f1
