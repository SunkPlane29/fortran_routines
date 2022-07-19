program derivada
  implicit none

  real*8 f1, x, h, der1, pi
  integer n, i

  pi = 4.D0*DATAN(1.D0)

  h = 1.0
  n = 1000
  x = pi / 2

  do i = 1, n
     h = h / 2

     der1 = (f1(x + h) - f1(x))/h
     print *, 'x: ', x, 'h: ', h, 'f(x): ', f1(x), 'f(x+h): ', f1(x+h), 'derivada: ', der1
  end do

end program derivada

real*8 function f1(x)
  real*8 x
  f1 = sin(x)
  return
end function f1
