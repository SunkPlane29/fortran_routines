program derivada
  implicit none

  external f1

  real*8 f1, x, xmin, xmax, dx, h, der1, first_val, second_val
  integer n, i, j, nx

  h = 1.0
  n = 20
  xmin = -5.0
  xmax = 5.0
  nx = 20
  dx = (xmax - xmin) / dfloat(nx - 1)

  open(unit=0, file='grafico_funcao.dat')
  open(unit=1, file='grafico_derivada.dat')

  do j = 0, nx - 1

     x = xmin + dfloat(j) * dx
     h = 1.0
     do i = 1, n

        second_val = f1(x + h)
        first_val = f1(x - h)
        der1 = (second_val - first_val) / (2*h)

        h = h / 2.0
     end do

     write(0,*) x, f1(x)
     write(1,*) x, der1
  end do

end program derivada

real*8 function f1(x)
  implicit none

  real*8 x

  f1 = x**3 + 2*x - 1
  return

end function f1
