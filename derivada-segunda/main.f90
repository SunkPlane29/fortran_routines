program teste
  implicit none

  real*8 derivada_segunda

  print *, derivada_segunda(1.0d0)
end program teste

function derivada_segunda(x) result(derivada)
  implicit none

  real*8 x, derivada
  real*8 f1
  real*8 h, valor_direito, valor_esquerdo
  integer i, n

  n = 20
  h = 1.0

  do i = 1, n
     valor_direito = f1(x + h/2)
     valor_direito = f1(x - h/2)
     derivada = (valor_direito - (2*f1(x)) + valor_esquerdo)/(h**2)
  end do

end function derivada_segunda

function f1(x) result(y)
  implicit none

  real*8 x, y
  y = x**3
end function f1
