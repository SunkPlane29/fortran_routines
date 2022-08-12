program main
  implicit none

  external f

  real*8 :: f, a, b, integral_trap, integral_rect
  integer :: n_shapes

  a = 1.0
  b = 2.0
  n_shapes = 100000

  call integral_retangulo(f, a, b, n_shapes, integral_rect)
  call integral_trapezio(f, a, b, n_shapes, integral_trap)

  print *, ""

  print *, "*** valor da integral definida de f em a = ", a, " e b = ", b
  print *, "*** calculada usando ", n_shapes, " trapézios: "
  print *, "*** ", integral_trap

  print *, ""
  print *, "------------------------------------------------------------"
  print *, ""

  print *, "*** valor da integral definida de f em a = ", a, " e b = ", b
  print *, "*** calculada usando ", n_shapes, " retâgulos "
  print *, "*** ", integral_rect

  print *, ""
end program main

function f(x) result(y)
  implicit none

  real*8 :: x, y

  y = x**2 - 1

end function f

subroutine integral_retangulo(f, a, b, n_rect, integral)
  implicit none

  ! esse interface e f deixam o codigo um pouco mais versatil
  ! f pode ser qualquer funcao definida pelo usuario que aceite
  ! uma variavel real como argumento e retorne um valor real
  interface
     function f(x) result(y)
       implicit none

       real*8 :: x, y
     end function f
  end interface

  real*8, intent(in) :: a, b
  integer, intent(out) :: n_rect
  real*8, intent(out) :: integral

  real*8 :: delx
  integer :: i

  delx = (b-a)/n_rect

  integral = 0

  do i = 0, n_rect-1
     integral = integral + f(a + i*delx)
  end do

  integral = integral * delx

end subroutine integral_retangulo

subroutine integral_trapezio(f, a, b, n_trap, integral)
  implicit none

  ! esse interface e f deixam o codigo um pouco mais versatil
  ! f pode ser qualquer funcao definida pelo usuario que aceite
  ! uma variavel real como argumento e retorne um valor real
  interface
     function f(x) result(y)
       implicit none

       real*8 :: x, y
     end function f
  end interface

  real*8, intent(in) :: a, b
  integer, intent(in) :: n_trap
  real*8, intent(out) :: integral

  real*8 :: delx
  integer :: i

  delx = (b-a)/n_trap

  integral = f(a) + f(b)

  ! esses indices do loop sao meio estranhos porque o primeiro e
  ! o ultimo valor tem que ser excluidos do loop, pois ja foram
  ! computados
  do i = 1, n_trap-1
    integral = integral + 2*f(a + i*delx)
  end do

  integral = integral * (delx/2)

end subroutine integral_trapezio
