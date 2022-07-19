program raiz_bissec
  implicit none

  real*8 x_min, x_max, root, find_roots

  x_min = -3.0
  x_max = 0.0

  root = find_roots(x_min, x_max)
  print *, root

end program raiz_bissec

! it starts from the assumption that the functions values changes
! sign inside this interval, but if this doesn't happen it doesn't
! mean that there is no root. maybe there is a workaround like dividing
! the interval in half a couple of times and checking this sign changing
! property (but doing this you need to check both halves)
function find_roots(x_min, x_max) result(root)
  implicit none

  real*8 x_min, x_max, x_med, precision, root, f1
  logical calculate_sign_change

  precision = 1.0e-20

  if (calculate_sign_change(x_min, x_max)) then
     ! this conditional is only here as a placeholder, will possibly change
     do while (abs(f1(x_med)) > precision)
        x_med = (x_min + x_max) / 2

        if (calculate_sign_change(x_min, x_med)) then
           x_max = x_med
        else if (calculate_sign_change(x_med,x_max)) then
           x_min = x_med
        end if
     end do
  else
     print *, 'function does not change value from start of interval to end of interval'
     !TODO: test if the method i thought work
  end if

  root = x_med

  print *, 'root is ', root

end function find_roots

! calculate_sign_change returns true when the function changes values from
! point a to point b
function calculate_sign_change(a, b) result(sign_change)
  implicit none

  real*8 a, b, f1, f1a, f1b
  logical sign_change

  f1a = f1(a)
  f1b = f1(b)

  sign_change = abs(f1a + f1b) < (abs(f1a) + abs(f1b))

end function calculate_sign_change

function f1(x) result(f1x)
  implicit none

  real*8 x, f1x

  f1x = x**2 - x - 1
end function f1
