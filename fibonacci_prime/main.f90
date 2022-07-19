program fibonacci
  implicit none

  integer fib, i, n, fib_val
  logical is_prime
  n = 15

  do i = 1, n
     fib_val = fib(i)
     if (is_prime(fib_val)) then
        print *, fib_val
     end if
  end do

end program fibonacci

function fib(n) result(fib_val)
  implicit none

  integer previous_val, current_val, fib_val
  integer n, i

  previous_val = 0
  current_val = 1

  do i=1, n
     fib_val = previous_val + current_val
     previous_val = current_val
     current_val = fib_val
  end do
end function fib

function is_prime(n) result(prime)
  implicit none

  integer n, i
  logical prime

  prime = .true.

  do i = 2, n/2
     if (mod(n, i) == 0) then
        prime = .false.
        return
     end if
  end do
end function is_prime
