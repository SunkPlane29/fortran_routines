program fibonacci
  implicit none

  integer fibonacci_iterative, fibonacci_recursive, i, n
  n = 100

  ! print *, 'Número de números na sequência? '
  ! read(*,*) n

  ! print *, ''

  do i = 1, n
     print *, fibonacci_iterative(i)
  end do

  ! print *, ''

  ! do i = 1, n
  !    print *, fibonacci_recursive(i+1)
  ! end do

end program fibonacci

function fibonacci_iterative(n) result(fib)
  implicit none

  integer previous_val, current_val, fib
  integer n, i

  previous_val = 0
  current_val = 1

  !TODO: fix that helps visualize the comparison between the recursive and iterative
  !approach, should be returned to n (from n-1)
  do i=1, n
     fib = previous_val + current_val
     previous_val = current_val
     current_val = fib
  end do
end function fibonacci_iterative

recursive function fibonacci_recursive(n) result(fib)
  implicit none

  integer n, fib

  if (n < 2) then
     fib = n
     return
  endif

  fib = fibonacci_recursive(n-2) + fibonacci_recursive(n-1)
end function fibonacci_recursive
