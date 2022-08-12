program fibonaccilis
  implicit none

  external fib, eh_primo

  integer fib, n, fib_val, i
  logical eh_primo, primo

  n = 10

  do i = 1, n
     fib_val = fib(i)
     primo = eh_primo(fib_val)
     print *, fib_val
     if (primo) then
        print *, "eh primo"
     else
        print *, "não eh primo"
     end if
  end do
end program fibonaccilis

integer function fib(n)
  implicit none

  integer n, anterior, atual, proximo, i

  if (n == 0) then
     fib = 0
     return
  end if

  anterior = 0
  atual = 1

  do i = 1, n
     proximo = anterior + atual
     anterior = atual
     atual = proximo
  end do

  fib = proximo
end function fib

logical function eh_primo(n)
  implicit none

  integer n, i

  eh_primo = .true.

  if (n == 1)

  do i = 2, n/2
     if(mod(n, i) == 0) then
        eh_primo = .false.
        return
     end if
  end do
end function eh_primo
