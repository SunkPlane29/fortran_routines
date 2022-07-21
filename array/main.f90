program array
  implicit none

  ! interface required for allocatable argument
  interface
     subroutine append_element(arr, element)
       integer, intent(in) :: element
       integer, allocatable, intent(inout) :: arr(:)
     end subroutine append_element
  end interface

  integer, allocatable :: arr(:)
  integer :: i, n, fib

  external array_subroutine

  n = 20

  do i = 1, n
     call append_element(arr, fib(i))
  end do

  print *, arr

end program array

integer function fib(n)
  implicit none

  integer, intent(in) :: n
  integer :: previous, current, i

  if (n == 0) then
     fib = 0
     return
  end if

  previous = 0
  current = 1

  do i = 1, n
     fib = previous + current
     previous = current
     current = fib
  end do

end function fib

! this subroutine copies the elements from the array given and dealocates it,
! then it allocates the array again but with an added element (no extra capacity)
subroutine append_element(arr, element)
  implicit none

  integer, intent(in) :: element
  integer, allocatable, intent(inout) :: arr(:)
  integer, allocatable :: arr_cp(:)
  integer arr_size

  if (allocated(arr) .eqv. .false.) then
     allocate(arr(0))
  end if

  arr_size = size(arr)

  allocate(arr_cp(arr_size))
  arr_cp = arr

  deallocate(arr)
  allocate(arr(arr_size+1))
  arr = [arr_cp(:), element]
end subroutine append_element
