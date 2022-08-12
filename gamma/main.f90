program gamma
  implicit none

  complex, dimension(4, 4) :: const, gamma0, gamma1, gamma2, gamma3, gamma5
  complex :: trace

  const = reshape((/(0, 1), (0, 0), (0, 0), (0, 0), &
                    (0, 0), (0, 1), (0, 0), (0, 0), &
                    (0, 0), (0, 0), (0, 1), (0, 0), &
                    (0, 0), (0, 0), (0, 0), (0, 1)/), shape(const), order=(/2, 1/))

  gamma0 = reshape((/(1, 0), (0, 0), (0, 0), (0, 0), &
                    (0, 0), (1, 0), (0, 0), (0, 0), &
                    (0, 0), (0, 0), (-1, 0), (0, 0), &
                    (0, 0), (0, 0), (0, 0), (-1, 0)/), shape(gamma0), order=(/2, 1/))

  gamma1 = reshape((/(0, 0), (0, 0), (0, 0), (1, 0), &
                    (0, 0), (0, 0), (1, 0), (0, 0), &
                    (0, 0), (-1, 0), (0, 0), (0, 0), &
                    (-1, 0), (0, 0), (0, 0), (0, 0)/), shape(gamma1), order=(/2, 1/))

  gamma2 = reshape((/(0, 0), (0, 0), (0, 0), (0, -1), &
                    (0, 0), (0, 0), (0, 1), (0, 0), &
                    (0, 0), (0, 1), (0, 0), (0, 0), &
                    (0, -1), (0, 0), (0, 0), (0, 0)/), shape(gamma2), order=(/2, 1/))

  gamma3 = reshape((/(0, 0), (0, 0), (1, 0), (0, 0), &
                    (0, 0), (0, 0), (0, 0), (-1, 0), &
                    (-1, 0), (0, 0), (0, 0), (0, 0), &
                    (0, 0), (1, 0), (0, 0), (0, 0)/), shape(gamma3), order=(/2, 1/))

  print *, ""
  print *, "gamma0: "
  call print_matrix(gamma0)

  print *, ""
  print *, "gamma1: "
  call print_matrix(gamma1)

  print *, ""
  print *, "gamma2: "
  call print_matrix(gamma2)

  print *, ""
  print *, "gamma3: "
  call print_matrix(gamma3)

  gamma5 = matmul(const, matmul(matmul(gamma0, gamma1), &
                                matmul(gamma2, gamma3)))

  print *, ""
  print *, "gamma5: "
  call print_matrix(gamma5)
  print *, ""

  print *, ""
  print *, "gamma5 trace: "
  call matrix_trace(gamma5, 4, trace)
  print *, trace
  print *, ""

end program gamma

subroutine print_matrix(matrix)
  implicit none

  complex, dimension(4, 4) :: matrix
  integer i, j

  do i = 1, 4
     print "(*('('sf6.2xspf6.2x'i)':x))", (matrix(i, j), j=1,4)
  end do

end subroutine print_matrix

subroutine matrix_trace(matrix, n, trace)
  implicit none

  integer, intent(in) :: n
  complex, dimension(n, n), intent(in) :: matrix
  complex, intent(out) :: trace

  integer :: i, j

  trace = (0, 0)

  do i = 1, n
     do j = 1, n
        if (i == j) then
           trace = trace + matrix(i, j)
        end if
     end do
  end do
end subroutine matrix_trace
