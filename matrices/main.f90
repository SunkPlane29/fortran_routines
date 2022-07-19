program matrices
  implicit none

  real, dimension(2, 2) :: m1, m2
  real, dimension(3, 3) :: m3, m4

  m1 = transpose(reshape((/ 1.9, 4.4, 1.0, 9.0 /), shape(m1)))
  m2 = transpose(reshape((/ 8.1, 5.6, 9.0, 1.0 /), shape(m2)))

  print *, 'm1 + m2: ', m1 + m2, NEW_LINE('a')
  print *, 'm1 * 10: ', m1 * 10, NEW_LINE('a')
  print *, 'm1 * m2: ', m1 * m2, NEW_LINE('a')

  print *, NEW_LINE('a')

  m3 = transpose(reshape((/ 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0 /), shape(m3)))
  m4 = transpose(reshape((/ 9.0, 9.0, 9.0, 8.0, 8.0, 8.0, 7.0, 7.0, 7.0 /), shape(m4)))

  print *, 'm3 + m4: ', m3 + m4, NEW_LINE('a')
  print *, 'm3 * 10: ', m3 * 10, NEW_LINE('a')
  print *, 'm3 * m4: ', m3 * m4, NEW_LINE('a')

end program matrices
