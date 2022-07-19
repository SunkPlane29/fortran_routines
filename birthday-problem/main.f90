program birthday
  implicit none

  real*8 calculate_probability, probability_percentage
  integer n_people

  print *, 'how many people are we calculating? '
  read (*,*) n_people
  print *, 'the probability of finding two mathing birthdays is '

  probability_percentage = calculate_probability(n_people)

  print *, probability_percentage

end program birthday

! this function calculates the probability of finding two people with the same birthday
! given the number of people and returns its decimal representation, the formula is given
!
! probability (n = 3) = 1 - (364/365) * (363/365)
! probability (n = 4) = 1 - (364/365) * (363/365) * (362/365)
! ...
!
! ps: recursion was not used because I (the author of the code) have a personal problem
! with recursion, I cannot wrap my head arround it. For me iterative approches are better
! both in readability and performance
real*8 function calculate_probability(n_people) result(probability_percentage)
  implicit none

  integer n_people, i
  ! probability describes the chances of not having two people with the same birthday
  ! which starts high, current_odds describes the new probability to multiply the base
  ! probability
  real*8 probability, n_days_in_year, current_odds

  n_days_in_year = 365.0
  current_odds = 364.0

  probability = current_odds / n_days_in_year

  ! here the counter starts at 1 because i'm a programmer, and the limit of the loop
  ! has a - 2 because fortran has a closed interval in this number and one of the
  ! factors was already computed (364/365)
  do i = 0, n_people - 2
     current_odds = current_odds - 1
     probability = probability * (current_odds / n_days_in_year)
  end do

  probability_percentage = 1 - probability

end function calculate_probability
