module integral
    implicit none

contains
    function calculate_integral_rectangle(f, xmin, xmax, n) result(sum)
        implicit none
        real*8 xmin, xmax, sum, delx
        integer n, i
        interface
            function f(x) result(y)
                real*8 x, y
            end function f
        end interface

        delx = (xmax - xmin) / n
        sum = 0.0

        do i = 1, n
            sum = sum + f(xmin + (i - 1) * delx) * delx
        end do 
    end function calculate_integral_rectangle

    function calculate_integral_trapezoid(f, xmin, xmax, n) result(sum)
        implicit none

        real*8 xmin, xmax, sum, delx
        integer n, i
        interface
            function f(x) result(y)
                real*8 x, y
            end function f
        end interface

        delx = (xmax - xmin) / n
        sum = 0.0

        do i = 1, n
            sum = sum + f(xmin + (i - 1) * delx)
        end do
        sum =  (delx / 2.0)*(f(xmin) + 2.0*sum + f(xmax))
    end function calculate_integral_trapezoid
end module integral