program sequencia
	implicit none
	integer n1,n2,n3, ix, i, p
	logical divisivel
	write(*,*) 'Esse programa serve para calcular a sequencia de fibonacci e primos, escreva quantas casas voce quer:(exceto o 0)'
	read(*,*) ix
	n1=0
	n2=1
	do i=1,ix
		divisivel = .false.
		n3=n1+n2
		write(*,*) n3
		do p=2,(n3/2)
			write (*,*) p, mod(n3, p)
			if (mod(p,n3) == 0) then
				divisivel = .true.
			end if
		enddo
		
		if (divisivel .eqv. .false.) then
			write(*,*) n3, 'primo'
		end if
		n1=n2
		n2=n3
	enddo
	stop
end
