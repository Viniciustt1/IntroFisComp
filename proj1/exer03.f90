program exer03
    IMPLICIT NONE 
    ! SIEVE OF ERATOSTHENES
    INTEGER :: i, j, M
    LOGICAL, DIMENSION(:), ALLOCATABLE :: A
  
    print*, 'Entre com um número: '
    read*, M

    allocate(A(2:M)) 
    A = .TRUE.
    do i = 2, floor(sqrt(real(M))) 
     if (A(i) .eqv. .TRUE.) then
      do j = i**2, M, i
       A(j) = .FALSE.
      end do
     end if 
    end do
    
    open(1,file = 'primos_out.dat', status='replace')

    do i = 2, M 

     if (A(i) .neqv. .FALSE.) then
      write(1,*) i 
     end if

    end do
    close(1)

end program
