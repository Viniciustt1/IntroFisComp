FUNCTION f(x)

real(8), intent(in) :: x
real(8) :: f 

f = sin(x/2)*cos(x)

END FUNCTION

program exerB
    IMPLICIT NONE
    integer :: m, i, j, k
    real(8), ALLOCATABLE, dimension(:) :: h,  e_trap, e_simp, e_bode !erros das integrais
    integer, ALLOCATABLE, dimension(:) :: N
    real(8) :: int, trap, simp, bode
    real(8) :: x0, x1, x2, x3, x4, f

    int = (cos(1.0d0/2)-(cos(3*1.0d0/2))/3) - (cos(0.0d0/2)-(cos(3*0.0d0/2))/3) !integral definida calculada diretamente

    open(6, file = "tabB_out.dat", status = "unknown")
    open(5, file = "tabB_in.dat", status = "old")
    read(5,*) m 
    ALLOCATE(h(m), e_trap(m), e_simp(m), e_bode(m), N(m))
    read(5,*) (N(i), i = 1, m)
    close(5)

    do i = 1, m
      h(i) = 1.0d0/N(i)
    end do
   
    do i = 1, m 
      trap = 0.0d0
      simp = 0.0d0
      bode = 0.0d0
      do j = 0, N(i), 2
        if (j == N(i)) then 
          e_trap(i) = abs(trap - int)
          e_simp(i) = abs(simp - int)
        else 
          x1 = j*h(i) !atrás
          x0 = (j + 1.0d0)*h(i)
          x2 = (j + 2.0d0)*h(i) !frente
          trap = trap + ((h(i)/2)*(f(x2) + 2.0d0*f(x0) + f(x1)))
          simp = simp + ((h(i)/3)*(f(x2) + 4.0d0*f(x0) + f(x1)))
        end if
      end do

      do k = 0, N(i), 4
        if (k == N(i)) then 
          e_bode(i) = abs(bode - int)
        else 
          x0 = k*h(i)
          x1 = (k + 1.0d0)*h(i)
          x2 = (k + 2.0d0)*h(i)
          x3 = (k + 3.0d0)*h(i)
          x4 = (k + 4.0d0)*h(i)
          bode = bode + ((2.0d0*h(i)/45)*(7*f(x0) + 32.0d0*f(x1) + 12.0d0*f(x2) + 32.0d0*f(x3) + 7.0d0*f(x4)))
        end if
      end do
      write(6,*) N(i), h(i), e_trap(i), e_simp(i), e_bode(i)
    end do
    close(6)
end program

    


          

       
    
