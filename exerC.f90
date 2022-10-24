FUNCTION f(x)

real(8), intent(in) :: x
real(8) :: f 

f = x**3 - x**2 - 2*x + 1

END FUNCTION

FUNCTION dev(x)

real(8), intent(in) :: x 
real(8) :: dev

dev = 3.0d0*x**2 -2.0d0*x - 2.0d0

END FUNCTION

program exerC
    IMPLICIT NONE
    real(8) :: r1_d, r2_d, r3_d, r1_n, r2_n, r3_n, r1_s0, r1_s1, r2_s0, r2_s1, r3_s0, r3_s1
    real(8) :: f, dev, e
    real(8), allocatable, dimension(:) :: list_nr, list_sec
    integer :: i, n
    
    print*, 'Entre com o número de interações: \n'
    read(*,*) n 
    open(7, file = 'tabC_out.dat', status = 'unknown')
    write(7,*) '  INTER             ', '          DIR1                 ', '     DIR2               ', &
     '       DIR3               ', '       NR1               ', '         NR2               ', &
      '        NR3                 ', &
    '    SEC1                  ', '     SEC2                ', 'SEC3               '
    
    !chutes iniciais para a busca direta
    r1_d = 1.9d0
    r2_d = 0.55d0
    r3_d = -1.35d0
    e = 0.03d0

    !chutes iniciais para o método de Newton-Raphson
    r1_n = 1.75d0
    r2_n = 0.35d0
    r3_n = -1.15d0


    !chutes iniciais para o método da secante
    r1_s0 = 1.85d0
    r1_s1 = 1.95d0

    r2_s0 = 0.39d0
    r2_s1 = 0.47d0

    r3_s0 = -1.27d0
    r3_s1 = -1.19d0

!-------------------------------------------------------------------------------------------------------------------------
    !Loop para busca direta:
    do i = 1, n, 1
     if (f(r1_d + e) > 0) then !Quando a função inverte de sinal, encerra-se a busca
       r1_d = r1_d + e
     end if

     if (f(r2_d + e) > 0) then
       r2_d = r2_d + e
     end if

     if (f(r3_d - e) < 0) then
       r3_d = r3_d - e
     end if
!-------------------------------------------------------------------------------------------------------------------------
      !Método de Newton-Raphson:
      r1_n = r1_n - (f(r1_n)/dev(r1_n)) !ponto de intersecção com o eixo x
      r2_n = r2_n - (f(r2_n)/dev(r2_n)) !ponto de intersecção com o eixo x
      r3_n = r3_n - (f(r3_n)/dev(r3_n)) !ponto de intersecção com o eixo x

!-------------------------------------------------------------------------------------------------------------------------
      !Método da secante:
      r1_s1 = r1_s1 - f(r1_s1)*((r1_s1 - r1_s0)/f(r1_s1) - f(r1_s0)) 

      r2_s1 = r2_s1 - f(r2_s1)*((r2_s1 - r2_s0)/f(r2_s1) - f(r2_s0))

      r3_s1 = r3_s1 - f(r3_s1)*((r3_s1 - r3_s0)/f(r3_s1) - f(r3_s0)) 
    

      write(7,*) i, r1_d, r2_d, r3_d, r1_n, r2_n, r3_n, r1_s1, r2_s1, r3_s1

    end do
    close(7)
    

end program
