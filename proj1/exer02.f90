subroutine sin_s(x, termos) !subrotina para precisão simples
    implicit none
    integer :: fat, n, i
    real(4), intent(in) :: x
    real(4), dimension(2), intent(out) :: termos
    real(4) :: sin1 = 0.e0, precisao, S1, last_S1
    real(4), dimension(2) :: termos_s
    


    n = 1
    do
     fat = 1
     do i = 1, 2*n - 1 !cálculo do fatorial
      fat = fat*i
     end do
     S1 = (((-1)**(n+1)) * (x**(2*n-1))) / fat !termos da série de Taylor do sin
     if (sin1 .ne. sin1 + S1) then
      last_S1 = S1
      sin1 = sin1 + S1
      n = n + 1
     else 
      exit
     end if
    end do

     precisao = abs(last_S1)/sin1 !divide o último termo QUE MODIFICA pela valor do sin
     termos(1) = precisao
     termos(2) = sin1
     
     n = 1
     sin1 = 0 !reinicializando os valores
    
end subroutine sin_s

subroutine sin_d(x, termos) !subrotina para precisão dupla
    implicit none
    integer :: fat, n, i
    real(8), intent(in) :: x
    real(8), dimension(2), intent(out) :: termos
    real(8) :: sin2 = 0.d0, precisao, S2, last_S2
    real(8), dimension(2) :: termos_d
    
    
    n = 1
    do
     fat = 1
     do i = 1, 2*n - 1 !cálculo do fatorial
      fat = fat*i
     end do
     S2 = (((-1)**(n+1)) * (x**(2*n-1))) / fat !termos da série de Taylor do sin
     if (sin2 .ne. sin2 + S2) then
      last_S2 = S2
      sin2 = sin2 + S2
      n = n + 1 !atualiza os valores
     else 
      exit
     end if
    end do

     precisao = abs(last_S2)/sin2 !divide o último termo QUE MODIFICA pela valor do sin
     termos(1) = precisao
     termos(2) = sin2

    n = 1
    sin2 = 0

    
end subroutine sin_d

program exer02
    implicit none
    integer :: fat, i, n 
    real(4) :: x1 = 0.1e0, y1 = 0.2e0, z1 = 0.3e0, w1 = 0.4e0 
    real(4), dimension(2) :: termos_s
    real(8) :: x2 = 0.1d0, y2 = 0.2d0, z2 = 0.3d0, w2 = 0.4d0
    real(8), dimension(2) :: termos_d

    print*, "PRECISÃO SIMPLES"

    ! x = 0.1:

    call sin_s(x1, termos_s)
    print *, x1, termos_s(1)
    
    ! x = y = 0.2:

    call sin_s(y1, termos_s)
    print *, y1, termos_s(1)

    ! x = z = 0.3:

    call sin_s(z1, termos_s)
    print *, z1, termos_s(1)

    ! x = w = 0.4:

    call sin_s(w1, termos_s)
    print *, w1, termos_s(1)

    print*, "PRECISÃO DUPLA"

    ! x = 0.1:

    call sin_d(x2, termos_d)
    print *, x2, termos_d(1)
    
    ! x = y = 0.2:

    call sin_d(y2, termos_d)
    print *, y2, termos_d(1)

    ! x = z = 0.3:

    call sin_d(z2, termos_d)
    print *, z2, termos_d(1)

    ! x = w = 0.4:

    call sin_d(w2, termos_d)
    print *, w2, termos_d(1)

end program