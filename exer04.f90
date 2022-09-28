module functions
implicit none

contains
FUNCTION orden(val, n) ! função que ordena uma array em ordem crescente excluindo termos iguais
        integer :: m = 0, n
        real :: valor_min, valor_max
        real, dimension(n) :: val, y
        real, dimension(:), allocatable :: orden
        
        valor_min = minval(val)-1
        valor_max = maxval(val)
        
        do while (valor_min<valor_max)
            m = m+1
            valor_min = minval(val, mask=val>valor_min)
            y(m) = valor_min
        enddo
        
        if(allocated(orden)) then
            orden = y(1:m)
        else
            allocate(orden(m), source=y(1:m))
        endif
    END FUNCTION orden

end module functions

program exer04
    use functions
    IMPLICIT NONE
    real, dimension(3) :: v1, v2, v3, v4, v5, v6
    real :: a1, a2, a3, a4, vol, a_t, x
    integer :: i, n = 3, j, m
    real, dimension(4) :: sort_a

    v1 = (/0,1,0/) !vetor da base AC
    v2 = (/-1,0,0/) !vetor da base AB
    v3 = (/-1,-1,0/) !vetor da base BC
    v4 = (/0,0,1/) !vetor da altura AD

    !abre o arquivo vet_in.dat e escreve v1, v2, v3 e v4 nele
    open(2, file = 'vet_in.dat', status = 'replace')
    
    write(2,*) v1(1), v1(2), v1(3)
    write(2,*) v2(1), v2(2), v2(3)
    write(2,*) v3(1), v3(2), v3(3)
    write(2,*) v4(1), v4(2), v4(3)

    close(2) !fecha o arquivo


     !abre o arquivo vet_in.dat e le os vetores (um por linha) presentes nele
    open(2, file = 'vet_in.dat', status = 'old')
    
    read(2,*) (v1(i), i = 1, n)
    read(2,*) (v2(i), i = 1, n)
    read(2,*) (v3(i), i = 1, n)
    read(2,*) (v4(i), i = 1, n)

    close(2) !fecha o arquivo
   
    print*, v1(1), v1(2), v1(3)
    print*, v2(1), v2(2), v2(3)
    print*, v3(1), v3(2), v3(3)
    print*, v4(1), v4(2), v4(3)

    v5 = v1 - v4
    v6 = v2 - v4
    
    a1 = sqrt((v1(2)*v2(3)-v1(3)*v2(2))**2 + (v1(3)*v2(1)-v2(3)*v1(1))**2 + (v1(1)*v2(2)-v1(2)*v2(1))**2)/2
    a2 = sqrt((v1(2)*v4(3)-v1(3)*v4(2))**2 + (v1(3)*v4(1)-v4(3)*v1(1))**2 + (v1(1)*v4(2)-v1(2)*v4(1))**2)/2
    a3 = sqrt((v2(2)*v4(3)-v2(3)*v4(2))**2 + (v2(3)*v4(1)-v4(3)*v2(1))**2 + (v2(1)*v4(2)-v2(2)*v4(1))**2)/2
    a4 = sqrt((v5(2)*v6(3)-v6(2)*v5(3))**2 + (v5(3)*v6(1)-v5(1)*v6(3))**2 + (v5(1)*v6(2)-v6(1)*v5(2))**2)/2
    vol = abs((v4(1)*v1(2)*v2(3) + v4(2)*v1(3)*v2(1) + v4(3)*v1(1)*v2(2)) &
            - (v2(1)*v1(2)*v4(3) + v2(2)*v1(3)*v4(1) + v2(3)*v1(1)*v4(2)))/6

    sort_a = (/a1,a2,a3,a4/)
  
    a_t = a1+a2+a3+a4

    open(3, file = "tetra_out.dat", status = "replace")
    write(3,*) vol
    write(3,*) a_t
    write(3,*) orden(sort_a, 4)
    close(3)

end program