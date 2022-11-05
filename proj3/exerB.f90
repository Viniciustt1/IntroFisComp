program exerB
    IMPLICIT NONE
    real(8), parameter :: P = 400.0d0, m = 80.0d0, d = 1.2d0, C = 0.5d0
    integer :: i
    real(8) :: T, Dt, v01, v02, v03, v1, v2, v3, A1, A2, A3, S, t_ter

    open(9, file = 'velB_out.dat', status = 'unknown')
    read(*,*) T, Dt, v01, v02, v03, A1, A2, A3 !tempos em segundo e velocidade em m/s
    write(9,*) 0, v01
    
 
    S = 0.0d0
    t_ter = 0.0d0

    do i = 1, int(T/Dt)
      
      S = S + v1*Dt
      
      if ((v1 == v01 + (P/(m*v01))*Dt - ((C*d*A1*(v01**2))/m)*Dt) .and. (t_ter == 0)) then
        t_ter = (i-1)*Dt
      end if

      v1 = v01 + (P/(m*v01))*Dt - ((C*d*A1*(v01**2))/m)*Dt
      v01 = v1

      v2 = v02 + (P/(m*v02))*Dt - ((C*d*A2*(v02**2))/m)*Dt 
      v02 = v2

      v3 = v03 + (P/(m*v03))*Dt - ((C*d*A3*(v03**2))/m)*Dt
      v03 = v3

      ! v2 e v3 são as velocidades para as outras duas áreas diferentes, criei-as apenas para armazená-la e escreve-las
      !coluna a coluna no velB_out.dat pra depois poder plotar no gnuplot
      !as perguntas que devem ser respondidas são levando em conta v1 e A1

      write(9,*) i*Dt, v1, v2, v3
    end do
    

    close(9)

    print*, '1 - O ciclista se curva para que a área A diminua, diminuindo, portanto, o arraste do ar,', &
    ' uma vez que a força de arraste do ar é proporcional a A. Os ciclistas costumam andar em grupo para aproveitarem', &
    'um o "vácuo" do outro, ou seja, para que o ciclista da frente sirva como um agente que vai diminuir o ', &
    'arraste do ar no ciclista de trás e é por conta disso que é mais vantajoso um ciclista colar-se atrás de ',&
    'outro, para aproveitar o "vácuo" gerado pelo ciclista da frente e, assim, diminuir o arraste do ar sobre ele mesmo.' 
    print*, '2 - o espaço total percorido pelo ciclista é: ', S
    print*, '3 - a velocidade final do ciclista é: ', v1
    print*, '4 - a velocidade terminal é atingida em t =', t_ter
    print*, '5 - a velocidade média do ciclista é:', S/T

end program
    