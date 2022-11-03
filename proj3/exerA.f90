program exerA
    IMPLICIT NONE
    real(8), parameter :: P = 400.0d0, m = 80.0d0
    integer :: i
    real(8) :: T, Dt, v0, v1

    open(8, file = 'velA_out.dat', status = 'unknown')
    read(*,*) T, Dt, v0 !tempos em segundo e velocidade em m/s
    write(8,*) 0, v0

    do i = 1, int(T/Dt)
      v1 = v0 + (P/(m*v0))*Dt
      v0 = v1

      write(8,*) i*Dt, v1

    end do
    
    close(8)
end program