program exer1
    IMPLICIT NONE   
    integer :: N_0, t, N_t, N_d, i
    real(8) :: dt, lambda

    open(12, file = 'decai2_out', status = 'unknown')
    read(*,*) N_0
    read(*,*) dt
    read(*,*) lambda
    
    N_d = 0 !número de átomos decaídos
    N_t = N_0

    write(12,*) 0.0d0, N_t

    do t = 1, int(10/dt)
      do i = 1, N_t
        if (rand() < lambda*dt) then
          N_d = N_d + 1
        end if
      end do
      N_t = N_t - N_d
      N_d = 0
      WRITE(12,*) t*dt, N_t
    end do
    close(12)
end program
      


